// generated with brms 2.22.0
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  // data for splines
  int Ks;  // number of linear effects
  matrix[N, Ks] Xs;  // design matrix for the linear effects
  // data for spline 1
  int nb_1;  // number of bases
  array[nb_1] int knots_1;  // number of knots
  // basis function matrices
  matrix[N, knots_1[1]] Zs_1_1;
  // data for spline 2
  int nb_2;  // number of bases
  array[nb_2] int knots_2;  // number of knots
  // basis function matrices
  matrix[N, knots_2[1]] Zs_2_1;
  // data for spline 3
  int nb_3;  // number of bases
  array[nb_3] int knots_3;  // number of knots
  // basis function matrices
  matrix[N, knots_3[1]] Zs_3_1;
  // data for spline 4
  int nb_4;  // number of bases
  array[nb_4] int knots_4;  // number of knots
  // basis function matrices
  matrix[N, knots_4[1]] Zs_4_1;
  // data for spline 5
  int nb_5;  // number of bases
  array[nb_5] int knots_5;  // number of knots
  // basis function matrices
  matrix[N, knots_5[1]] Zs_5_1;
  // data for spline 6
  int nb_6;  // number of bases
  array[nb_6] int knots_6;  // number of knots
  // basis function matrices
  matrix[N, knots_6[1]] Zs_6_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  real min_Y = min(Y);
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  vector[Ks] bs;  // unpenalized spline coefficients
  // parameters for spline 1
  // standardized penalized spline coefficients
  vector[knots_1[1]] zs_1_1;
  vector<lower=0>[nb_1] sds_1;  // SDs of penalized spline coefficients
  // parameters for spline 2
  // standardized penalized spline coefficients
  vector[knots_2[1]] zs_2_1;
  vector<lower=0>[nb_2] sds_2;  // SDs of penalized spline coefficients
  // parameters for spline 3
  // standardized penalized spline coefficients
  vector[knots_3[1]] zs_3_1;
  vector<lower=0>[nb_3] sds_3;  // SDs of penalized spline coefficients
  // parameters for spline 4
  // standardized penalized spline coefficients
  vector[knots_4[1]] zs_4_1;
  vector<lower=0>[nb_4] sds_4;  // SDs of penalized spline coefficients
  // parameters for spline 5
  // standardized penalized spline coefficients
  vector[knots_5[1]] zs_5_1;
  vector<lower=0>[nb_5] sds_5;  // SDs of penalized spline coefficients
  // parameters for spline 6
  // standardized penalized spline coefficients
  vector[knots_6[1]] zs_6_1;
  vector<lower=0>[nb_6] sds_6;  // SDs of penalized spline coefficients
  real<lower=0> sigma;  // dispersion parameter
  real<lower=0,upper=min_Y> ndt;  // non-decision time parameter
}
transformed parameters {
  // penalized spline coefficients
  vector[knots_1[1]] s_1_1;
  // penalized spline coefficients
  vector[knots_2[1]] s_2_1;
  // penalized spline coefficients
  vector[knots_3[1]] s_3_1;
  // penalized spline coefficients
  vector[knots_4[1]] s_4_1;
  // penalized spline coefficients
  vector[knots_5[1]] s_5_1;
  // penalized spline coefficients
  vector[knots_6[1]] s_6_1;
  real lprior = 0;  // prior contributions to the log posterior
  // compute penalized spline coefficients
  s_1_1 = sds_1[1] * zs_1_1;
  // compute penalized spline coefficients
  s_2_1 = sds_2[1] * zs_2_1;
  // compute penalized spline coefficients
  s_3_1 = sds_3[1] * zs_3_1;
  // compute penalized spline coefficients
  s_4_1 = sds_4[1] * zs_4_1;
  // compute penalized spline coefficients
  s_5_1 = sds_5[1] * zs_5_1;
  // compute penalized spline coefficients
  s_6_1 = sds_6[1] * zs_6_1;
  lprior += normal_lpdf(b | 0, 5);
  lprior += student_t_lpdf(Intercept | 3, 1.5, 2.5);
  lprior += normal_lpdf(bs | 0, 5);
  lprior += exponential_lpdf(sds_1 | 10);
  lprior += exponential_lpdf(sds_2 | 10);
  lprior += exponential_lpdf(sds_3 | 10);
  lprior += exponential_lpdf(sds_4 | 10);
  lprior += exponential_lpdf(sds_5 | 10);
  lprior += exponential_lpdf(sds_6 | 10);
  lprior += exponential_lpdf(sigma | 10);
  lprior += uniform_lpdf(ndt | 0, min_Y)
    - 1 * log_diff_exp(uniform_lcdf(min_Y | 0, min_Y), uniform_lcdf(0 | 0, min_Y));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + Xc * b + Xs * bs + Zs_1_1 * s_1_1 + Zs_2_1 * s_2_1 + Zs_3_1 * s_3_1 + Zs_4_1 * s_4_1 + Zs_5_1 * s_5_1 + Zs_6_1 * s_6_1;
    target += lognormal_lpdf(Y - ndt | mu, sigma);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(zs_1_1);
  target += std_normal_lpdf(zs_2_1);
  target += std_normal_lpdf(zs_3_1);
  target += std_normal_lpdf(zs_4_1);
  target += std_normal_lpdf(zs_5_1);
  target += std_normal_lpdf(zs_6_1);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}

