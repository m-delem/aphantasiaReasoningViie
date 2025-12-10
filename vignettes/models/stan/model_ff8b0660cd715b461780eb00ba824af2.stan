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
  // data for spline 7
  int nb_7;  // number of bases
  array[nb_7] int knots_7;  // number of knots
  // basis function matrices
  matrix[N, knots_7[1]] Zs_7_1;
  matrix[N, knots_7[2]] Zs_7_2;
  // data for spline 8
  int nb_8;  // number of bases
  array[nb_8] int knots_8;  // number of knots
  // basis function matrices
  matrix[N, knots_8[1]] Zs_8_1;
  matrix[N, knots_8[2]] Zs_8_2;
  // data for spline 9
  int nb_9;  // number of bases
  array[nb_9] int knots_9;  // number of knots
  // basis function matrices
  matrix[N, knots_9[1]] Zs_9_1;
  matrix[N, knots_9[2]] Zs_9_2;
  // data for spline 10
  int nb_10;  // number of bases
  array[nb_10] int knots_10;  // number of knots
  // basis function matrices
  matrix[N, knots_10[1]] Zs_10_1;
  matrix[N, knots_10[2]] Zs_10_2;
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
  // parameters for spline 7
  // standardized penalized spline coefficients
  vector[knots_7[1]] zs_7_1;
  // standardized penalized spline coefficients
  vector[knots_7[2]] zs_7_2;
  vector<lower=0>[nb_7] sds_7;  // SDs of penalized spline coefficients
  // parameters for spline 8
  // standardized penalized spline coefficients
  vector[knots_8[1]] zs_8_1;
  // standardized penalized spline coefficients
  vector[knots_8[2]] zs_8_2;
  vector<lower=0>[nb_8] sds_8;  // SDs of penalized spline coefficients
  // parameters for spline 9
  // standardized penalized spline coefficients
  vector[knots_9[1]] zs_9_1;
  // standardized penalized spline coefficients
  vector[knots_9[2]] zs_9_2;
  vector<lower=0>[nb_9] sds_9;  // SDs of penalized spline coefficients
  // parameters for spline 10
  // standardized penalized spline coefficients
  vector[knots_10[1]] zs_10_1;
  // standardized penalized spline coefficients
  vector[knots_10[2]] zs_10_2;
  vector<lower=0>[nb_10] sds_10;  // SDs of penalized spline coefficients
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
  // penalized spline coefficients
  vector[knots_7[1]] s_7_1;
  // penalized spline coefficients
  vector[knots_7[2]] s_7_2;
  // penalized spline coefficients
  vector[knots_8[1]] s_8_1;
  // penalized spline coefficients
  vector[knots_8[2]] s_8_2;
  // penalized spline coefficients
  vector[knots_9[1]] s_9_1;
  // penalized spline coefficients
  vector[knots_9[2]] s_9_2;
  // penalized spline coefficients
  vector[knots_10[1]] s_10_1;
  // penalized spline coefficients
  vector[knots_10[2]] s_10_2;
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
  // compute penalized spline coefficients
  s_7_1 = sds_7[1] * zs_7_1;
  // compute penalized spline coefficients
  s_7_2 = sds_7[2] * zs_7_2;
  // compute penalized spline coefficients
  s_8_1 = sds_8[1] * zs_8_1;
  // compute penalized spline coefficients
  s_8_2 = sds_8[2] * zs_8_2;
  // compute penalized spline coefficients
  s_9_1 = sds_9[1] * zs_9_1;
  // compute penalized spline coefficients
  s_9_2 = sds_9[2] * zs_9_2;
  // compute penalized spline coefficients
  s_10_1 = sds_10[1] * zs_10_1;
  // compute penalized spline coefficients
  s_10_2 = sds_10[2] * zs_10_2;
  lprior += normal_lpdf(b | 0, 1);
  lprior += student_t_lpdf(Intercept | 3, 1.5, 2.5);
  lprior += normal_lpdf(bs | 0, 1);
  lprior += exponential_lpdf(sds_1 | 1);
  lprior += exponential_lpdf(sds_2 | 1);
  lprior += exponential_lpdf(sds_3 | 1);
  lprior += exponential_lpdf(sds_4 | 1);
  lprior += exponential_lpdf(sds_5 | 1);
  lprior += exponential_lpdf(sds_6 | 1);
  lprior += exponential_lpdf(sds_7 | 1);
  lprior += exponential_lpdf(sds_8 | 1);
  lprior += exponential_lpdf(sds_9 | 1);
  lprior += exponential_lpdf(sds_10 | 1);
  lprior += exponential_lpdf(sigma | 1);
  lprior += uniform_lpdf(ndt | 0, min_Y)
    - 1 * log_diff_exp(uniform_lcdf(min_Y | 0, min_Y), uniform_lcdf(0 | 0, min_Y));
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + Xc * b + Xs * bs + Zs_1_1 * s_1_1 + Zs_2_1 * s_2_1 + Zs_3_1 * s_3_1 + Zs_4_1 * s_4_1 + Zs_5_1 * s_5_1 + Zs_6_1 * s_6_1 + Zs_7_1 * s_7_1 + Zs_7_2 * s_7_2 + Zs_8_1 * s_8_1 + Zs_8_2 * s_8_2 + Zs_9_1 * s_9_1 + Zs_9_2 * s_9_2 + Zs_10_1 * s_10_1 + Zs_10_2 * s_10_2;
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
  target += std_normal_lpdf(zs_7_1);
  target += std_normal_lpdf(zs_7_2);
  target += std_normal_lpdf(zs_8_1);
  target += std_normal_lpdf(zs_8_2);
  target += std_normal_lpdf(zs_9_1);
  target += std_normal_lpdf(zs_9_2);
  target += std_normal_lpdf(zs_10_1);
  target += std_normal_lpdf(zs_10_2);
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
}

