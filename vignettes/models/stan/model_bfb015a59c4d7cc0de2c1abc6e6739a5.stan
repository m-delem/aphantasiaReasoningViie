// generated with brms 2.22.0
functions {
  /* cumulative-probit log-PDF for a single response
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: ordinal thresholds
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cumulative_probit_lpmf(int y, real mu, real disc, vector thres) {
     int nthres = num_elements(thres);
     real p;
     if (y == 1) {
       p = Phi(disc * (thres[1] - mu));
     } else if (y == nthres + 1) {
       p = 1 - Phi(disc * (thres[nthres] - mu));
     } else {
       p = Phi(disc * (thres[y] - mu)) -
           Phi(disc * (thres[y - 1] - mu));
     }
     return log(p);
   }
  /* cumulative-probit log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cumulative_probit_merged_lpmf(int y, real mu, real disc, vector thres, array[] int j) {
     return cumulative_probit_lpmf(y | mu, disc, thres[j[1]:j[2]]);
   }
  /* ordered-probit log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real ordered_probit_merged_lpmf(int y, real mu, vector thres, array[] int j) {
     return ordered_probit_lpmf(y | mu, thres[j[1]:j[2]]);
   }

}
data {
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int<lower=2> nthres;  // number of thresholds
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X
  vector[Kc] means_X;  // column means of X before centering
  for (i in 1:K) {
    means_X[i] = mean(X[, i]);
    Xc[, i] = X[, i] - means_X[i];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  ordered[nthres] Intercept;  // temporary thresholds for centered predictors
}
transformed parameters {
  real disc = 1;  // discrimination parameters
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Xc * b;
    for (n in 1:N) {
      target += ordered_probit_lpmf(Y[n] | mu[n], Intercept);
    }
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // compute actual thresholds
  vector[nthres] b_Intercept = Intercept + dot_product(means_X, b);
}

