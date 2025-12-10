# Fit a Bayesian model using the brms package with default settings

Fit a Bayesian model using the brms package with default settings

## Usage

``` r
fit_brms_model(
  ...,
  iterations = 24000,
  warmup = 2000,
  refresh = 500,
  backend = "cmdstanr",
  file_refit = "on_change",
  file_compress = "xz",
  sample_prior = FALSE,
  save_pars = NULL,
  adapt_delta = 0.95,
  seed = 667
)
```

## Arguments

- ...:

  Arguments passed to brms::brm(), such as formula, data, family,
  priors, etc.

- iterations:

  Total number of iterations. This number is divided by the number of
  cores for parallel processing. Default is 20000 (40k recommended if
  Bayes Factors are needed).

- warmup:

  Number of warmup iterations added for each chain. Default is 2000.

- refresh:

  Frequency of progress updates. Default is 500.

- backend:

  Backend to use for fitting the model. Default is "cmdstanr".

- file_refit:

  Condition for refitting the model. Default is "on_change".

- file_compress:

  Compression method for saving the model file. Default is "xz".

- sample_prior:

  Logical. If TRUE, prior samples are drawn. If "only", only prior
  samples are drawn. Default is FALSE. FALSE

- save_pars:

  Parameters to save. Default is NULL.

- adapt_delta:

  Target acceptance rate for the NUTS sampler. Default is 0.95.

- seed:

  Random seed for reproducibility. Default is 667.

## Value

A fitted brms model object.
