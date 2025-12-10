# Get performance indices for a model in a clean format

Get performance indices for a model in a clean format

## Usage

``` r
get_performance(model, metrics = "common", verbose = FALSE, ...)
```

## Arguments

- model:

  A fitted model object.

- metrics:

  Can be "all", "common" or a character vector of metrics to be
  computed, see `?performance::model_performance()` for details.

- verbose:

  Toggle warnings on or off. Default is FALSE.

- ...:

  Additional arguments passed to
  [`performance::model_performance()`](https://easystats.github.io/performance/reference/model_performance.html).

## Value

A formatted data frame with the performance indices of the model.

## Examples

``` r
df_expe <- get_clean_data()$df_expe

if (require("glmmTMB", quietly = TRUE)) {
 model <- glmmTMB::glmmTMB(
  formula = accuracy ~ group_2 * category + (1 | id),
  data = df_expe,
  family = binomial(link = "logit"),
  prior = set_ranef_prior(20)
 )

 get_performance(model)
}
#>      AIC    BIC R2 (cond.) R2 (marg.)   ICC  RMSE
#> 1 2843.8 2885.4      0.108      0.009 0.100 0.391
```
