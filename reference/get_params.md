# Get the fixed parameters of a model in a clean format

Get the fixed parameters of a model in a clean format

## Usage

``` r
get_params(model, ...)
```

## Arguments

- model:

  A fitted model object.

- ...:

  Additional arguments passed to
  [`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html).

## Value

A formatted data frame with the fixed parameters of the model.

## Examples

``` r
df_expe <- get_clean_data()$df_expe

if (require("glmmTMB", quietly = TRUE)) {
 model <- glmmTMB::glmmTMB(
  formula = accuracy ~ group_2 * category + (1 | id),
  data = df_expe,
  family = binomial(link = "logit"),
  prior = set_ranef_prior(65)
 )

 get_params(model)
}
#>                               Parameter Log-Odds       95% CI      p
#> 1                           (Intercept)     1.30   1.03, 1.56 < .001
#> 2                    group 2 aphantasia     0.02  -0.37, 0.42 0.919 
#> 3                      category control     0.47   0.15, 0.79 0.004 
#> 4                      category spatial     0.27  -0.04, 0.58 0.083 
#> 5 group 2 aphantasia × category control    -0.47 -0.93, -0.01 0.044 
#> 6 group 2 aphantasia × category spatial    -0.27  -0.73, 0.18 0.236 
```
