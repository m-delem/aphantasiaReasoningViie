# Fit a cumulative link model (CLM) using the ordinal package

Fit a cumulative link model (CLM) using the ordinal package

## Usage

``` r
fit_clm(formula, data, link = "probit")
```

## Arguments

- formula:

  A formula specifying the model to fit.

- data:

  A data frame containing the data to fit the model.

- link:

  A string specifying the link function to use. Default is "probit".

## Value

A fitted clm object from the ordinal package.

## Examples

``` r
df_strats <- get_clean_data()$df_survey |> pivot_strategies_longer()

model <- fit_clm(score ~ group_2 * strategy, df_strats)
report_contrast(model, ~ group_2 | strategy)
#>               Contrast     Strategy Difference            CI p.value
#> 1 Aphantasia - Typical       Visual     -1.689 [-2.18, -1.2]   0.000
#> 2 Aphantasia - Typical      Spatial     -0.397 [-0.82, 0.03]   0.068
#> 3 Aphantasia - Typical       Verbal      0.267 [-0.14, 0.68]   0.203
#> 4 Aphantasia - Typical     Semantic      0.229 [-0.28, 0.74]   0.379
#> 5 Aphantasia - Typical Sensorimotor     -0.137 [-0.57, 0.29]   0.531
```
