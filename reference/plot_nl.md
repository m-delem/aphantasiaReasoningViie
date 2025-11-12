# Plot model predictions from the non-linear GAMM results

Plot model predictions from the non-linear GAMM results

## Usage

``` r
plot_nl(df, title = NULL, ...)
```

## Arguments

- df:

  A data frame containing the model predictions with columns:

  - `term`: the trial term as an integer from 1 to 4

  - `Predicted`: the predicted response time

  - `CI_low`: the lower bound of the confidence interval

  - `CI_high`: the upper bound of the confidence interval

  - `category`: the problem category (e.g., "Visual", "Control",
    "Spatial")

  - `group`: the participant group Results from model fits are saved
    natively in the package in the `nl_predictions` object.

- title:

  An optional title for the plot. Default is NULL.

- ...:

  Additional arguments passed to the
  [`theme_pdf()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/theme_pdf.md)
  function for customising the plot theme.

## Value

A ggplot2 object showing the model predictions with confidence
intervals, faceted by participant group and coloured by problem
category.

## Examples

``` r
plot_nl(nl_predictions$vviq_2, base_size = 12)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'
#> Warning: span too small.   fewer data values than degrees of freedom.
#> Warning: pseudoinverse used at 0.985
#> Warning: neighborhood radius 2.015
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 4.0602
#> Warning: span too small.   fewer data values than degrees of freedom.
#> Warning: pseudoinverse used at 0.985
#> Warning: neighborhood radius 2.015
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 4.0602
#> Warning: span too small.   fewer data values than degrees of freedom.
#> Warning: pseudoinverse used at 0.985
#> Warning: neighborhood radius 2.015
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 4.0602
#> Warning: span too small.   fewer data values than degrees of freedom.
#> Warning: pseudoinverse used at 0.985
#> Warning: neighborhood radius 2.015
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 4.0602
#> Warning: span too small.   fewer data values than degrees of freedom.
#> Warning: pseudoinverse used at 0.985
#> Warning: neighborhood radius 2.015
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 4.0602
#> Warning: span too small.   fewer data values than degrees of freedom.
#> Warning: pseudoinverse used at 0.985
#> Warning: neighborhood radius 2.015
#> Warning: reciprocal condition number  0
#> Warning: There are other near singularities as well. 4.0602
#> Warning: `position_dodge()` requires non-overlapping x intervals.
#> Warning: `position_dodge()` requires non-overlapping x intervals.
```
