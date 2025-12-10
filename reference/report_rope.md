# Report the ROPE analysis for marginal effects

Report the ROPE analysis for marginal effects

## Usage

``` r
report_rope(marg_effects, ..., digits = 3)
```

## Arguments

- marg_effects:

  A marginaleffects object obtained with
  [`marginaleffects::avg_comparisons()`](https://marginaleffects.com/man/r/comparisons.html)
  containing the contrasts to analyse.

- ...:

  Grouping variables for summarising the ROPE results.

- digits:

  Number of decimal places to round the results. Default is 3.

## Value

A data frame summarising the ROPE analysis with the estimates, 95% CIs,
and proportions of draws within, below, and above the ROPE.
