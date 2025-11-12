# Plot the results of a power analysis by simulation

This function plots the results of a power analysis by simulation. It
takes a data frame similar to those output by the
[`run_power_analysis()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/run_power_analysis.md)
function.

## Usage

``` r
plot_power(power_results, threshold = 0.05, ...)
```

## Arguments

- power_results:

  A data frame with the results of the power analysis by simulation.

- threshold:

  The significance threshold to use for calculating power. Default is
  0.05.

- ...:

  Additional arguments passed to the
  [`theme_pdf()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/theme_pdf.md)
  function for customizing the plot theme.

## Value

A ggplot2 object with the power curves.

## Examples

``` r
plot_power(power_results) # power_results is natively in the package
```
