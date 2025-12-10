# Plot proportions of strategy use for groups as barplots

Plot proportions of strategy use for groups as barplots

## Usage

``` r
plot_strategies_barplot(df_long, grouping = "group_4", title = NULL, ...)
```

## Arguments

- df_long:

  A data frame containing the strategies data in a long format with
  columns for grouping, strategy, and score.

- grouping:

  A variable to group the data by, typically group or cluster. Can be
  unquoted.

- title:

  Optional title for the plot.

- ...:

  Additional arguments passed to the
  [`theme_pdf()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/theme_pdf.md)
  function for customizing the plot theme.

## Value

A ggplot2 object showing the proportions of strategy use for each group
as a barplot, with strategies as facets.

## Examples

``` r
df_long <-
  get_clean_data()$df_survey |>
  pivot_strategies_longer()

plot_strategies_barplot(df_long, grouping = group_4, base_size = 12)
```
