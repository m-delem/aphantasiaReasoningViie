# Get a long format data frame with the strategies gathered in a single column

Get a long format data frame with the strategies gathered in a single
column

## Usage

``` r
pivot_strategies_longer(df, base = 1, ...)
```

## Arguments

- df:

  A data frame containing the strategies data with columns for `id`,
  `group`, `visual_strat`, `spatial_strat`, `verbal_strat`,
  `semantic_strat`, and `sensorimotor_strat`.

- base:

  An integer indicating the base level for factor contrasts. Default is
  1 for the visual strategy.

- ...:

  Additional arguments passed to
  [`add_factor_contrasts()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_factor_contrasts.md).

## Value

A data frame in long format with columns for `id`, `group`, `strategy`,
and `score`, where `strategy` is a factor with levels for each strategy
and `score` is a factor indicating the level of strategy use.

## Examples

``` r
get_clean_data()$df_survey |>
  pivot_strategies_longer() |>
  dplyr::select(id, group, strategy, score)
#> # A tibble: 520 × 4
#>    id                      group      strategy     score            
#>    <fct>                   <fct>      <fct>        <fct>            
#>  1 acdn247721443631359lzxb Typical    Visual       no_use           
#>  2 acdn247721443631359lzxb Typical    Verbal       mainly_this_strat
#>  3 acdn247721443631359lzxb Typical    Spatial      secondary_strat  
#>  4 acdn247721443631359lzxb Typical    Semantic     no_use           
#>  5 acdn247721443631359lzxb Typical    Sensorimotor only_this_strat  
#>  6 ahos206230340719944kiwl Aphantasia Visual       no_use           
#>  7 ahos206230340719944kiwl Aphantasia Verbal       mainly_this_strat
#>  8 ahos206230340719944kiwl Aphantasia Spatial      secondary_strat  
#>  9 ahos206230340719944kiwl Aphantasia Semantic     no_use           
#> 10 ahos206230340719944kiwl Aphantasia Sensorimotor secondary_strat  
#> # ℹ 510 more rows
```
