# Get a long format data frame with the problem terms in a single column

Get a long format data frame with the problem terms in a single column

## Usage

``` r
pivot_terms_longer(df)
```

## Arguments

- df:

  A data frame containing the problem terms with columns for
  `premise_1_rt`, `premise_2_rt`, `premise_3_rt`, and `conclusion_rt`
  along with other relevant columns like `group`, `category`, and
  `problem`.

## Value

A data frame in long format with columns with the problem term a an
integer in a `term` column and as a factor in a `term_name` column.

## Examples

``` r
get_clean_data()$df_expe |>
  filter_trials_on_rt() |>
  pivot_terms_longer() |>
  dplyr::select(id, problem, category, term, term_name, rt)
#> # A tibble: 7,616 × 6
#>    id                      problem category  term term_name     rt
#>    <fct>                   <fct>   <fct>    <dbl> <fct>      <dbl>
#>  1 acdn247721443631359lzxb 25      Control      1 Premise 1   7.41
#>  2 acdn247721443631359lzxb 25      Control      2 Premise 2   3.15
#>  3 acdn247721443631359lzxb 25      Control      3 Premise 3   4.48
#>  4 acdn247721443631359lzxb 25      Control      4 Conclusion  3.89
#>  5 acdn247721443631359lzxb 1       Visual       1 Premise 1   5.77
#>  6 acdn247721443631359lzxb 1       Visual       2 Premise 2   1.56
#>  7 acdn247721443631359lzxb 1       Visual       3 Premise 3   9.68
#>  8 acdn247721443631359lzxb 1       Visual       4 Conclusion  4.76
#>  9 acdn247721443631359lzxb 6       Visual       1 Premise 1   5.60
#> 10 acdn247721443631359lzxb 6       Visual       2 Premise 2   1.65
#> # ℹ 7,606 more rows
```
