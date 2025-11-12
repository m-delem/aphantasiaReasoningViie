# Filter (or mark) participants with suspicious median RTs

These two functions identify participants with suspiciously fast RTs
overall using their median RTs and comparing them to the sample mean of
median RTs.

`filter_suspicious_rt_ids` removes these participants from the data
frame, while `mark_suspicious_rt_ids` adds a column to flag them without
removing their data (e.g., for manual examination after identifying
them).

## Usage

``` r
filter_suspicious_rt_ids(df, sd_mult = 2, verbose = TRUE)

mark_suspicious_rt_ids(df, sd_mult = 2, verbose = TRUE)
```

## Arguments

- df:

  A data frame containing participants' total response times for each
  trial in a `rt_total` column.

- sd_mult:

  A numeric value indicating how many standard deviations to use for
  identifying suspicious median RTs. The default is 2, which means that
  median RTs that are more than 2 standard deviations away from the mean
  will be considered suspicious.

- verbose:

  Logical, whether to print the number of participants excluded or
  marked based on median RTs.

## Value

A data frame with suspicious participants removed (or marked).

## Examples

``` r
df <- mark_suspicious_rt_ids(experiment_data)
#> 
#> Marked 0 suspicious median RTs (0%) out of 137 IDs.
df |>
  dplyr::filter(is_suspicious == TRUE) |>
  dplyr::select(id, median_rt) |>
  dplyr::distinct()
#> # A tibble: 0 × 2
#> # ℹ 2 variables: id <chr>, median_rt <dbl>

df <- filter_suspicious_rt_ids(experiment_data)
#> 
#> Sample size before median RTs analysis: 137
#> Participants with median RTs outside 2 SDs: 4 (2.92%)
length(unique(df$id))
#> [1] 133
```
