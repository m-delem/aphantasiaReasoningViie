# Filter outlier trials based on mean response time per participant

Filter outlier trials based on mean response time per participant

## Usage

``` r
filter_trials_on_rt(
  df,
  mult = 2,
  min_rt = 0.6,
  max_rt = 30,
  verbose = FALSE,
  ...
)
```

## Arguments

- df:

  A data frame containing trial data with columns for id, Group,
  trial_number, category, premise_1_rt, premise_2_rt, premise_3_rt,
  conclusion_rt and accuracy.

- mult:

  A multiplier for the standard deviation to set the threshold for
  outlier detection. Default is 2.

- min_rt:

  Minimum response time in seconds for trials to be considered valid.
  Default is 0.6 seconds.

- max_rt:

  Maximum response time in seconds for trials to be considered valid.
  Default is 30 seconds.

- verbose:

  Logical. Whether to print a summary of the filtering process. Default
  is FALSE.

- ...:

  Additional arguments passed to the function. Unused.

## Value

A filtered data frame containing only valid trials, with outliers
removed based on the specified thresholds.

## Examples

``` r
df_rt <-
  get_clean_data()$df_expe |>
  filter_trials_on_rt()
```
