# Filter participants with below random accuracy

Filter participants with below random accuracy

## Usage

``` r
filter_random_accuracy_ids(df, verbose = TRUE)
```

## Arguments

- df:

  A data frame containing participant responses and accuracy.

- verbose:

  Logical, whether to print the number of participants excluded based on
  accuracy.

## Value

A filtered data frame with participants who have an accuracy above 50%
across all trials.

## Examples

``` r
df <- filter_random_accuracy_ids(experiment_data)
#> 
#> Sample size before accuracy analysis: 137
#> Participants below random accuracy (<= 50%): 8 (5.84%)
```
