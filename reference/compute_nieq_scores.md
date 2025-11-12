# Compute NIEQ scores by combining the frequency and proportion items of each subscale

Compute NIEQ scores by combining the frequency and proportion items of
each subscale

## Usage

``` r
compute_nieq_scores(df)
```

## Arguments

- df:

  A data frame containing the NIEQ data with columns for frequency and
  proportion of each subscale.

## Value

A data frame with new columns for NIEQ scores, calculated as the mean of
the frequency and proportion items for each subscale.

## Examples

``` r
df <- compute_nieq_scores(survey_data)
df |>
  dplyr::filter(nieq_is_complete == TRUE) |>
  dplyr::select("id", tidyselect::contains("nieq_")) |>
  head()
#> # A tibble: 6 × 17
#>   id          nieq_is_complete nieq_voice nieq_visual nieq_emotions nieq_sensory
#>   <chr>       <lgl>                 <dbl>       <dbl>         <dbl>        <dbl>
#> 1 aacu640913… TRUE                   95           0            85           55  
#> 2 akdu763174… TRUE                   74.5         0            15.5         25  
#> 3 azcj317771… TRUE                   90          20.5          90           31  
#> 4 bkdq973389… TRUE                   74           2            74.5         50.5
#> 5 cbkt251247… TRUE                   80          60.5          41           50.5
#> 6 ckmw156723… TRUE                  100           0            69.5         54.5
#> # ℹ 11 more variables: nieq_abstract <dbl>, nieq_freq_inner_voice <int>,
#> #   nieq_freq_mental_imagery <int>, nieq_freq_emotions <int>,
#> #   nieq_freq_sensory_focus <int>, nieq_freq_unsymbolised <int>,
#> #   nieq_prop_inner_voice <int>, nieq_prop_mental_imagery <int>,
#> #   nieq_prop_emotions <int>, nieq_prop_sensory_focus <int>,
#> #   nieq_prop_unsymbolised <int>
```
