# Helper function to quickly describe the cleaned survey data

Helper function to quickly describe the cleaned survey data

## Usage

``` r
describe_survey_data(df, grouping = NULL)
```

## Arguments

- df:

  A data frame containing the survey data columns related to the
  questionnaires. Note that the functions computes means for the
  combined NIEQ scores, so the data frame must have gone through
  [`compute_nieq_scores()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/compute_nieq_scores.md).

## Value

A data frame summarising the survey data grouped by VVIQ group,
including sample size, gender distribution, mean scores for VVIQ, OSIVQ
sub-scales, Raven matrices and NIEQ sub-scales, along with the number of
participants who completed the NIEQ questionnaire (which was part of
another online experiment, so completely optional).

## Examples

``` r
# The filtering function removes participants with bad or incomplete data,
# notably those who did not complete certain questionnaires.
survey_data |>
 filter_manually_identified_ids() |>
 compute_nieq_scores() |>
 describe_survey_data()
#> 
#> Sample size before manual examination: 137
#> Manually identified participants:
#> - N without VVIQ: 3 -> Excluded
#> - N without OSIVQ: 6 -> Excluded
#> - N without Raven: 2 -> Excluded
#> - N who cheated: 3 -> Excluded
#> - N who were distracted: 12 -> Excluded
#> - N who had treatment: 4 -> Included
#> - N with ADHD: 7 -> Included
#> - N with ASD: 5 -> Included
#> - N with dyslexia: 2 -> Included
#> - N with other neuro troubles: 2 -> Included
#> Participants to exclude: 24 (17.52%)
#> # A tibble: 1 × 13
#>   N          Age   VVIQ  `OSIVQ-Object` `OSIVQ-Spatial` `OSIVQ-Verbal` `RSPM-18`
#>   <glue>     <chr> <chr> <chr>          <chr>           <chr>          <chr>    
#> 1 113 (87 F… 34.0… 38.8… 2.44 (1.22)    2.69 (0.8)      3.1 (0.92)     15.51 (2…
#> # ℹ 6 more variables: nieq_completed <int>, voice <chr>, visual <chr>,
#> #   emotions <chr>, sensory <chr>, abstract <chr>
```
