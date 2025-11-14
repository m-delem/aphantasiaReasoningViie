# Convert strategy columns to factors with optionally ordered levels

Convert strategy columns to factors with optionally ordered levels

## Usage

``` r
factor_strategies(df, ordered = TRUE)
```

## Arguments

- df:

  A data frame containing strategy columns to be converted to factors.

- ordered:

  A logical indicating whether the factor levels should be ordered.

## Value

A data frame with strategy columns converted to factors, with levels
ranging from 1 to 5 for ordered factors, or as character strings for
unordered factors.

## Examples

``` r
# The filtering function removes participants with bad or incomplete data,
# notably those who did not complete the strategy questionnaire.
df <- survey_data |>
  filter_manually_identified_ids() |>
  factor_strategies(ordered = TRUE)
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
levels(df$visual_strat)
#> [1] "no_use"            "secondary_strat"   "as_much_as_others"
#> [4] "mainly_this_strat" "only_this_strat"  
contrasts(df$visual_strat)
#>                 .L         .Q            .C         ^4
#> [1,] -6.324555e-01  0.5345225 -3.162278e-01  0.1195229
#> [2,] -3.162278e-01 -0.2672612  6.324555e-01 -0.4780914
#> [3,] -3.288380e-17 -0.5345225  9.637305e-17  0.7171372
#> [4,]  3.162278e-01 -0.2672612 -6.324555e-01 -0.4780914
#> [5,]  6.324555e-01  0.5345225  3.162278e-01  0.1195229

df <- survey_data |>
  filter_manually_identified_ids() |>
  factor_strategies(ordered = FALSE)
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
levels(df$visual_strat)
#> [1] "no_use"            "secondary_strat"   "as_much_as_others"
#> [4] "mainly_this_strat" "only_this_strat"  
contrasts(df$visual_strat)
#>                   secondary_strat as_much_as_others mainly_this_strat
#> no_use                          0                 0                 0
#> secondary_strat                 1                 0                 0
#> as_much_as_others               0                 1                 0
#> mainly_this_strat               0                 0                 1
#> only_this_strat                 0                 0                 0
#>                   only_this_strat
#> no_use                          0
#> secondary_strat                 0
#> as_much_as_others               0
#> mainly_this_strat               0
#> only_this_strat                 1
```
