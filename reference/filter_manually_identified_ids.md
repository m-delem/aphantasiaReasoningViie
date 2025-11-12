# Filter manually identified participant based on various criteria

Filter manually identified participant based on various criteria

## Usage

``` r
filter_manually_identified_ids(
  df,
  exclude_no_vviq = TRUE,
  exclude_no_osivq = TRUE,
  exclude_no_raven = TRUE,
  exclude_cheated = TRUE,
  exclude_distracted = TRUE,
  exclude_treatment = FALSE,
  exclude_adhd = FALSE,
  exclude_asd = FALSE,
  exclude_dyslexia = FALSE,
  exclude_other = FALSE,
  verbose = TRUE
)
```

## Arguments

- df:

  A data frame containing participant information.

- exclude_no_vviq:

  Logical, whether to exclude participants without VVIQ.

- exclude_no_osivq:

  Logical, whether to exclude participants without OSIVQ.

- exclude_no_raven:

  Logical, whether to exclude participants without Raven.

- exclude_cheated:

  Logical, whether to exclude participants who have cheated (based on
  self-report).

- exclude_distracted:

  Logical, whether to exclude participants who have been distracted
  (based on self-report).

- exclude_treatment:

  Logical, whether to exclude participants who have a treatment for a
  neurological or psychiatric disorder.

- exclude_adhd:

  Logical, whether to exclude participants who have ADHD.

- exclude_asd:

  Logical, whether to exclude participants who have ASD.

- exclude_dyslexia:

  Logical, whether to exclude participants who have dyslexia.

- exclude_other:

  Logical, whether to exclude participants who have other neurological
  troubles.

- verbose:

  Logical, whether to print the number of participants excluded based on
  the criteria.

## Value

A filtered data frame with participants who meet the specified criteria.

## Examples

``` r
df <- filter_manually_identified_ids(survey_data)
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

# We might want to try being less strict and keeping people who did not
# complete the OSIVQ or Raven, e.g. if we're only interested in the VVIQ
df <-
 filter_manually_identified_ids(
  survey_data,
  exclude_no_osivq = FALSE,
  exclude_no_raven = FALSE
 )
#> 
#> Sample size before manual examination: 137
#> Manually identified participants:
#> - N without VVIQ: 3 -> Excluded
#> - N without OSIVQ: 6 -> Included
#> - N without Raven: 2 -> Included
#> - N who cheated: 3 -> Excluded
#> - N who were distracted: 12 -> Excluded
#> - N who had treatment: 4 -> Included
#> - N with ADHD: 7 -> Included
#> - N with ASD: 5 -> Included
#> - N with dyslexia: 2 -> Included
#> - N with other neuro troubles: 2 -> Included
#> Participants to exclude: 18 (13.14%)
# Alternatively we could study a sample without any psychiatric/neurological
# troubles, etc.
```
