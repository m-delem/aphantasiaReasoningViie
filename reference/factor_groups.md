# Convert the `group` column to a factor with the desired VVIQ classification

Convert the `group` column to a factor with the desired VVIQ
classification

## Usage

``` r
factor_groups(df, n_groups = 2, contrast_base = NULL, ...)
```

## Arguments

- df:

  A data frame containing a `group` column with VVIQ groups among
  "Aphantasia", "Hypophantasia", "Typical" and "Hyperphantasia".

- n_groups:

  An integer indicating the number of groups to create, which reflect
  specific classifications of VVIQ scores: two groups mean grouping
  Aphants and Hypophants on one end and Typical and Hyperphants on the
  other, three groups mean separating Aphants, Hypophants and Typical
  but removing Hyperphants (that have been shown to have distinct
  characteristics), and four groups mean separating and keeping all four
  groups. Default is 2.

- contrast_base:

  Optional. An integer indicating the base level for the contrasts. By
  default, the base level is set to the Typical group for all
  classifications. This argument allows you to set a different base
  level (i.e., a different group to compare against for planned
  comparisons).

- ...:

  Additional arguments passed to
  [`add_factor_contrasts()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_factor_contrasts.md).

## Value

A data frame with the `group` column converted to a factor with
contrasts added, and the levels set according to the number of groups.

## Examples

``` r
df <- factor_groups(survey_data, n_groups = 2)
contrasts(df$group)
#>            _aphantasia
#> Aphantasia           1
#> Typical              0

df <- factor_groups(survey_data, n_groups = 3)
contrasts(df$group)
#>               _aphantasia _hypophantasia
#> Aphantasia              1              0
#> Hypophantasia           0              1
#> Typical                 0              0

df <- factor_groups(survey_data, n_groups = 4)
contrasts(df$group)
#>                _aphantasia _hypophantasia _hyperphantasia
#> Aphantasia               1              0               0
#> Hypophantasia            0              1               0
#> Typical                  0              0               0
#> Hyperphantasia           0              0               1

# Choosing Aphantasia as the reference level for the contrasts
# (i.e., for planned comparisons)
df <- factor_groups(survey_data, n_groups = 4, contrast_base = 1)
contrasts(df$group)
#>                _hypophantasia _typical _hyperphantasia
#> Aphantasia                  0        0               0
#> Hypophantasia               1        0               0
#> Typical                     0        1               0
#> Hyperphantasia              0        0               1
```
