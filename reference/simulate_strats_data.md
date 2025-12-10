# Simulate strategies data for all four groups

This function simulates strategies data for the four groups of phantasia
based on the means and standard deviations provided. The data is
simulated using the
[`latent2likert::rlikert()`](https://markolalovic.github.io/latent2likert/reference/rlikert.html)
function, which generates likert-type data based on latent variables.
The function allows for customization of the means and standard
deviations for each group, as well as the option to generate data in a
format suitable for modelling with
[`faux::add_contrast()`](https://scienceverse.github.io/faux/reference/add_contrast.html).
The default means and standard deviations are based on the findings of
Reeder & Pounder (2024) on strategies used by people with different
levels of imagery.

It was designed for potential power analyses on strategies data, but it
ended up being unused due to a lack of time and previous data to build
upon. The power analyses were conducted on RT data instead.

## Usage

``` r
simulate_strats_data(
  n,
  means_aph = c(-2, 1, -1.3, -1.6, -0.7),
  means_hypo = c(-1.1, 0.1, -1.2, -1.6, -0.1),
  means_typical = c(0.1, 0.1, -1.5, -1.4, -0.3),
  means_hyper = c(-0.1, -0.2, -1.7, -1.7, -0.9),
  sd = 0.5,
  modelling_version = FALSE,
  seed = NULL,
  ...
)
```

## Arguments

- n:

  Number of participants per group.

- means_aph:

  Numeric vector of means for the Aphantasia group.

- means_hypo:

  Numeric vector of means for the Hypophantasia group.

- means_typical:

  Numeric vector of means for the Typical group.

- means_hyper:

  Numeric vector of means for the Hyperphantasia group.

- sd:

  Standard deviation for all groups (default is 0.5).

- modelling_version:

  Logical indicating whether to return the data in a format suitable for
  modelling with
  [`faux::add_contrast()`](https://scienceverse.github.io/faux/reference/add_contrast.html).
  If `TRUE`, the data will be returned with contrasts added for the
  groups and the variables will be converted to ordered factors. If
  `FALSE`, the data will be returned in its original format with the
  group names as character strings.

- seed:

  Optional seed for reproducibility of the random data generation.

- ...:

  Additional arguments passed to the function. Unused.

## Value

A tibble containing the simulated strategies data for the four groups.

## Examples

``` r
df <- simulate_strats_data(n = 100, seed = 123)
head(df)
#> # A tibble: 6 × 8
#>   id              group_4 visual_strat spatial_strat verbal_strat semantic_strat
#>   <chr>           <chr>          <int>         <int>        <int>          <int>
#> 1 subj_aphantasi… Aphant…            1             5            1              2
#> 2 subj_aphantasi… Aphant…            1             4            2              1
#> 3 subj_aphantasi… Aphant…            1             4            2              2
#> 4 subj_aphantasi… Aphant…            1             3            1              1
#> 5 subj_aphantasi… Aphant…            2             4            1              1
#> 6 subj_aphantasi… Aphant…            1             5            2              1
#> # ℹ 2 more variables: sensorimotor_strat <int>, group <fct>

df |>
 dplyr::group_by(group) |>
 dplyr::reframe(dplyr::across(tidyselect::contains("_strat"), mean))
#> # A tibble: 4 × 6
#>   group          visual_strat spatial_strat verbal_strat semantic_strat
#>   <fct>                 <dbl>         <dbl>        <dbl>          <dbl>
#> 1 Aphantasia             1.06          4.18         1.47           1.27
#> 2 Hypophantasia          1.7           3.14         1.54           1.17
#> 3 Typical                3.27          3.14         1.35           1.3 
#> 4 Hyperphantasia         2.8           2.69         1.26           1.22
#> # ℹ 1 more variable: sensorimotor_strat <dbl>
```
