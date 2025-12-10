# Simulate accuracy data for the factorial design

This function simulates accuracy data for the factorial design of the
experiment.

It was designed for potential power analyses on accuracy data, but it
ended up being unused due to a lack of previous data to base the
analyses on. The power analyses were conducted on RT data instead.

It creates a data frame with the following columns:

- `id`: A unique identifier for each subject.

- `Group`: The group to which the subject belongs (aphantasia or
  typical).

- `category`: The category of the trial (visual, spatial, or control).

- `trial`: The trial number (1 to 9).

- `accuracy`: The accuracy of the subject's response (1 for correct, 0
  for incorrect). Inspired by [Lisa
  DeBruine](https://debruine.github.io/lmem_sim/articles/appendix3a_binomial.html).

## Usage

``` r
simulate_acc_data(
  n_subj_per_group,
  beta_0 = 1.36,
  tau_0 = 0.05,
  tau_vis = 0.01,
  tau_spa = 0.01,
  beta_aph = 0,
  beta_vis = 0,
  beta_spa = 0,
  beta_aph_vis = 0,
  beta_aph_spa = 0,
  seed = NULL,
  ...
)
```

## Arguments

- n_subj_per_group:

  The number of subjects per group.

- beta_0:

  The intercept for the model.

- tau_0:

  The standard deviation of the random intercept for each subject.

- tau_vis:

  The standard deviation of the random slope for the visual category.

- tau_spa:

  The standard deviation of the random slope for the spatial category.

- beta_aph:

  The fixed effect of the aphantasia group.

- beta_vis:

  The fixed effect of the visual category.

- beta_spa:

  The fixed effect of the spatial category.

- beta_aph_vis:

  The interaction between the aphantasia group and the visual category.

- beta_aph_spa:

  The interaction between the aphantasia group and the spatial category.

- seed:

  The seed for random number generation. If NULL, the seed is not set.

- ...:

  Additional arguments passed to the function. Unused.

## Value

A data frame with the simulated accuracy data.

## Examples

``` r
df <- simulate_acc_data(100)
head(df)
#> # A tibble: 6 × 17
#>   id    group_4    category accuracy trial group         tau_0  tau_vis tau_spa
#>   <chr> <fct>      <fct>       <int> <int> <fct>         <dbl>    <dbl>   <dbl>
#> 1 id001 Aphantasia Visual          1     1 Aphantasia -0.00823 -0.00434 0.00707
#> 2 id001 Aphantasia Visual          1     2 Aphantasia -0.00823 -0.00434 0.00707
#> 3 id001 Aphantasia Visual          1     3 Aphantasia -0.00823 -0.00434 0.00707
#> 4 id001 Aphantasia Visual          1     4 Aphantasia -0.00823 -0.00434 0.00707
#> 5 id001 Aphantasia Visual          1     5 Aphantasia -0.00823 -0.00434 0.00707
#> 6 id001 Aphantasia Visual          1     6 Aphantasia -0.00823 -0.00434 0.00707
#> # ℹ 8 more variables: aphantasia <dbl>, visual <dbl>, spatial <dbl>, Y <dbl>,
#> #   pr <dbl>, n_correct <int>, n_trials <int>, mean_acc <dbl>

df |>
  dplyr::group_by(group, category) |>
  dplyr::reframe(
    mean_acc = mean(accuracy),
    median_acc = median(accuracy),
    sd_acc = sd(accuracy)
    )
#> # A tibble: 6 × 5
#>   group      category mean_acc median_acc sd_acc
#>   <fct>      <fct>       <dbl>      <dbl>  <dbl>
#> 1 Aphantasia Control     0.796          1  0.404
#> 2 Aphantasia Spatial     0.782          1  0.413
#> 3 Aphantasia Visual      0.784          1  0.411
#> 4 Typical    Control     0.772          1  0.420
#> 5 Typical    Spatial     0.791          1  0.407
#> 6 Typical    Visual      0.783          1  0.412
```
