# Simulate skewed RT data for the factorial design

This function simulates response time data for the factorial design of
the experiment. It was used for power analyses by simulation.

Inspired by [Chris Jungerius](https://cjungerius.github.io/powersim/).

## Usage

``` r
simulate_rt_data(
  n_subj_per_group,
  meanlog = 2.1,
  sdlog = 0.55,
  shift = 5,
  tau_0 = 0.9,
  tau_vis = 0.75,
  tau_spa = 0,
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

  Number of subjects per group. The total number of subjects will be
  `n_subj_per_group * 2`.

- meanlog:

  Mean of the log-normal distribution for the base RT values.

- sdlog:

  Standard deviation of the log-normal distribution for the base RT
  values.

- shift:

  Non-decision time, i.e., the minimum value of the RTs.

- tau_0:

  By-subject random intercept standard deviation.

- tau_vis:

  By-subject random slope standard deviation for the visual category.

- tau_spa:

  By-subject random slope standard deviation for the spatial category.

- beta_aph:

  Effect of the aphantasia group on the RTs.

- beta_vis:

  Effect of the visual category on the RTs.

- beta_spa:

  Effect of the spatial category on the RTs.

- beta_aph_vis:

  Effect of the interaction between the aphantasia group and the visual
  category on the RTs.

- beta_aph_spa:

  Effect of the interaction between the aphantasia group and the spatial
  category on the RTs.

- seed:

  Random seed for reproducibility. If `NULL`, no seed is set.

- ...:

  Additional arguments passed to the function. Unused.

## Value

A dataframe with the simulated RT data.

## Examples

``` r
# No main effects
df <- simulate_rt_data(100)
head(df)
#> # A tibble: 6 × 12
#>   id    group    category rt_total trial tau_0 tau_vis tau_spa beta_0 aphantasia
#>   <chr> <fct>    <fct>       <dbl> <int> <dbl>   <dbl>   <dbl>  <dbl>      <dbl>
#> 1 id001 Aphanta… Visual       19.5     1 0.249    1.42       0   17.9          1
#> 2 id001 Aphanta… Visual       16.8     2 0.249    1.42       0   15.1          1
#> 3 id001 Aphanta… Visual       18.9     3 0.249    1.42       0   17.3          1
#> 4 id001 Aphanta… Visual       13.7     4 0.249    1.42       0   12.0          1
#> 5 id001 Aphanta… Visual       14.6     5 0.249    1.42       0   12.9          1
#> 6 id001 Aphanta… Visual       19.7     6 0.249    1.42       0   18.0          1
#> # ℹ 2 more variables: visual <dbl>, spatial <dbl>

df |>
  dplyr::group_by(group, category) |>
  dplyr::reframe(
    mean_rt = mean(rt_total),
    median_rt = median(rt_total),
    sd_rt = sd(rt_total),
    min_rt = min(rt_total),
    max_rt = max(rt_total)
  )
#> # A tibble: 6 × 7
#>   group      category mean_rt median_rt sd_rt min_rt max_rt
#>   <fct>      <fct>      <dbl>     <dbl> <dbl>  <dbl>  <dbl>
#> 1 Aphantasia Control     14.6      13.2  5.70   4.74   45.1
#> 2 Aphantasia Spatial     14.6      13.4  5.43   5.16   46.2
#> 3 Aphantasia Visual      14.3      13.1  5.63   3.51   45.6
#> 4 Typical    Control     14.1      13.2  5.33   5.81   49.5
#> 5 Typical    Spatial     14.7      13.3  5.90   4.57   65.9
#> 6 Typical    Visual      14.2      12.8  5.87   4.72   59.7

# Visual category effect around 2.5s
df <- simulate_rt_data(100, beta_vis = 2.5)
df |>
  dplyr::group_by(group, category) |>
  dplyr::reframe(
    mean_rt = mean(rt_total),
    median_rt = median(rt_total),
    sd_rt = sd(rt_total),
    min_rt = min(rt_total),
    max_rt = max(rt_total)
  )
#> # A tibble: 6 × 7
#>   group      category mean_rt median_rt sd_rt min_rt max_rt
#>   <fct>      <fct>      <dbl>     <dbl> <dbl>  <dbl>  <dbl>
#> 1 Aphantasia Control     14.6      13.1  5.86   5.59   63.5
#> 2 Aphantasia Spatial     14.0      12.8  5.47   5.54   48.3
#> 3 Aphantasia Visual      16.9      15.8  5.59   8.32   59.8
#> 4 Typical    Control     14.1      12.7  5.14   5.62   37.7
#> 5 Typical    Spatial     14.3      12.9  5.86   5.57   70.7
#> 6 Typical    Visual      17.1      15.6  5.70   8.04   54.7
```
