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
#> # A tibble: 6 × 13
#>   id    group_4    category rt_total trial group    tau_0 tau_vis tau_spa beta_0
#>   <chr> <fct>      <fct>       <dbl> <int> <fct>    <dbl>   <dbl>   <dbl>  <dbl>
#> 1 id001 Aphantasia Visual       11.1     1 Aphanta… -1.98   0.778       0   12.3
#> 2 id001 Aphantasia Visual       17.0     2 Aphanta… -1.98   0.778       0   18.2
#> 3 id001 Aphantasia Visual       23.0     3 Aphanta… -1.98   0.778       0   24.2
#> 4 id001 Aphantasia Visual       13.0     4 Aphanta… -1.98   0.778       0   14.2
#> 5 id001 Aphantasia Visual       14.0     5 Aphanta… -1.98   0.778       0   15.2
#> 6 id001 Aphantasia Visual       10.9     6 Aphanta… -1.98   0.778       0   12.1
#> # ℹ 3 more variables: aphantasia <dbl>, visual <dbl>, spatial <dbl>

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
#> 1 Aphantasia Control     14.5      13.3  5.49   5.73   47.1
#> 2 Aphantasia Spatial     14.3      13.2  5.36   4.98   49.8
#> 3 Aphantasia Visual      14.3      13.2  5.75   3.21   65.1
#> 4 Typical    Control     14.7      13.3  5.88   5.19   57.5
#> 5 Typical    Spatial     14.5      12.9  5.71   4.80   39.8
#> 6 Typical    Visual      14.4      13.3  5.49   5.43   46.0

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
#> 1 Aphantasia Control     14.2      13.2  5.34   5.54   58.5
#> 2 Aphantasia Spatial     14.1      12.9  5.26   5.71   38.9
#> 3 Aphantasia Visual      16.6      15.3  5.35   7.46   47.9
#> 4 Typical    Control     14.8      13.3  6.09   5.77   70.9
#> 5 Typical    Spatial     14.4      13.3  5.49   6.12   61.7
#> 6 Typical    Visual      17.0      15.5  6.01   7.12   56.2
```
