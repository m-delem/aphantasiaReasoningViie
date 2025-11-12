# Simulate data, fit a model on it and test our hypothesis

This function simulates data based on the parameters provided, fits a
model to the simulated data, and extracts the p-value for the group x
control-visual interaction contrast (our theoretical contrast of
interest). This function is used repeatedly in a power analysis by
simulation.

## Usage

``` r
simulate_rt_test(n_subj_per_group, beta_vis, p = progressr::progressor(), ...)
```

## Arguments

- n_subj_per_group:

  Number of subjects per group. The total number of subjects will be
  `n_subj_per_group * 2`.

- beta_vis:

  Effect of the visual category on the RTs. This is used to create the
  main effect of interest by adding this effect to the typical group
  only (thus creating an interaction between group and category).

- p:

  A progressor object from the `progressr` package to update the
  progress bar during the simulation.

- ...:

  Additional arguments passed to the `simulate_rt_data` function. These
  can include parameters like `meanlog`, `sdlog`, `shift`, etc.

## Value

The p-value for the group x control-visual interaction contrast.

## Examples

``` r
simulate_rt_test(10, 1)   # Low power, unlikely to detect the effect
#> [1] 0.733
simulate_rt_test(40, 1.5) # Better power, more likely
#> [1] 0.05
simulate_rt_test(150, 1)  # High power, highly likely even for a small effect
#> [1] 0.02
```
