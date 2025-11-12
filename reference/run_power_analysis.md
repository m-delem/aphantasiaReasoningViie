# Conduct a power analysis by simulation

This function simulates data for a range of parameters, fits a model to
each dataset, and extracts the p-values for the interaction contrast
between the group and the control-visual difference. It allows the user
to specify the range of sample sizes and the range of beta values for
the visual category effect size. This function is interactive, so try it
out in the console to see a breakdown of the parameters you chose and an
estimate of the time it will take to run the analysis.

## Usage

``` r
run_power_analysis(
  n_min = 20,
  n_max = 40,
  n_step = 10,
  beta_vis_min = 1,
  beta_vis_max = 3,
  beta_step = 0.5,
  n_simulations = 10,
  time_estimate = 0.1,
  test_skip = NA
)
```

## Arguments

- n_min:

  Smallest number of participants per group to simulate. The total
  number of participants will be `n_min * 2`.

- n_max:

  Largest number of participants per group to simulate. The total number
  of participants will be `n_max * 2`.

- n_step:

  Step size for the number of participants per group to test.

- beta_vis_min:

  Smallest value for the visual category effect to test.

- beta_vis_max:

  Largest value for the visual category effect to test.

- beta_step:

  Step size for the visual category effect to test.

- n_simulations:

  Number of simulations to run for each combination of parameters. This
  allows for a more robust estimation of the power.

- time_estimate:

  Estimated time (in seconds) to fit a single model on a single dataset.
  This is used to estimate the total time the power analysis will take.

- test_skip:

  Optional argument for testing purposes. See [this Stack
  thread](https://stackoverflow.com/questions/65740390/how-to-test-a-function-that-depends-on-a-menu-user-input)
  about testing functions with
  [`utils::menu()`](https://rdrr.io/r/utils/menu.html) inputs.

## Value

A data frame with the results of the power analysis, including the
parameters used and the p-values obtained for each simulation.
