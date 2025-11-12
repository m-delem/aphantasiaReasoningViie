# Results of the power analysis by simulation presented in the manuscript

This dataset contains the results of the power analysis conducted by
fitting models on 147,000 simulated datasets with various sample sizes
and effect sizes. The results are used to estimate the statistical power
of the study to detect effects of different sizes with different sample
sizes. This analysis took 16 hours and 44 minutes on a machine with 40
cores, so naturally the dataset is included here to avoid having to redo
it. It contains the same columns as the results of the
[`run_power_analysis()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/run_power_analysis.md):

- `n_subj_per_group` (number of subjects per group in the simulated
  dataset)

- `beta_vis` (the effect size used in the simulation, in seconds)

- `n_simulations` (the identifier of the simulation, from 1 to the
  number of simulations per combination, here 350)

- `p_value` (the p-value of the effect of interest in the fitted model)

## Usage

``` r
power_results
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
147000 rows and 4 columns.
