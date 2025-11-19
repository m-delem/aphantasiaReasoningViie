# Response time analyses

This vignette contains a full breakdown of the analyses of participants’
total response times on the reasoning problems. The most interesting
results are synthetically presented in the main manuscript (preprint
[here](https://doi.org/10.31234/osf.io/vsjtb_v1)). The present document
aims to provide a full account of the steps taken to reach these
results, and to provide additional analyses that were not included in
the manuscript for brevity.

``` r
library(aphantasiaReasoningViie)
#> Welcome to aphantasiaReasoningViie.
#> See https://osf.io/hfbcp/ for the associated study.
```

## Data preparation

First, let’s get the clean, analysis-ready data, create cognitive style
clusters using OSIVQ scores (see
[`vignette("preparing_data")`](https://m-delem.github.io/aphantasiaReasoningViie/articles/preparing_data.md)
and
[`vignette("osivq_clusters")`](https://m-delem.github.io/aphantasiaReasoningViie/articles/osivq_clusters.md)
for details), and add this classification to the dataset containing
response times (RT) data.

``` r
df_survey  <- get_clean_data()$df_survey
# Clustering OSIVQ data
clustering <- cluster_osivq(df_survey)
# Adding named clusters to the survey data
df_survey <- add_named_clusters(
  df_survey, clustering,
  names  = c("Spatialiser", "Visualiser", "Verbaliser"),
  levels = c("Visualiser", "Spatialiser", "Verbaliser"),
  contrasts = c("_visualiser", "_spatialiser", "_verbaliser"),
  base = 1
)
# Merging with experiment data
df_expe <-
  dplyr::left_join(
    get_clean_data()$df_expe,
    df_survey |> dplyr::select(id, cluster),
    by = dplyr::join_by("id")
  ) |>
  dplyr::relocate(cluster, .after = "group")
```

For RT analyses, we removed incorrect trials and trials with suspicious
RT patterns. These filtering steps were gathered in the
[`filter_trials_on_rt()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/filter_trials_on_rt.md)
function, which also provides a short summary of the process.

``` r
df_rt <- 
  filter_trials_on_rt(df_expe, verbose = TRUE) |> 
  dplyr::select(id, group:strategy_group, problem, category, rt_total)
#> 
#> Outlier trials filtration summary
#> 2808 trials before filtering
#> 587 incorrect trials removed (20.9%)
#> 317 trials filtered based on mean + 2 * SD
#> (14.27% of remaining trials)
#> 1904 trials remaining after filtering

dplyr::glimpse(df_rt)
#> Rows: 1,904
#> Columns: 9
#> $ id             <fct> acdn247721443631359lzxb, acdn247721443631359lzxb, acdn2…
#> $ group          <fct> Typical, Typical, Typical, Typical, Typical, Typical, T…
#> $ cluster        <fct> Visualiser, Visualiser, Visualiser, Visualiser, Visuali…
#> $ group_2        <fct> Typical, Typical, Typical, Typical, Typical, Typical, T…
#> $ group_3        <fct> Typical, Typical, Typical, Typical, Typical, Typical, T…
#> $ strategy_group <fct> No visual strategy, No visual strategy, No visual strat…
#> $ problem        <int> 25, 1, 6, 9, 8, 5, 14, 17, 21, 12, 24, 3, 7, 22, 16, 4,…
#> $ category       <fct> Control, Visual, Visual, Visual, Visual, Visual, Spatia…
#> $ rt_total       <dbl> 18.924, 21.771, 21.817, 20.505, 27.169, 22.155, 26.500,…
```

## Method

As described in the manuscript:

> We fitted GLMMs with Gamma distributions and identity links to account
> for the skewed distributions of RTs, using the glmmTMB package
> (McGillycuddy et al., 2025). The models included a grouping variable
> (VVIQ groups, OSIVQ clusters), Category (visual, spatial, or control)
> along with their two-way interactions as fixed categorical predictors.
> Varying slopes and intercepts (“random effects”) have been added for
> each participant by category and for each problem by grouping
> variable.

Let’s break this down.

### The grouping variables

We used several grouping variables to classify participants, all of
which are in the `df_rt` data frame:

- `group` is the 4-group VVIQ classification with an “aphantasia” group
  (VVIQ = 16), “hypophantasia” group (VVIQ $\in$ \[17, 32\]), “typical”
  group (VVIQ $\in$ \[33, 74\]), and “hyperphantasia” group (VVIQ $\in$
  \[75, 80\]). It was not used in the analyses because the
  hyperphantasia group was too small (N = 4).

- `group_3` is the same as `group`, but with the hyperphantasia group
  merged with the typical group. It is referred to as “VVIQ 3 groups” in
  the manuscript results.

- `group_2` is the binary classification often used in aphantasia
  literature that additionally merges the hypophantasia group with the
  aphantasia group. It is refereed to as “VVIQ 2 groups” in the
  manuscript results.

- `cluster` is the cognitive style classification obtained from
  clustering OSIVQ scores. It contains three groups: “Visualiser”,
  “Spatialiser”, and “Verbaliser”. It is referred to as “OSIVQ 3
  clusters” in the manuscript results.

- `strategy_group` is a classification based on the self-reported
  strategies used by the participants to solve the problems. It focuses
  on whether participants used a visual mental imagery strategy and
  contains two groups: “Visual strategy user” and “No visual strategy”.
  It is not reported in the manuscript because it was suggested to us
  after submission by colleagues at a conference.

The same modelling pipeline was therefore applied four times, once for
each of the last four grouping variables.

### The modelling pipeline

We created a small
[`build_formula()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/build_formula.md)
helper function to write the formula easily, as we used the same model
structure a lot of times:

``` r
build_formula("rt_total", "group_2")
#> rt_total ~ group_2 * category + (category | id) + (group_2 | 
#>     problem)
#> <environment: 0x55ac01205940>

build_formula("rt_total", "cluster")
#> rt_total ~ cluster * category + (category | id) + (cluster | 
#>     problem)
#> <environment: 0x55ac012961b0>
```

The models were fitted using the
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html)
function (and a little
[`set_ranef_prior()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/set_ranef_prior.md)
helper function to set a weakly informative prior on the random effects,
which helps with convergence and singularity issues). After model fit,
we checked for potential singularity issues and model performance using
the
[`get_singularity()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_singularity.md)
and
[`get_performance()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_performance.md)
functions created for the occasion (which are convenient wrappers around
the *performance* package).

Finally, we tested our hypotheses with marginal contrasts. As stated in
the manuscript:

> Due to the way that variance is partitioned in GLMMs (Rights & Sterba,
> 2019), there does not exist an agreed-upon way to calculate standard
> effect sizes for individual terms such as main effects or interactions
> in these models. Thus, in line with general recommendations on how to
> report effect sizes (e.g., Pek & Flora, 2018), we report and analyse
> unstandardised effect sizes for post-hoc tests in the form of
> estimated marginal contrasts (i.e. differences in model-estimated
> marginal means, hereinafter denoted Δ), in seconds for RTs or as odds
> ratios for accuracies. To answer our hypotheses, we planned to analyse
> contrasts between groups, contrasts between categories for each group
> separately, and interaction contrasts testing the differences in
> category contrasts between the groups.

This task was performed with the
[`report_contrast()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/report_contrast.md)
function, which is a wrapper around several functions from the *emmeans*
package. We used it to compute response time contrasts between groups,
contrasts between categories within each group, and interaction
contrasts (differences in category contrasts between groups).

Here we go!

## Results

### VVIQ 2 groups

``` r
m_rt_vviq_2 <-
  glmmTMB::glmmTMB(
    data    = df_rt,
    formula = build_formula("rt_total", "group_2"),
    family  = Gamma(link = "identity"),
    prior   = set_ranef_prior(70)
  )

# Singularity
m_rt_vviq_2 |> get_singularity()
#> The model is not singular, parameter estimates are trustworthy.

# Performance
m_rt_vviq_2 |> get_performance() |> knitr::kable(align = "c")
```

|   AIC   |   BIC   | R2 (cond.) | R2 (marg.) |  ICC  | RMSE  |
|:-------:|:-------:|:----------:|:----------:|:-----:|:-----:|
| 12508.3 | 12597.1 |   0.998    |   0.027    | 0.998 | 5.910 |

``` r
# Group contrasts
m_rt_vviq_2 |> report_contrast(~ group_2) |> knitr::kable()
#> NOTE: Results may be misleading due to involvement in interactions
```

| Contrast             | Difference | 95% CI         | p.value |
|:---------------------|-----------:|:---------------|--------:|
| Aphantasia - Typical |     -0.628 | \[-2.96, 1.7\] |   0.598 |

``` r
# Category contrasts within groups
m_rt_vviq_2 |> report_contrast(~ category | group_2) |> knitr::kable()
```

| Contrast          | group_2    | Difference | 95% CI           | p.value |
|:------------------|:-----------|-----------:|:-----------------|--------:|
| Control - Spatial | Aphantasia |     -0.066 | \[-2.22, 2.09\]  |   0.997 |
| Control - Visual  | Aphantasia |     -1.685 | \[-3.82, 0.45\]  |   0.153 |
| Spatial - Visual  | Aphantasia |     -1.618 | \[-3.97, 0.73\]  |   0.241 |
| Control - Spatial | Typical    |      0.037 | \[-1.83, 1.91\]  |   0.999 |
| Control - Visual  | Typical    |     -2.411 | \[-4.27, -0.55\] |   0.007 |
| Spatial - Visual  | Typical    |     -2.448 | \[-4.52, -0.38\] |   0.015 |

``` r
# Interaction contrasts
m_rt_vviq_2 |> report_contrast(~ category * group_2, interaction = TRUE) |>
  knitr::kable()
```

| Category contrast | group_2_pairwise     | Difference | 95% CI          | p.value |
|:------------------|:---------------------|-----------:|:----------------|--------:|
| Control - Spatial | Aphantasia - Typical |     -0.103 | \[-1.51, 1.3\]  |   0.885 |
| Control - Visual  | Aphantasia - Typical |      0.726 | \[-0.63, 2.08\] |   0.294 |
| Spatial - Visual  | Aphantasia - Typical |      0.829 | \[-0.94, 2.6\]  |   0.358 |

### VVIQ 3 groups

``` r
m_rt_vviq_3 <-
  glmmTMB::glmmTMB(
    data    = df_rt,
    formula = build_formula("rt_total", "group_3"),
    family  = Gamma(link = "identity"),
    prior   = set_ranef_prior(70)
  )

m_rt_vviq_3 |> get_singularity()
#> The model is not singular, parameter estimates are trustworthy.

m_rt_vviq_3 |> get_performance() |> knitr::kable(align = "c")
```

|   AIC   |   BIC   | R2 (cond.) | R2 (marg.) |  ICC  | RMSE  |
|:-------:|:-------:|:----------:|:----------:|:-----:|:-----:|
| 12553.0 | 12675.1 |   0.998    |   0.054    | 0.998 | 5.909 |

``` r
m_rt_vviq_3 |> report_contrast(~ group_3) |> knitr::kable()
#> NOTE: Results may be misleading due to involvement in interactions
```

| Contrast                   | Difference | 95% CI          | p.value |
|:---------------------------|-----------:|:----------------|--------:|
| Aphantasia - Hypophantasia |      3.160 | \[-1.06, 7.38\] |   0.185 |
| Aphantasia - Typical       |      0.555 | \[-2.6, 3.71\]  |   0.911 |
| Hypophantasia - Typical    |     -2.605 | \[-6.45, 1.24\] |   0.251 |

``` r
m_rt_vviq_3 |> report_contrast(~ category | group_3) |> knitr::kable()
```

| Contrast          | group_3       | Difference | 95% CI           | p.value |
|:------------------|:--------------|-----------:|:-----------------|--------:|
| Control - Spatial | Aphantasia    |     -0.061 | \[-2.62, 2.5\]   |   0.998 |
| Control - Visual  | Aphantasia    |     -1.345 | \[-3.85, 1.16\]  |   0.420 |
| Spatial - Visual  | Aphantasia    |     -1.284 | \[-4.09, 1.52\]  |   0.532 |
| Control - Spatial | Hypophantasia |     -0.066 | \[-2.83, 2.7\]   |   0.998 |
| Control - Visual  | Hypophantasia |     -2.297 | \[-5, 0.41\]     |   0.115 |
| Spatial - Visual  | Hypophantasia |     -2.231 | \[-5.4, 0.94\]   |   0.225 |
| Control - Spatial | Typical       |      0.037 | \[-1.82, 1.9\]   |   0.999 |
| Control - Visual  | Typical       |     -2.434 | \[-4.28, -0.59\] |   0.006 |
| Spatial - Visual  | Typical       |     -2.470 | \[-4.52, -0.42\] |   0.013 |

``` r
m_rt_vviq_3 |> report_contrast(~ category * group_3, interaction = TRUE) |>
  knitr::kable()
```

| Category contrast | group_3_pairwise           | Difference | 95% CI          | p.value |
|:------------------|:---------------------------|-----------:|:----------------|--------:|
| Control - Spatial | Aphantasia - Hypophantasia |      0.006 | \[-2.11, 2.12\] |   0.996 |
| Control - Visual  | Aphantasia - Hypophantasia |      0.953 | \[-1.05, 2.95\] |   0.351 |
| Spatial - Visual  | Aphantasia - Hypophantasia |      0.947 | \[-1.71, 3.6\]  |   0.484 |
| Control - Spatial | Aphantasia - Typical       |     -0.097 | \[-1.79, 1.59\] |   0.910 |
| Control - Visual  | Aphantasia - Typical       |      1.089 | \[-0.53, 2.71\] |   0.187 |
| Spatial - Visual  | Aphantasia - Typical       |      1.186 | \[-0.9, 3.27\]  |   0.264 |
| Control - Spatial | Hypophantasia - Typical    |     -0.103 | \[-2.05, 1.85\] |   0.917 |
| Control - Visual  | Hypophantasia - Typical    |      0.136 | \[-1.73, 2\]    |   0.886 |
| Spatial - Visual  | Hypophantasia - Typical    |      0.239 | \[-2.21, 2.69\] |   0.848 |

### OSIVQ 3 clusters

``` r
m_rt_osivq <-
  glmmTMB::glmmTMB(
    data    = df_rt,
    formula = build_formula("rt_total", "cluster"),
    family  = Gamma(link = "identity"),
    prior   = set_ranef_prior(70)
  )

m_rt_osivq  |> get_singularity()
#> The model is not singular, parameter estimates are trustworthy.

m_rt_osivq  |> get_performance() |> knitr::kable(align = "c")
```

|   AIC   |   BIC   | R2 (cond.) | R2 (marg.) |  ICC  | RMSE  |
|:-------:|:-------:|:----------:|:----------:|:-----:|:-----:|
| 12555.2 | 12677.3 |   0.998    |   0.032    | 0.998 | 5.907 |

``` r
m_rt_osivq  |> report_contrast(~ cluster) |> knitr::kable()
#> NOTE: Results may be misleading due to involvement in interactions
```

| Contrast                 | Difference | 95% CI          | p.value |
|:-------------------------|-----------:|:----------------|--------:|
| Visualiser - Spatialiser |      1.265 | \[-2.62, 5.15\] |   0.726 |
| Visualiser - Verbaliser  |      0.667 | \[-2.4, 3.74\]  |   0.867 |
| Spatialiser - Verbaliser |     -0.597 | \[-4.49, 3.3\]  |   0.931 |

``` r
m_rt_osivq  |> report_contrast(~ category | cluster) |> knitr::kable()
```

| Contrast          | Cluster     | Difference | 95% CI          | p.value |
|:------------------|:------------|-----------:|:----------------|--------:|
| Control - Spatial | Visualiser  |     -0.266 | \[-2.2, 1.67\]  |   0.944 |
| Control - Visual  | Visualiser  |     -2.890 | \[-4.8, -0.98\] |   0.001 |
| Spatial - Visual  | Visualiser  |     -2.624 | \[-4.8, -0.44\] |   0.013 |
| Control - Spatial | Spatialiser |      0.206 | \[-2.43, 2.84\] |   0.982 |
| Control - Visual  | Spatialiser |     -1.365 | \[-3.94, 1.21\] |   0.429 |
| Spatial - Visual  | Spatialiser |     -1.572 | \[-4.58, 1.44\] |   0.440 |
| Control - Spatial | Verbaliser  |      0.167 | \[-2.08, 2.41\] |   0.983 |
| Control - Visual  | Verbaliser  |     -1.540 | \[-3.75, 0.67\] |   0.231 |
| Spatial - Visual  | Verbaliser  |     -1.707 | \[-4.16, 0.74\] |   0.231 |

``` r
m_rt_osivq  |> report_contrast(~ category * cluster, interaction = TRUE) |>
  knitr::kable()
```

| Category contrast | Cluster contrast         | Difference | 95% CI          | p.value |
|:------------------|:-------------------------|-----------:|:----------------|--------:|
| Control - Spatial | Visualiser - Spatialiser |     -0.473 | \[-2.42, 1.47\] |   0.634 |
| Control - Visual  | Visualiser - Spatialiser |     -1.525 | \[-3.38, 0.33\] |   0.107 |
| Spatial - Visual  | Visualiser - Spatialiser |     -1.052 | \[-3.5, 1.39\]  |   0.399 |
| Control - Spatial | Visualiser - Verbaliser  |     -0.433 | \[-2.04, 1.18\] |   0.598 |
| Control - Visual  | Visualiser - Verbaliser  |     -1.351 | \[-2.9, 0.2\]   |   0.087 |
| Spatial - Visual  | Visualiser - Verbaliser  |     -0.917 | \[-2.91, 1.07\] |   0.366 |
| Control - Spatial | Spatialiser - Verbaliser |      0.039 | \[-1.88, 1.95\] |   0.968 |
| Control - Visual  | Spatialiser - Verbaliser |      0.174 | \[-1.62, 1.97\] |   0.849 |
| Spatial - Visual  | Spatialiser - Verbaliser |      0.135 | \[-2.28, 2.54\] |   0.913 |

### Strategy groups

``` r
m_rt_strat <-
  glmmTMB::glmmTMB(
    data    = df_rt,
    formula = build_formula("rt_total", "strategy_group"),
    family  = Gamma(link = "identity"),
    prior   = set_ranef_prior(70)
  )

m_rt_strat |> get_singularity()
#> The model is not singular, parameter estimates are trustworthy.

m_rt_strat |> get_performance() |> knitr::kable(align = "c")
```

|   AIC   |   BIC   | R2 (cond.) | R2 (marg.) |  ICC  | RMSE  |
|:-------:|:-------:|:----------:|:----------:|:-----:|:-----:|
| 12505.6 | 12594.4 |   0.998    |   0.031    | 0.998 | 5.904 |

``` r
m_rt_strat |> report_contrast(~ strategy_group) |> knitr::kable()
#> NOTE: Results may be misleading due to involvement in interactions
```

| Contrast                                  | Difference | 95% CI          | p.value |
|:------------------------------------------|-----------:|:----------------|--------:|
| Visual strategy user - No visual strategy |     -0.895 | \[-3.21, 1.42\] |   0.449 |

``` r
m_rt_strat |> report_contrast(~ category | strategy_group) |> knitr::kable()
```

| Contrast          | strategy_group       | Difference | 95% CI           | p.value |
|:------------------|:---------------------|-----------:|:-----------------|--------:|
| Control - Spatial | Visual Strategy User |     -0.032 | \[-1.88, 1.82\]  |   0.999 |
| Control - Visual  | Visual Strategy User |     -2.523 | \[-4.36, -0.68\] |   0.004 |
| Spatial - Visual  | Visual Strategy User |     -2.491 | \[-4.55, -0.43\] |   0.013 |
| Control - Spatial | No Visual Strategy   |      0.018 | \[-2.19, 2.23\]  |   1.000 |
| Control - Visual  | No Visual Strategy   |     -1.605 | \[-3.8, 0.59\]   |   0.199 |
| Spatial - Visual  | No Visual Strategy   |     -1.623 | \[-4.01, 0.77\]  |   0.249 |

``` r
m_rt_strat |> report_contrast(~ category * strategy_group, interaction = TRUE) |>
  knitr::kable()
```

| Category contrast | strategy_group_pairwise                   | Difference | 95% CI          | p.value |
|:------------------|:------------------------------------------|-----------:|:----------------|--------:|
| Control - Spatial | Visual Strategy User - No Visual Strategy |     -0.050 | \[-1.47, 1.37\] |   0.945 |
| Control - Visual  | Visual Strategy User - No Visual Strategy |     -0.918 | \[-2.29, 0.46\] |   0.191 |
| Spatial - Visual  | Visual Strategy User - No Visual Strategy |     -0.868 | \[-2.65, 0.91\] |   0.340 |

## Visualisation

The figures showing the distribution of response times displayed in the
manuscript were created with the
[`plot_superb_raincloud()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_superb_raincloud.md)
function from the package, which wraps functions from the *ggplot2* and
*superb* packages to create nice visualisations easily. A little
[`add_significance()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_significance.md)
helper function was also created to add significance stars to the plots.

``` r
library(patchwork)
library(superb)

pr1 <-
  plot_superb_raincloud(
    df_rt, rt_total, group_2,
    title = "VVIQ 2 groups", y_title = "Mean total RT (s)",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    axis_rel_x = 1.2,
    exp_add_right = 0.7
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 2,
      y_star = 30,
      stars  = "**",
      x_line = .data$x_star - 0.14,
      x_line_end = .data$x_star + 0.14,
      y_line = 29.5
    )
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 2.07,
      y_star = 28,
      stars  = "*",
      x_line = .data$x_star - 0.07,
      x_line_end = .data$x_star + 0.07,
      y_line = 27.5
    )
  )

pr2 <-
  plot_superb_raincloud(
    df_rt, rt_total, group_3,
    title = "VVIQ 3 groups", y_title = "Mean total RT (s)",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    axis_rel_x = 1.2,
    exp_add_right = 0.7
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3,
      y_star = 30,
      stars  = "**",
      x_line = .data$x_star - 0.14,
      x_line_end = .data$x_star + 0.14,
      y_line = 29.5
    )
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3.07,
      y_star = 28,
      stars  = "*",
      x_line = .data$x_star - 0.07,
      x_line_end = .data$x_star + 0.07,
      y_line = 27.5
    )
  )

pr3 <-
  plot_superb_raincloud(
    df_rt, rt_total, cluster,
    title = "OSIVQ clusters", y_title = "Mean total RT (s)",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    axis_rel_x = 1.2,
    exp_add_right = 0.7
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3,
      y_star = 30,
      stars  = "***",
      x_line = .data$x_star - 0.14,
      x_line_end = .data$x_star + 0.14,
      y_line = 29.5
    )
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3.07,
      y_star = 28,
      stars  = "*",
      x_line = .data$x_star - 0.07,
      x_line_end = .data$x_star + 0.07,
      y_line = 27.5
    )
  )

pr <- pr1 + pr2 + pr3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

plot(pr)
```

![A jitter plot showing the mean RTs in each category for several
different grouping
variables.](analysing_rt_files/figure-html/plot-rt-1.png)

All done!

------------------------------------------------------------------------

    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting  value
    #>  version  R version 4.5.2 (2025-10-31)
    #>  os       Ubuntu 24.04.3 LTS
    #>  system   x86_64, linux-gnu
    #>  ui       X11
    #>  language en
    #>  collate  C.UTF-8
    #>  ctype    C.UTF-8
    #>  tz       UTC
    #>  date     2025-11-19
    #>  pandoc   3.1.11 @ /opt/hostedtoolcache/pandoc/3.1.11/x64/ (via rmarkdown)
    #>  quarto   1.8.26 @ /usr/local/bin/quarto
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  ! package                 * version    date (UTC) lib source
    #>    abind                     1.4-8      2024-09-12 [1] RSPM
    #>    aphantasiaReasoningViie * 1.0        2025-11-19 [1] local
    #>    assertthat                0.2.1      2019-03-21 [1] RSPM
    #>  P boot                      1.3-32     2025-08-29 [?] CRAN (R 4.5.2)
    #>    bslib                     0.9.0      2025-01-30 [1] RSPM
    #>    cachem                    1.1.0      2024-05-16 [1] RSPM
    #>  P class                     7.3-23     2025-01-01 [?] CRAN (R 4.5.2)
    #>    cli                       3.6.5      2025-04-23 [1] RSPM
    #>    clue                      0.3-66     2024-11-13 [1] RSPM
    #>  P cluster                   2.1.8.1    2025-03-12 [?] CRAN (R 4.5.2)
    #>    clusterCrit               1.3.0      2023-11-23 [1] RSPM
    #>    clValid                   0.7        2021-02-14 [1] RSPM
    #>    coda                      0.19-4.1   2024-01-31 [1] RSPM
    #>    combinat                  0.0-8      2012-10-29 [1] RSPM
    #>    crayon                    1.5.3      2024-06-20 [1] RSPM
    #>    curl                      7.0.0      2025-08-19 [1] RSPM
    #>    desc                      1.4.3      2023-12-10 [1] RSPM
    #>  P devtools                * 2.4.6      2025-10-03 [?] RSPM
    #>    diceR                     3.1.0      2025-06-19 [1] RSPM
    #>    digest                    0.6.38     2025-11-12 [1] RSPM
    #>    dplyr                     1.1.4      2023-11-17 [1] RSPM
    #>    e1071                     1.7-16     2024-09-16 [1] RSPM
    #>  P ellipsis                  0.3.2      2021-04-29 [?] RSPM
    #>    emmeans                   2.0.0      2025-10-29 [1] RSPM
    #>    estimability              1.5.1      2024-05-12 [1] RSPM
    #>    evaluate                  1.0.5      2025-08-27 [1] RSPM
    #>    farver                    2.1.2      2024-05-13 [1] RSPM
    #>    fastmap                   1.2.0      2024-05-15 [1] RSPM
    #>    forcats                   1.0.1      2025-09-25 [1] RSPM
    #>  P foreign                   0.8-90     2025-03-31 [?] CRAN (R 4.5.2)
    #>    fs                        1.6.6      2025-04-12 [1] RSPM
    #>    generics                  0.1.4      2025-05-09 [1] RSPM
    #>    ggplot2                   4.0.1      2025-11-14 [1] RSPM
    #>    glmmTMB                   1.1.13     2025-10-09 [1] RSPM
    #>    glue                      1.8.0      2024-09-30 [1] RSPM
    #>    gtable                    0.3.6      2024-10-25 [1] RSPM
    #>    haven                     2.5.5      2025-05-30 [1] RSPM
    #>    highr                     0.11       2024-05-26 [1] RSPM
    #>    hms                       1.1.4      2025-10-17 [1] RSPM
    #>    htmltools                 0.5.8.1    2024-04-04 [1] RSPM
    #>    htmlwidgets               1.6.4      2023-12-06 [1] RSPM
    #>    httpuv                    1.6.16     2025-04-16 [1] RSPM
    #>    insight                   1.4.2      2025-09-02 [1] RSPM
    #>    jquerylib                 0.1.4      2021-04-26 [1] RSPM
    #>    jsonlite                  2.0.0      2025-03-27 [1] RSPM
    #>    klaR                      1.7-3      2023-12-13 [1] RSPM
    #>    knitr                     1.50       2025-03-16 [1] RSPM
    #>    labelled                  2.16.0     2025-10-22 [1] RSPM
    #>    later                     1.4.4      2025-08-27 [1] RSPM
    #>  P lattice                   0.22-7     2025-04-02 [?] CRAN (R 4.5.2)
    #>    lifecycle                 1.0.4      2023-11-07 [1] RSPM
    #>    lme4                      1.1-37     2025-03-26 [1] RSPM
    #>    lsr                       0.5.2      2021-12-01 [1] RSPM
    #>    magrittr                  2.0.4      2025-09-12 [1] RSPM
    #>  P MASS                      7.3-65     2025-02-28 [?] CRAN (R 4.5.2)
    #>  P Matrix                    1.7-4      2025-08-28 [?] CRAN (R 4.5.2)
    #>    mclust                    6.1.2      2025-10-31 [1] RSPM
    #>    memoise                   2.0.1      2021-11-26 [1] RSPM
    #>  P mgcv                      1.9-3      2025-04-04 [?] CRAN (R 4.5.2)
    #>    mime                      0.13       2025-03-17 [1] RSPM
    #>    miniUI                    0.1.2      2025-04-17 [1] RSPM
    #>    minqa                     1.2.8      2024-08-17 [1] RSPM
    #>    mvtnorm                   1.3-3      2025-01-10 [1] RSPM
    #>  P nlme                      3.1-168    2025-03-31 [?] CRAN (R 4.5.2)
    #>    nloptr                    2.2.1      2025-03-17 [1] RSPM
    #>    numDeriv                  2016.8-1.1 2019-06-06 [1] RSPM
    #>    otel                      0.2.0      2025-08-29 [1] RSPM
    #>    patchwork               * 1.3.2      2025-08-25 [1] RSPM
    #>    performance               0.15.2     2025-10-06 [1] RSPM
    #>    pillar                    1.11.1     2025-09-17 [1] RSPM
    #>    pkgbuild                  1.4.8      2025-05-26 [1] RSPM
    #>    pkgconfig                 2.0.3      2019-09-22 [1] RSPM
    #>    pkgdown                   2.2.0      2025-11-06 [1] any (@2.2.0)
    #>    pkgload                   1.4.1      2025-09-23 [1] RSPM
    #>    plyr                      1.8.9      2023-10-02 [1] RSPM
    #>    promises                  1.5.0      2025-11-01 [1] RSPM
    #>    proxy                     0.4-27     2022-06-09 [1] RSPM
    #>    purrr                     1.2.0      2025-11-04 [1] RSPM
    #>    questionr                 0.8.1      2025-06-10 [1] RSPM
    #>    R6                        2.6.1      2025-02-15 [1] RSPM
    #>    ragg                      1.5.0      2025-09-02 [1] RSPM
    #>    rbibutils                 2.4        2025-11-07 [1] RSPM
    #>    RColorBrewer              1.1-3      2022-04-03 [1] RSPM
    #>    Rcpp                      1.1.0      2025-07-02 [1] RSPM
    #>    Rdpack                    2.6.4      2025-04-09 [1] RSPM
    #>    reformulas                0.4.2      2025-10-28 [1] RSPM
    #>  P remotes                   2.5.0      2024-03-17 [?] RSPM
    #>    renv                      1.1.4      2025-03-20 [1] RSPM (R 4.5.0)
    #>    reshape2                  1.4.5      2025-11-12 [1] RSPM
    #>    rlang                     1.1.6      2025-04-11 [1] RSPM
    #>    rmarkdown                 2.30       2025-09-28 [1] RSPM
    #>    rrapply                   1.2.7      2024-06-26 [1] RSPM
    #>    rstudioapi                0.17.1     2024-10-22 [1] RSPM
    #>    S7                        0.2.1      2025-11-14 [1] RSPM
    #>    sandwich                  3.1-1      2024-09-15 [1] RSPM
    #>    sass                      0.4.10     2025-04-11 [1] RSPM
    #>    scales                    1.4.0      2025-04-24 [1] RSPM
    #>    sessioninfo               1.2.3      2025-02-05 [1] RSPM
    #>    shiny                     1.11.1     2025-07-03 [1] RSPM
    #>    shinyBS                   0.61.1     2022-04-17 [1] RSPM
    #>    showtext                  0.9-7      2024-03-02 [1] RSPM
    #>    showtextdb                3.0        2020-06-04 [1] RSPM
    #>    stringi                   1.8.7      2025-03-27 [1] RSPM
    #>    stringr                   1.6.0      2025-11-04 [1] RSPM
    #>    superb                  * 1.0.0      2025-08-18 [1] RSPM
    #>    sysfonts                  0.8.9      2024-03-02 [1] RSPM
    #>    systemfonts               1.3.1      2025-10-01 [1] RSPM
    #>    textshaping               1.0.4      2025-10-10 [1] RSPM
    #>    tibble                    3.3.0      2025-06-08 [1] RSPM
    #>    tidyr                     1.3.1      2024-01-24 [1] RSPM
    #>    tidyselect                1.2.1      2024-03-11 [1] RSPM
    #>    TMB                       1.9.18     2025-10-13 [1] RSPM
    #>  P usethis                 * 3.2.1      2025-09-06 [?] RSPM
    #>    vctrs                     0.6.5      2023-12-01 [1] RSPM
    #>    withr                     3.0.2      2024-10-28 [1] RSPM
    #>    xfun                      0.54       2025-10-30 [1] RSPM
    #>    xtable                    1.8-4      2019-04-21 [1] RSPM
    #>    yaml                      2.3.10     2024-07-26 [1] RSPM
    #>    zoo                       1.8-14     2025-04-10 [1] RSPM
    #> 
    #>  [1] /home/runner/.cache/R/renv/library/aphantasiaReasoningViie-b75da44b/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu
    #>  [2] /home/runner/.cache/R/renv/sandbox/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/8f3cef43
    #> 
    #>  * ── Packages attached to the search path.
    #>  P ── Loaded and on-disk path mismatch.
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
