# Strategy use analyses

This vignette contains a full breakdown of the analyses of the
self-reported (using Likert scales) mental strategies that participants
used to solve the reasoning problems. The most interesting results are
synthetically presented in the main manuscript (preprint
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

First, let’s get the clean, analysis-ready data and create cognitive
style clusters using OSIVQ scores (see
[`vignette("preparing_data")`](https://m-delem.github.io/aphantasiaReasoningViie/articles/preparing_data.md)
and
[`vignette("osivq_clusters")`](https://m-delem.github.io/aphantasiaReasoningViie/articles/osivq_clusters.md)
for details).

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
```

By default, the `df_survey` data frame has one variable (one column)
containing the responses of each participant for each strategy question
(Visual, Spatial, Verbal, Semantic, Sensorimotor). We needed to have all
of these five modalities in a single “strategy” variable along with the
associated “score” variable for modelling, so we gathered these five
columns into two long columns by “pivoting” the strategies data in a
long format (reducing the number of columns and increasing the number of
rows). This operation is performed by the
[`pivot_strategies_longer()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/pivot_strategies_longer.md)
function.

``` r
df_strats_long <- pivot_strategies_longer(df_survey)

dplyr::glimpse(df_strats_long)
#> Rows: 520
#> Columns: 11
#> $ id             <fct> acdn247721443631359lzxb, acdn247721443631359lzxb, acdn2…
#> $ language       <fct> fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr,…
#> $ age            <int> 24, 24, 24, 24, 24, 26, 26, 26, 26, 26, 23, 23, 23, 23,…
#> $ gender         <fct> f, f, f, f, f, f, f, f, f, f, m, m, m, m, m, f, f, f, f…
#> $ group          <fct> Typical, Typical, Typical, Typical, Typical, Aphantasia…
#> $ group_2        <fct> Typical, Typical, Typical, Typical, Typical, Aphantasia…
#> $ group_3        <fct> Typical, Typical, Typical, Typical, Typical, Aphantasia…
#> $ strategy_group <fct> No visual strategy, No visual strategy, No visual strat…
#> $ cluster        <fct> Visualiser, Visualiser, Visualiser, Visualiser, Visuali…
#> $ strategy       <fct> Visual, Verbal, Spatial, Semantic, Sensorimotor, Visual…
#> $ score          <fct> no_use, mainly_this_strat, secondary_strat, no_use, onl…
```

## Method

As described in the manuscript:

> Ordinal cumulative link regression models were fitted using the
> ordinal package (Christensen, 2023) to predict the score (on a
> question about the use of a given strategy) with a grouping variable
> (VVIQ groups, OSIVQ clusters), Strategy (visual, verbal, spatial,
> semantic or sensorimotor) and their two-way interaction as fixed
> categorical predictors. We planned to analyse the contrasts between
> groups for each strategy separately.

Let’s break this down.

### The grouping variables

We used several grouping variables to classify participants, all of
which are in the `df_strats_long` data frame:

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

The same modelling pipeline was therefore applied three times, once for
each of the last three grouping variables.

### The modelling pipeline

The ordinal models were fitted using the
[`fit_clm()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/fit_clm.md)
function, which is a wrapper around the
[`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) function
that simply sets a probit link by default. After model fit, we checked
for potential singularity issues and model performance using the
[`get_singularity()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_singularity.md)
and
[`get_performance()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_performance.md)
functions created for the occasion (which are convenient wrappers around
the *performance* package).

Finally, we tested our hypotheses with marginal contrasts. This task was
performed with the
[`report_contrast()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/report_contrast.md)
function, which is a wrapper around several functions from the *emmeans*
package. We used it to compute strategy use contrasts between groups for
each strategy, contrasts between strategies within each group, and
interaction contrasts (differences in strategy contrasts between
groups).

Here we go!

## Results

### VVIQ 2 groups

``` r
m_strats_vviq_2 <- fit_clm(score ~ group_2 * strategy, df_strats_long)

# Singularity
m_strats_vviq_2 |> get_singularity()
#> The model is not singular, parameter estimates are trustworthy.

# Performance
m_strats_vviq_2 |> get_performance() |> knitr::kable(align = "c")
```

|  AIC   |  BIC   | RMSE  |
|:------:|:------:|:-----:|
| 1268.9 | 1324.2 | 2.400 |

``` r
# Group contrasts by strategy
m_strats_vviq_2 |> report_contrast(~ group_2 | strategy) |> knitr::kable()
```

| Contrast             | Strategy     | Difference | 95% CI          | p.value |
|:---------------------|:-------------|-----------:|:----------------|--------:|
| Aphantasia - Typical | Visual       |     -1.689 | \[-2.18, -1.2\] |   0.000 |
| Aphantasia - Typical | Spatial      |     -0.397 | \[-0.82, 0.03\] |   0.068 |
| Aphantasia - Typical | Verbal       |      0.267 | \[-0.14, 0.68\] |   0.203 |
| Aphantasia - Typical | Semantic     |      0.229 | \[-0.28, 0.74\] |   0.379 |
| Aphantasia - Typical | Sensorimotor |     -0.137 | \[-0.57, 0.29\] |   0.531 |

``` r
# Strategy contrasts within groups
m_strats_vviq_2 |> report_contrast(~ strategy | group_2) |> knitr::kable()
```

| Contrast                | group_2    | Difference | 95% CI           | p.value |
|:------------------------|:-----------|-----------:|:-----------------|--------:|
| Visual - Spatial        | Aphantasia |     -1.078 | \[-1.8, -0.36\]  |   0.000 |
| Visual - Verbal         | Aphantasia |     -2.454 | \[-3.18, -1.73\] |   0.000 |
| Visual - Semantic       | Aphantasia |     -0.302 | \[-1.06, 0.46\]  |   0.814 |
| Visual - Sensorimotor   | Aphantasia |     -0.953 | \[-1.67, -0.24\] |   0.003 |
| Spatial - Verbal        | Aphantasia |     -1.376 | \[-2, -0.75\]    |   0.000 |
| Spatial - Semantic      | Aphantasia |      0.775 | \[0.09, 1.46\]   |   0.017 |
| Spatial - Sensorimotor  | Aphantasia |      0.125 | \[-0.51, 0.76\]  |   0.983 |
| Verbal - Semantic       | Aphantasia |      2.152 | \[1.47, 2.84\]   |   0.000 |
| Verbal - Sensorimotor   | Aphantasia |      1.501 | \[0.87, 2.13\]   |   0.000 |
| Semantic - Sensorimotor | Aphantasia |     -0.651 | \[-1.33, 0.03\]  |   0.069 |
| Visual - Spatial        | Typical    |      0.214 | \[-0.33, 0.76\]  |   0.822 |
| Visual - Verbal         | Typical    |     -0.498 | \[-1.04, 0.04\]  |   0.088 |
| Visual - Semantic       | Typical    |      1.616 | \[0.98, 2.25\]   |   0.000 |
| Visual - Sensorimotor   | Typical    |      0.599 | \[0.05, 1.15\]   |   0.026 |
| Spatial - Verbal        | Typical    |     -0.713 | \[-1.26, -0.17\] |   0.004 |
| Spatial - Semantic      | Typical    |      1.401 | \[0.77, 2.03\]   |   0.000 |
| Spatial - Sensorimotor  | Typical    |      0.385 | \[-0.17, 0.94\]  |   0.323 |
| Verbal - Semantic       | Typical    |      2.114 | \[1.48, 2.75\]   |   0.000 |
| Verbal - Sensorimotor   | Typical    |      1.097 | \[0.54, 1.65\]   |   0.000 |
| Semantic - Sensorimotor | Typical    |     -1.017 | \[-1.65, -0.38\] |   0.000 |

``` r
# Interaction contrasts
m_strats_vviq_2 |> report_contrast(~ group_2 * strategy, interaction = TRUE) |>
  knitr::kable()
```

| group_2_pairwise     | Strategy contrast       | Difference | 95% CI           | p.value |
|:---------------------|:------------------------|-----------:|:-----------------|--------:|
| Aphantasia - Typical | Visual - Spatial        |     -1.292 | \[-1.94, -0.64\] |   0.000 |
| Aphantasia - Typical | Visual - Verbal         |     -1.956 | \[-2.6, -1.31\]  |   0.000 |
| Aphantasia - Typical | Visual - Semantic       |     -1.918 | \[-2.63, -1.21\] |   0.000 |
| Aphantasia - Typical | Visual - Sensorimotor   |     -1.552 | \[-2.2, -0.9\]   |   0.000 |
| Aphantasia - Typical | Spatial - Verbal        |     -0.664 | \[-1.26, -0.07\] |   0.028 |
| Aphantasia - Typical | Spatial - Semantic      |     -0.626 | \[-1.29, 0.04\]  |   0.065 |
| Aphantasia - Typical | Spatial - Sensorimotor  |     -0.260 | \[-0.86, 0.34\]  |   0.399 |
| Aphantasia - Typical | Verbal - Semantic       |      0.038 | \[-0.62, 0.69\]  |   0.910 |
| Aphantasia - Typical | Verbal - Sensorimotor   |      0.404 | \[-0.19, 1\]     |   0.182 |
| Aphantasia - Typical | Semantic - Sensorimotor |      0.366 | \[-0.3, 1.03\]   |   0.281 |

### VVIQ 3 groups

``` r
m_strats_vviq_3 <- fit_clm(score ~ group_3 * strategy, df_strats_long)

m_strats_vviq_3 |> get_singularity()
#> The model is not singular, parameter estimates are trustworthy.

m_strats_vviq_3 |> get_performance() |> knitr::kable(align = "c")
```

|  AIC   |  BIC   | RMSE  |
|:------:|:------:|:-----:|
| 1263.7 | 1340.3 | 2.399 |

``` r
m_strats_vviq_3 |> report_contrast(~ group_3 | strategy) |> knitr::kable()
```

| Contrast                   | Strategy     | Difference | 95% CI           | p.value |
|:---------------------------|:-------------|-----------:|:-----------------|--------:|
| Aphantasia - Hypophantasia | Visual       |     -1.606 | \[-2.84, -0.37\] |   0.006 |
| Aphantasia - Typical       | Visual       |     -2.643 | \[-3.74, -1.54\] |   0.000 |
| Hypophantasia - Typical    | Visual       |     -1.036 | \[-1.77, -0.3\]  |   0.003 |
| Aphantasia - Hypophantasia | Spatial      |      0.598 | \[-0.23, 1.42\]  |   0.205 |
| Aphantasia - Typical       | Spatial      |     -0.197 | \[-0.77, 0.38\]  |   0.703 |
| Hypophantasia - Typical    | Spatial      |     -0.795 | \[-1.55, -0.04\] |   0.036 |
| Aphantasia - Hypophantasia | Verbal       |     -0.093 | \[-0.85, 0.67\]  |   0.956 |
| Aphantasia - Typical       | Verbal       |      0.234 | \[-0.33, 0.8\]   |   0.595 |
| Hypophantasia - Typical    | Verbal       |      0.327 | \[-0.36, 1.02\]  |   0.507 |
| Aphantasia - Hypophantasia | Semantic     |     -0.202 | \[-1.11, 0.7\]   |   0.860 |
| Aphantasia - Typical       | Semantic     |      0.154 | \[-0.55, 0.86\]  |   0.866 |
| Hypophantasia - Typical    | Semantic     |      0.356 | \[-0.47, 1.18\]  |   0.570 |
| Aphantasia - Hypophantasia | Sensorimotor |      0.004 | \[-0.8, 0.8\]    |   1.000 |
| Aphantasia - Typical       | Sensorimotor |     -0.137 | \[-0.72, 0.45\]  |   0.849 |
| Hypophantasia - Typical    | Sensorimotor |     -0.140 | \[-0.87, 0.59\]  |   0.893 |

``` r
m_strats_vviq_3 |> report_contrast(~ strategy | group_3) |> knitr::kable()
```

| Contrast                | group_3       | Difference | 95% CI           | p.value |
|:------------------------|:--------------|-----------:|:-----------------|--------:|
| Visual - Spatial        | Aphantasia    |     -2.230 | \[-3.57, -0.89\] |   0.000 |
| Visual - Verbal         | Aphantasia    |     -3.378 | \[-4.72, -2.04\] |   0.000 |
| Visual - Semantic       | Aphantasia    |     -1.173 | \[-2.56, 0.21\]  |   0.140 |
| Visual - Sensorimotor   | Aphantasia    |     -1.904 | \[-3.24, -0.56\] |   0.001 |
| Spatial - Verbal        | Aphantasia    |     -1.148 | \[-1.92, -0.38\] |   0.000 |
| Spatial - Semantic      | Aphantasia    |      1.057 | \[0.2, 1.91\]    |   0.007 |
| Spatial - Sensorimotor  | Aphantasia    |      0.326 | \[-0.46, 1.11\]  |   0.787 |
| Verbal - Semantic       | Aphantasia    |      2.205 | \[1.34, 3.07\]   |   0.000 |
| Verbal - Sensorimotor   | Aphantasia    |      1.474 | \[0.69, 2.26\]   |   0.000 |
| Semantic - Sensorimotor | Aphantasia    |     -0.731 | \[-1.59, 0.13\]  |   0.140 |
| Visual - Spatial        | Hypophantasia |     -0.025 | \[-1.12, 1.07\]  |   1.000 |
| Visual - Verbal         | Hypophantasia |     -1.865 | \[-2.91, -0.82\] |   0.000 |
| Visual - Semantic       | Hypophantasia |      0.231 | \[-0.89, 1.35\]  |   0.980 |
| Visual - Sensorimotor   | Hypophantasia |     -0.294 | \[-1.36, 0.77\]  |   0.944 |
| Spatial - Verbal        | Hypophantasia |     -1.839 | \[-2.91, -0.77\] |   0.000 |
| Spatial - Semantic      | Hypophantasia |      0.257 | \[-0.88, 1.4\]   |   0.973 |
| Spatial - Sensorimotor  | Hypophantasia |     -0.268 | \[-1.35, 0.82\]  |   0.962 |
| Verbal - Semantic       | Hypophantasia |      2.096 | \[1, 3.2\]       |   0.000 |
| Verbal - Sensorimotor   | Hypophantasia |      1.571 | \[0.53, 2.61\]   |   0.000 |
| Semantic - Sensorimotor | Hypophantasia |     -0.525 | \[-1.64, 0.59\]  |   0.699 |
| Visual - Spatial        | Typical       |      0.216 | \[-0.33, 0.76\]  |   0.818 |
| Visual - Verbal         | Typical       |     -0.501 | \[-1.04, 0.04\]  |   0.085 |
| Visual - Semantic       | Typical       |      1.624 | \[0.99, 2.25\]   |   0.000 |
| Visual - Sensorimotor   | Typical       |      0.602 | \[0.05, 1.15\]   |   0.024 |
| Spatial - Verbal        | Typical       |     -0.717 | \[-1.26, -0.17\] |   0.003 |
| Spatial - Semantic      | Typical       |      1.408 | \[0.78, 2.04\]   |   0.000 |
| Spatial - Sensorimotor  | Typical       |      0.386 | \[-0.17, 0.94\]  |   0.319 |
| Verbal - Semantic       | Typical       |      2.125 | \[1.49, 2.76\]   |   0.000 |
| Verbal - Sensorimotor   | Typical       |      1.103 | \[0.55, 1.66\]   |   0.000 |
| Semantic - Sensorimotor | Typical       |     -1.021 | \[-1.65, -0.39\] |   0.000 |

``` r
m_strats_vviq_3 |> report_contrast(~ group_3 * strategy, interaction = TRUE) |>
  knitr::kable()
```

| group_3_pairwise           | Strategy contrast       | Difference | 95% CI           | p.value |
|:---------------------------|:------------------------|-----------:|:-----------------|--------:|
| Aphantasia - Hypophantasia | Visual - Spatial        |     -2.204 | \[-3.44, -0.96\] |   0.000 |
| Aphantasia - Typical       | Visual - Spatial        |     -2.446 | \[-3.48, -1.41\] |   0.000 |
| Hypophantasia - Typical    | Visual - Spatial        |     -0.241 | \[-1.12, 0.64\]  |   0.590 |
| Aphantasia - Hypophantasia | Visual - Verbal         |     -1.513 | \[-2.72, -0.3\]  |   0.014 |
| Aphantasia - Typical       | Visual - Verbal         |     -2.877 | \[-3.91, -1.84\] |   0.000 |
| Hypophantasia - Typical    | Visual - Verbal         |     -1.363 | \[-2.21, -0.52\] |   0.002 |
| Aphantasia - Hypophantasia | Visual - Semantic       |     -1.404 | \[-2.68, -0.13\] |   0.031 |
| Aphantasia - Typical       | Visual - Semantic       |     -2.796 | \[-3.89, -1.7\]  |   0.000 |
| Hypophantasia - Typical    | Visual - Semantic       |     -1.392 | \[-2.32, -0.47\] |   0.003 |
| Aphantasia - Hypophantasia | Visual - Sensorimotor   |     -1.610 | \[-2.84, -0.38\] |   0.010 |
| Aphantasia - Typical       | Visual - Sensorimotor   |     -2.506 | \[-3.55, -1.46\] |   0.000 |
| Hypophantasia - Typical    | Visual - Sensorimotor   |     -0.896 | \[-1.76, -0.03\] |   0.042 |
| Aphantasia - Hypophantasia | Spatial - Verbal        |      0.691 | \[-0.25, 1.63\]  |   0.149 |
| Aphantasia - Typical       | Spatial - Verbal        |     -0.431 | \[-1.11, 0.24\]  |   0.211 |
| Hypophantasia - Typical    | Spatial - Verbal        |     -1.122 | \[-1.98, -0.27\] |   0.010 |
| Aphantasia - Hypophantasia | Spatial - Semantic      |      0.800 | \[-0.22, 1.82\]  |   0.126 |
| Aphantasia - Typical       | Spatial - Semantic      |     -0.351 | \[-1.11, 0.41\]  |   0.367 |
| Hypophantasia - Typical    | Spatial - Semantic      |     -1.151 | \[-2.09, -0.21\] |   0.016 |
| Aphantasia - Hypophantasia | Spatial - Sensorimotor  |      0.594 | \[-0.37, 1.56\]  |   0.226 |
| Aphantasia - Typical       | Spatial - Sensorimotor  |     -0.061 | \[-0.75, 0.63\]  |   0.863 |
| Hypophantasia - Typical    | Spatial - Sensorimotor  |     -0.655 | \[-1.53, 0.22\]  |   0.143 |
| Aphantasia - Hypophantasia | Verbal - Semantic       |      0.109 | \[-0.88, 1.1\]   |   0.829 |
| Aphantasia - Typical       | Verbal - Semantic       |      0.080 | \[-0.67, 0.83\]  |   0.835 |
| Hypophantasia - Typical    | Verbal - Semantic       |     -0.029 | \[-0.93, 0.87\]  |   0.950 |
| Aphantasia - Hypophantasia | Verbal - Sensorimotor   |     -0.097 | \[-1.02, 0.83\]  |   0.837 |
| Aphantasia - Typical       | Verbal - Sensorimotor   |      0.371 | \[-0.31, 1.05\]  |   0.286 |
| Hypophantasia - Typical    | Verbal - Sensorimotor   |      0.467 | \[-0.37, 1.3\]   |   0.274 |
| Aphantasia - Hypophantasia | Semantic - Sensorimotor |     -0.206 | \[-1.22, 0.8\]   |   0.690 |
| Aphantasia - Typical       | Semantic - Sensorimotor |      0.290 | \[-0.48, 1.06\]  |   0.458 |
| Hypophantasia - Typical    | Semantic - Sensorimotor |      0.496 | \[-0.42, 1.42\]  |   0.290 |

### OSIVQ 3 clusters

``` r
m_strats_osivq  <- fit_clm(score ~ cluster * strategy, df_strats_long)

m_strats_osivq |> get_singularity()
#> The model is not singular, parameter estimates are trustworthy.

m_strats_osivq |> get_performance() |> knitr::kable(align = "c")
```

|  AIC   |  BIC   | RMSE  |
|:------:|:------:|:-----:|
| 1291.3 | 1367.8 | 2.403 |

``` r
m_strats_osivq |> report_contrast(~ cluster | strategy) |> knitr::kable()
```

| Contrast                 | Strategy     | Difference | 95% CI          | p.value |
|:-------------------------|:-------------|-----------:|:----------------|--------:|
| Visualiser - Spatialiser | Visual       |      0.810 | \[0.09, 1.53\]  |   0.024 |
| Visualiser - Verbaliser  | Visual       |      1.521 | \[0.91, 2.13\]  |   0.000 |
| Spatialiser - Verbaliser | Visual       |      0.710 | \[-0.07, 1.49\] |   0.083 |
| Visualiser - Spatialiser | Spatial      |      0.045 | \[-0.66, 0.75\] |   0.988 |
| Visualiser - Verbaliser  | Spatial      |      0.331 | \[-0.23, 0.89\] |   0.347 |
| Spatialiser - Verbaliser | Spatial      |      0.286 | \[-0.43, 1\]    |   0.614 |
| Visualiser - Spatialiser | Verbal       |     -0.388 | \[-1.07, 0.3\]  |   0.381 |
| Visualiser - Verbaliser  | Verbal       |     -0.404 | \[-0.95, 0.14\] |   0.186 |
| Spatialiser - Verbaliser | Verbal       |     -0.016 | \[-0.71, 0.67\] |   0.998 |
| Visualiser - Spatialiser | Semantic     |      0.146 | \[-0.73, 1.02\] |   0.919 |
| Visualiser - Verbaliser  | Semantic     |     -0.041 | \[-0.71, 0.63\] |   0.989 |
| Spatialiser - Verbaliser | Semantic     |     -0.187 | \[-1.07, 0.7\]  |   0.874 |
| Visualiser - Spatialiser | Sensorimotor |      0.104 | \[-0.61, 0.82\] |   0.937 |
| Visualiser - Verbaliser  | Sensorimotor |      0.267 | \[-0.3, 0.83\]  |   0.508 |
| Spatialiser - Verbaliser | Sensorimotor |      0.163 | \[-0.56, 0.89\] |   0.857 |

``` r
m_strats_osivq |> report_contrast(~ strategy | cluster) |> knitr::kable()
```

| Contrast                | Cluster     | Difference | 95% CI           | p.value |
|:------------------------|:------------|-----------:|:-----------------|--------:|
| Visual - Spatial        | Visualiser  |      0.297 | \[-0.33, 0.92\]  |   0.699 |
| Visual - Verbal         | Visualiser  |     -0.331 | \[-0.95, 0.29\]  |   0.590 |
| Visual - Semantic       | Visualiser  |      1.542 | \[0.84, 2.24\]   |   0.000 |
| Visual - Sensorimotor   | Visualiser  |      0.582 | \[-0.05, 1.21\]  |   0.087 |
| Spatial - Verbal        | Visualiser  |     -0.628 | \[-1.26, 0\]     |   0.050 |
| Spatial - Semantic      | Visualiser  |      1.245 | \[0.54, 1.95\]   |   0.000 |
| Spatial - Sensorimotor  | Visualiser  |      0.285 | \[-0.35, 0.92\]  |   0.740 |
| Verbal - Semantic       | Visualiser  |      1.873 | \[1.17, 2.58\]   |   0.000 |
| Verbal - Sensorimotor   | Visualiser  |      0.913 | \[0.28, 1.55\]   |   0.001 |
| Semantic - Sensorimotor | Visualiser  |     -0.960 | \[-1.67, -0.25\] |   0.002 |
| Visual - Spatial        | Spatialiser |     -0.469 | \[-1.46, 0.52\]  |   0.697 |
| Visual - Verbal         | Spatialiser |     -1.529 | \[-2.52, -0.54\] |   0.000 |
| Visual - Semantic       | Spatialiser |      0.877 | \[-0.25, 2\]     |   0.207 |
| Visual - Sensorimotor   | Spatialiser |     -0.124 | \[-1.12, 0.87\]  |   0.997 |
| Spatial - Verbal        | Spatialiser |     -1.061 | \[-2.02, -0.1\]  |   0.021 |
| Spatial - Semantic      | Spatialiser |      1.346 | \[0.24, 2.45\]   |   0.008 |
| Spatial - Sensorimotor  | Spatialiser |      0.344 | \[-0.63, 1.32\]  |   0.871 |
| Verbal - Semantic       | Spatialiser |      2.406 | \[1.3, 3.51\]    |   0.000 |
| Verbal - Sensorimotor   | Spatialiser |      1.405 | \[0.44, 2.37\]   |   0.001 |
| Semantic - Sensorimotor | Spatialiser |     -1.002 | \[-2.11, 0.11\]  |   0.099 |
| Visual - Spatial        | Verbaliser  |     -0.893 | \[-1.62, -0.16\] |   0.007 |
| Visual - Verbal         | Verbaliser  |     -2.256 | \[-2.99, -1.52\] |   0.000 |
| Visual - Semantic       | Verbaliser  |     -0.020 | \[-0.81, 0.77\]  |   1.000 |
| Visual - Sensorimotor   | Verbaliser  |     -0.672 | \[-1.4, 0.06\]   |   0.090 |
| Spatial - Verbal        | Verbaliser  |     -1.363 | \[-2.02, -0.7\]  |   0.000 |
| Spatial - Semantic      | Verbaliser  |      0.873 | \[0.14, 1.61\]   |   0.010 |
| Spatial - Sensorimotor  | Verbaliser  |      0.221 | \[-0.45, 0.89\]  |   0.896 |
| Verbal - Semantic       | Verbaliser  |      2.236 | \[1.5, 2.97\]    |   0.000 |
| Verbal - Sensorimotor   | Verbaliser  |      1.584 | \[0.91, 2.25\]   |   0.000 |
| Semantic - Sensorimotor | Verbaliser  |     -0.652 | \[-1.39, 0.08\]  |   0.111 |

``` r
m_strats_osivq |> report_contrast(~ cluster * strategy, interaction = TRUE) |>
  knitr::kable()
```

| Cluster contrast         | Strategy contrast       | Difference | 95% CI           | p.value |
|:-------------------------|:------------------------|-----------:|:-----------------|--------:|
| Visualiser - Spatialiser | Visual - Spatial        |      0.765 | \[-0.08, 1.61\]  |   0.075 |
| Visualiser - Verbaliser  | Visual - Spatial        |      1.190 | \[0.5, 1.88\]    |   0.001 |
| Spatialiser - Verbaliser | Visual - Spatial        |      0.424 | \[-0.46, 1.31\]  |   0.346 |
| Visualiser - Spatialiser | Visual - Verbal         |      1.198 | \[0.36, 2.03\]   |   0.005 |
| Visualiser - Verbaliser  | Visual - Verbal         |      1.925 | \[1.24, 2.61\]   |   0.000 |
| Spatialiser - Verbaliser | Visual - Verbal         |      0.727 | \[-0.14, 1.6\]   |   0.102 |
| Visualiser - Spatialiser | Visual - Semantic       |      0.665 | \[-0.28, 1.61\]  |   0.170 |
| Visualiser - Verbaliser  | Visual - Semantic       |      1.562 | \[0.8, 2.32\]    |   0.000 |
| Spatialiser - Verbaliser | Visual - Semantic       |      0.897 | \[-0.09, 1.88\]  |   0.075 |
| Visualiser - Spatialiser | Visual - Sensorimotor   |      0.706 | \[-0.14, 1.55\]  |   0.103 |
| Visualiser - Verbaliser  | Visual - Sensorimotor   |      1.254 | \[0.56, 1.95\]   |   0.000 |
| Spatialiser - Verbaliser | Visual - Sensorimotor   |      0.547 | \[-0.34, 1.44\]  |   0.227 |
| Visualiser - Spatialiser | Spatial - Verbal        |      0.433 | \[-0.39, 1.25\]  |   0.301 |
| Visualiser - Verbaliser  | Spatial - Verbal        |      0.735 | \[0.08, 1.39\]   |   0.027 |
| Spatialiser - Verbaliser | Spatial - Verbal        |      0.302 | \[-0.53, 1.13\]  |   0.475 |
| Visualiser - Spatialiser | Spatial - Semantic      |     -0.101 | \[-1.04, 0.84\]  |   0.833 |
| Visualiser - Verbaliser  | Spatial - Semantic      |      0.372 | \[-0.36, 1.1\]   |   0.317 |
| Spatialiser - Verbaliser | Spatial - Semantic      |      0.473 | \[-0.48, 1.42\]  |   0.329 |
| Visualiser - Spatialiser | Spatial - Sensorimotor  |     -0.059 | \[-0.89, 0.78\]  |   0.890 |
| Visualiser - Verbaliser  | Spatial - Sensorimotor  |      0.064 | \[-0.6, 0.73\]   |   0.850 |
| Spatialiser - Verbaliser | Spatial - Sensorimotor  |      0.123 | \[-0.73, 0.97\]  |   0.776 |
| Visualiser - Spatialiser | Verbal - Semantic       |     -0.534 | \[-1.46, 0.4\]   |   0.260 |
| Visualiser - Verbaliser  | Verbal - Semantic       |     -0.363 | \[-1.08, 0.36\]  |   0.322 |
| Spatialiser - Verbaliser | Verbal - Semantic       |      0.170 | \[-0.77, 1.11\]  |   0.722 |
| Visualiser - Spatialiser | Verbal - Sensorimotor   |     -0.492 | \[-1.32, 0.33\]  |   0.243 |
| Visualiser - Verbaliser  | Verbal - Sensorimotor   |     -0.671 | \[-1.33, -0.02\] |   0.044 |
| Spatialiser - Verbaliser | Verbal - Sensorimotor   |     -0.180 | \[-1.02, 0.66\]  |   0.674 |
| Visualiser - Spatialiser | Semantic - Sensorimotor |      0.042 | \[-0.9, 0.98\]   |   0.931 |
| Visualiser - Verbaliser  | Semantic - Sensorimotor |     -0.308 | \[-1.04, 0.42\]  |   0.409 |
| Spatialiser - Verbaliser | Semantic - Sensorimotor |     -0.350 | \[-1.3, 0.61\]   |   0.473 |

## Visualisation

Strategy data were visualised using two different representations: as
average Likert scores and as proportions of answers in each Likert
category for each strategy and group. These figures were created with
the functions
[`plot_strategies_scores()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_strategies_scores.md)
(for the average scores) and
[`plot_strategies_barplot()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_strategies_barplot.md)
(for the proportions) from the package. A little
[`add_significance()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_significance.md)
helper function was also created to add significance stars to the
average score plots.

``` r

library(patchwork)

star_size <- 3

ps1 <-
  plot_strategies_scores(
    df_strats_long,
    grouping = group_2,
    title = "VVIQ 2 groups",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  ) +
  add_significance(
    size_star = star_size,
    color = "#1b6096",
    tibble::tibble(
      x_star = 1.28,
      y_star = 4.67,
      stars  = "***",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = 4.6
    )
  )

ps2 <-
  plot_strategies_scores(
    df_strats_long,
    grouping = group_3,
    title = "VVIQ 3 groups",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  ) +
  add_significance(
    size_star = star_size,
    color = "#1b6096",
    tibble::tibble(
      x_star = 1.28,
      y_star = 4.67,
      stars  = "**",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = 4.6
    )
  ) +
  add_significance(
    size_star = star_size,
    color = "#1b6096",
    tibble::tibble(
      x_star = 2.28,
      y_star = 4.79,
      stars  = "**",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = 4.72
    )
  ) +
  add_significance(
    size_star = star_size,
    color = "#1b6096",
    tibble::tibble(
      x_star = 1.78,
      y_star = 5.07,
      stars  = "***",
      x_line = .data$x_star - 1,
      x_line_end = .data$x_star + 1,
      y_line = 5
    )
  ) +
  add_significance(
    size_star = star_size,
    color = "#318f2c",
    linetype = "longdash",
    lw = 0.3,
    tibble::tibble(
      x_star = 2.38,
      y_star = 4.54,
      stars  = "*",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = 4.47
    )
  )

ps3 <-
  plot_strategies_scores(
    df_strats_long,
    grouping = cluster,
    title = "OSIVQ clusters",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  ) +
  add_significance(
    size_star = star_size,
    color = "#1b6096",
    tibble::tibble(
      x_star = 2.28,
      y_star = 4.79,
      stars  = "*",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = 4.72
    )
  ) +
  add_significance(
    size_star = star_size,
    color = "#1b6096",
    tibble::tibble(
      x_star = 1.78,
      y_star = 5.07,
      stars  = "***",
      x_line = .data$x_star - 1,
      x_line_end = .data$x_star + 1,
      y_line = 5
    )
  )

ps <-
  ps1 + ps2 + ps3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

plot(ps)
```

![A plot showing the mean Likert score in each strategy for several
different grouping
variables.](analysing_strategies_files/figure-html/plot-strategies-average-1.png)

``` r
pb1 <- 
  plot_strategies_barplot(
    df_strats_long, group_2, title = "VVIQ 2 groups",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  )
pb2 <- 
  plot_strategies_barplot(
    df_strats_long, group_3, title = "VVIQ 3 groups",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  )
pb3 <- 
  plot_strategies_barplot(
    df_strats_long, cluster, title = "OSIVQ clusters",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  )

pb <-
  pb1 + pb2 + pb3 +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")
  
plot(pb)
```

![A plot showing the proportion of answers on each Likert score for each
strategy and several different grouping
variables.](analysing_strategies_files/figure-html/plot-strategies-proportions-1.png)

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
    #>  date     2025-11-12
    #>  pandoc   3.1.11 @ /opt/hostedtoolcache/pandoc/3.1.11/x64/ (via rmarkdown)
    #>  quarto   1.8.26 @ /usr/local/bin/quarto
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  ! package                 * version     date (UTC) lib source
    #>    abind                     1.4-8       2024-09-12 [1] RSPM
    #>    aphantasiaReasoningViie * 1.0         2025-11-12 [1] local
    #>    assertthat                0.2.1       2019-03-21 [1] RSPM
    #>    bslib                     0.9.0       2025-01-30 [1] RSPM
    #>    cachem                    1.1.0       2024-05-16 [1] RSPM
    #>  P class                     7.3-23      2025-01-01 [?] CRAN (R 4.5.2)
    #>    cli                       3.6.5       2025-04-23 [1] RSPM
    #>    clue                      0.3-66      2024-11-13 [1] RSPM
    #>  P cluster                   2.1.8.1     2025-03-12 [?] CRAN (R 4.5.2)
    #>    clusterCrit               1.3.0       2023-11-23 [1] RSPM
    #>    clValid                   0.7         2021-02-14 [1] RSPM
    #>    coda                      0.19-4.1    2024-01-31 [1] RSPM
    #>    combinat                  0.0-8       2012-10-29 [1] RSPM
    #>    crayon                    1.5.3       2024-06-20 [1] RSPM
    #>    curl                      7.0.0       2025-08-19 [1] RSPM
    #>    desc                      1.4.3       2023-12-10 [1] RSPM
    #>  P devtools                * 2.4.6       2025-10-03 [?] RSPM
    #>    diceR                     3.1.0       2025-06-19 [1] RSPM
    #>    digest                    0.6.37      2024-08-19 [1] RSPM
    #>    dplyr                     1.1.4       2023-11-17 [1] RSPM
    #>    e1071                     1.7-16      2024-09-16 [1] RSPM
    #>  P ellipsis                  0.3.2       2021-04-29 [?] RSPM
    #>    emmeans                   2.0.0       2025-10-29 [1] RSPM
    #>    estimability              1.5.1       2024-05-12 [1] RSPM
    #>    evaluate                  1.0.5       2025-08-27 [1] RSPM
    #>    farver                    2.1.2       2024-05-13 [1] RSPM
    #>    fastmap                   1.2.0       2024-05-15 [1] RSPM
    #>    forcats                   1.0.1       2025-09-25 [1] RSPM
    #>    fs                        1.6.6       2025-04-12 [1] RSPM
    #>    generics                  0.1.4       2025-05-09 [1] RSPM
    #>    ggplot2                   4.0.0       2025-09-11 [1] RSPM
    #>    glue                      1.8.0       2024-09-30 [1] RSPM
    #>    gtable                    0.3.6       2024-10-25 [1] RSPM
    #>    haven                     2.5.5       2025-05-30 [1] RSPM
    #>    highr                     0.11        2024-05-26 [1] RSPM
    #>    hms                       1.1.4       2025-10-17 [1] RSPM
    #>    htmltools                 0.5.8.1     2024-04-04 [1] RSPM
    #>    htmlwidgets               1.6.4       2023-12-06 [1] RSPM
    #>    httpuv                    1.6.16      2025-04-16 [1] RSPM
    #>    insight                   1.4.2       2025-09-02 [1] RSPM
    #>    jquerylib                 0.1.4       2021-04-26 [1] RSPM
    #>    jsonlite                  2.0.0       2025-03-27 [1] RSPM
    #>    klaR                      1.7-3       2023-12-13 [1] RSPM
    #>    knitr                     1.50        2025-03-16 [1] RSPM
    #>    labeling                  0.4.3       2023-08-29 [1] RSPM
    #>    labelled                  2.16.0      2025-10-22 [1] RSPM
    #>    later                     1.4.4       2025-08-27 [1] RSPM
    #>  P lattice                   0.22-7      2025-04-02 [?] CRAN (R 4.5.2)
    #>    lifecycle                 1.0.4       2023-11-07 [1] RSPM
    #>    magrittr                  2.0.4       2025-09-12 [1] RSPM
    #>  P MASS                      7.3-65      2025-02-28 [?] CRAN (R 4.5.2)
    #>  P Matrix                    1.7-4       2025-08-28 [?] CRAN (R 4.5.2)
    #>    mclust                    6.1.2       2025-10-31 [1] RSPM
    #>    memoise                   2.0.1       2021-11-26 [1] RSPM
    #>  P mgcv                      1.9-3       2025-04-04 [?] CRAN (R 4.5.2)
    #>    mime                      0.13        2025-03-17 [1] RSPM
    #>    miniUI                    0.1.2       2025-04-17 [1] RSPM
    #>    mvtnorm                   1.3-3       2025-01-10 [1] RSPM
    #>  P nlme                      3.1-168     2025-03-31 [?] CRAN (R 4.5.2)
    #>    numDeriv                  2016.8-1.1  2019-06-06 [1] RSPM
    #>    ordinal                   2023.12-4.1 2024-08-19 [1] RSPM
    #>    otel                      0.2.0       2025-08-29 [1] RSPM
    #>    patchwork               * 1.3.2       2025-08-25 [1] RSPM
    #>    performance               0.15.2      2025-10-06 [1] RSPM
    #>    pillar                    1.11.1      2025-09-17 [1] RSPM
    #>    pkgbuild                  1.4.8       2025-05-26 [1] RSPM
    #>    pkgconfig                 2.0.3       2019-09-22 [1] RSPM
    #>    pkgdown                   2.2.0       2025-11-06 [1] any (@2.2.0)
    #>    pkgload                   1.4.1       2025-09-23 [1] RSPM
    #>    promises                  1.5.0       2025-11-01 [1] RSPM
    #>    proxy                     0.4-27      2022-06-09 [1] RSPM
    #>    purrr                     1.2.0       2025-11-04 [1] RSPM
    #>    questionr                 0.8.1       2025-06-10 [1] RSPM
    #>    R6                        2.6.1       2025-02-15 [1] RSPM
    #>    ragg                      1.5.0       2025-09-02 [1] RSPM
    #>    RColorBrewer              1.1-3       2022-04-03 [1] RSPM
    #>    Rcpp                      1.1.0       2025-07-02 [1] RSPM
    #>  P remotes                   2.5.0       2024-03-17 [?] RSPM
    #>    renv                      1.1.4       2025-03-20 [1] RSPM (R 4.5.0)
    #>    rlang                     1.1.6       2025-04-11 [1] RSPM
    #>    rmarkdown                 2.30        2025-09-28 [1] RSPM
    #>    rstudioapi                0.17.1      2024-10-22 [1] RSPM
    #>    S7                        0.2.0       2024-11-07 [1] RSPM
    #>    sass                      0.4.10      2025-04-11 [1] RSPM
    #>    scales                    1.4.0       2025-04-24 [1] RSPM
    #>    sessioninfo               1.2.3       2025-02-05 [1] RSPM
    #>    shiny                     1.11.1      2025-07-03 [1] RSPM
    #>    showtext                  0.9-7       2024-03-02 [1] RSPM
    #>    showtextdb                3.0         2020-06-04 [1] RSPM
    #>    stringi                   1.8.7       2025-03-27 [1] RSPM
    #>    stringr                   1.6.0       2025-11-04 [1] RSPM
    #>    sysfonts                  0.8.9       2024-03-02 [1] RSPM
    #>    systemfonts               1.3.1       2025-10-01 [1] RSPM
    #>    textshaping               1.0.4       2025-10-10 [1] RSPM
    #>    tibble                    3.3.0       2025-06-08 [1] RSPM
    #>    tidyr                     1.3.1       2024-01-24 [1] RSPM
    #>    tidyselect                1.2.1       2024-03-11 [1] RSPM
    #>    ucminf                    1.2.2       2024-06-24 [1] RSPM
    #>  P usethis                 * 3.2.1       2025-09-06 [?] RSPM
    #>    vctrs                     0.6.5       2023-12-01 [1] RSPM
    #>    viridisLite               0.4.2       2023-05-02 [1] RSPM
    #>    withr                     3.0.2       2024-10-28 [1] RSPM
    #>    xfun                      0.54        2025-10-30 [1] RSPM
    #>    xtable                    1.8-4       2019-04-21 [1] RSPM
    #>    yaml                      2.3.10      2024-07-26 [1] RSPM
    #> 
    #>  [1] /home/runner/.cache/R/renv/library/aphantasiaReasoningViie-b75da44b/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu
    #>  [2] /home/runner/.cache/R/renv/sandbox/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/8f3cef43
    #> 
    #>  * ── Packages attached to the search path.
    #>  P ── Loaded and on-disk path mismatch.
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
