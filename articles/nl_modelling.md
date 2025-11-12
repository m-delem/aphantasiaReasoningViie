# Exploring RTs per phase (non-linear modelling)

This vignette contains a full breakdown of the exploratory non-linear
modelling conducted to analyse the evolution of participants’ response
times over the course of the trial phases. As stated in the manuscript:

> In addition to the total response times per trial usually analysed in
> VIIE studies, we collected response time data for each trial phase
> (three premises and the conclusion). We explored whether the VIIE
> could be specific to certain phases, or whether it was a difference in
> the dynamics of reasoning across trial phases rather than an overall
> difference in speed, by modelling response times throughout a trial
> with non-linear models.

The non-linear modelling used here was heavily inspired by [this
tutorial by Ping Hei Yeung](https://yeungpinghei.github.io/).

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
for details), add this classification to the dataset containing response
times (RT) data and filter trials based on RTs (see
[`vignette("analysing_rt")`](https://m-delem.github.io/aphantasiaReasoningViie/articles/analysing_rt.md)
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
# Merging with experiment data
df_expe <-
  dplyr::left_join(
    get_clean_data()$df_expe,
    df_survey |> dplyr::select(id, cluster),
    by = dplyr::join_by("id")
  ) |>
  dplyr::relocate(cluster, .after = "group")

df_rt <- filter_trials_on_rt(df_expe) |> dplyr::select(id:conclusion_rt)

dplyr::glimpse(df_rt)
#> Rows: 1,904
#> Columns: 15
#> $ id             <fct> acdn247721443631359lzxb, acdn247721443631359lzxb, acdn2…
#> $ language       <fct> fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr,…
#> $ group          <fct> Typical, Typical, Typical, Typical, Typical, Typical, T…
#> $ cluster        <fct> Visualiser, Visualiser, Visualiser, Visualiser, Visuali…
#> $ group_2        <fct> Typical, Typical, Typical, Typical, Typical, Typical, T…
#> $ group_3        <fct> Typical, Typical, Typical, Typical, Typical, Typical, T…
#> $ strategy_group <fct> No visual strategy, No visual strategy, No visual strat…
#> $ expe_phase     <fct> expe_block_1, expe_block_1, expe_block_1, expe_block_1,…
#> $ trial_number   <int> 2, 5, 7, 8, 9, 11, 14, 15, 16, 17, 18, 19, 20, 21, 22, …
#> $ problem        <int> 25, 1, 6, 9, 8, 5, 14, 17, 21, 12, 24, 3, 7, 22, 16, 4,…
#> $ category       <fct> Control, Visual, Visual, Visual, Visual, Visual, Spatia…
#> $ premise_1_rt   <dbl> 7.412, 5.773, 5.596, 4.378, 8.030, 3.038, 4.623, 6.415,…
#> $ premise_2_rt   <dbl> 3.148, 1.559, 1.646, 7.822, 4.926, 2.881, 4.603, 2.603,…
#> $ premise_3_rt   <dbl> 4.477, 9.679, 9.206, 3.674, 9.846, 8.729, 8.430, 5.320,…
#> $ conclusion_rt  <dbl> 3.887, 4.760, 5.369, 4.631, 4.367, 7.507, 8.844, 2.512,…
```

By default, the `df_rt` data frame has one variable (one column)
containing the response times of each participant for each trial term
(or “phase”, as referred to in the manuscript — “term” is the word
commonly used in VIIE literature). We needed to have all of these terms
(premises and conclusion) in a single “term” variable along with the
associated “rt” variable for modelling, so we gathered these four
columns into two long columns by “pivoting” the RT data in a long format
(reducing the number of columns and increasing the number of rows). This
operation is performed by the
[`pivot_terms_longer()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/pivot_terms_longer.md)
function.

We also needed to create “interaction” variables between the grouping
variables and the categories of the problems (visual, spatial, control)
using the
[`base::interaction()`](https://rdrr.io/r/base/interaction.html)
function because *mgcv* models used for non-linear modelling do not
support interaction terms natively.

``` r
df_rt_long <-
  pivot_terms_longer(df_rt) |>
  dplyr::mutate(
    group_2_category = interaction(group_2, category),
    group_3_category = interaction(group_3, category),
    cluster_category = interaction(cluster, category)
  )

dplyr::glimpse(df_rt_long)
#> Rows: 7,616
#> Columns: 17
#> $ id               <fct> acdn247721443631359lzxb, acdn247721443631359lzxb, acd…
#> $ language         <fct> fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, f…
#> $ group            <fct> Typical, Typical, Typical, Typical, Typical, Typical,…
#> $ cluster          <fct> Visualiser, Visualiser, Visualiser, Visualiser, Visua…
#> $ group_2          <fct> Typical, Typical, Typical, Typical, Typical, Typical,…
#> $ group_3          <fct> Typical, Typical, Typical, Typical, Typical, Typical,…
#> $ strategy_group   <fct> No visual strategy, No visual strategy, No visual str…
#> $ expe_phase       <fct> expe_block_1, expe_block_1, expe_block_1, expe_block_…
#> $ trial_number     <int> 2, 2, 2, 2, 5, 5, 5, 5, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9,…
#> $ problem          <fct> 25, 25, 25, 25, 1, 1, 1, 1, 6, 6, 6, 6, 9, 9, 9, 9, 8…
#> $ category         <fct> Control, Control, Control, Control, Visual, Visual, V…
#> $ term             <dbl> 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2,…
#> $ term_name        <fct> Premise 1, Premise 2, Premise 3, Conclusion, Premise …
#> $ rt               <dbl> 7.412, 3.148, 4.477, 3.887, 5.773, 1.559, 9.679, 4.76…
#> $ group_2_category <fct> Typical.Control, Typical.Control, Typical.Control, Ty…
#> $ group_3_category <fct> Typical.Control, Typical.Control, Typical.Control, Ty…
#> $ cluster_category <fct> Visualiser.Control, Visualiser.Control, Visualiser.Co…
```

## Method

As described in the manuscript:

> We fitted generalised additive models using the mgcv package (Wood,
> 2011) to predict response times with a grouping variable (VVIQ groups,
> OSIVQ clusters), Category (visual, spatial or control) and their
> interaction as fixed categorical predictors, as well as “smooth”
> (non-linear) terms that capture the evolution of RTs across trial
> terms (premise 1/2/3 and conclusion) for each grouping and category,
> each problem for each grouping, and each participant for each category
> (the latter two being equivalent to “random effects” in mixed models).

Let’s break this down.

### The grouping variables

We used several grouping variables to classify participants, all of
which are in the `df_rt_long` data frame:

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

The models were fitted using the
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) function. After
model fit, we tested group and category differences with marginal
contrasts. This task was performed with the
[`get_contrast()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_contrast.md)
function, which is a wrapper around several functions from the *emmeans*
package.

These models took a long time to fit and needed to be saved for ease of
reuse, but model fits were too heavy to be kept as package data. Thus,
two sub-products of the models were saved as package data instead:

- Contrast analyses obtained with
  [`get_contrast()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_contrast.md)
  for the statistical tests. The analyses for the three grouping
  variables have been saved in the `nl_contrasts` list.

- Model predictions obtained with
  [`modelbased::estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.html)
  for the data used for visualisation. The predictions for the three
  grouping variables have been saved in the `nl_predictions` list.

The code to fit the models and get the contrasts is provided below but
does not run.

## Results

### VVIQ 2 groups

``` r
m_nl_vviq_2 <-
  mgcv::bam(
    formula = rt ~
      group_2_category +
      s(term, by = group_2_category,  bs = "tp", k = 4) +
      s(term, problem, by = group_2,  bs = "fs", m = 1, k = 4) +
      s(term, id,      by = category, bs = "fs", m = 1, k = 4),
    family  = Gamma(link = "identity"),
    data    = df_rt_long,
    method  = "fREML"
  )

contrasts_vviq_2 <-
  m_nl_vviq_2 |>
  get_contrast(
    ~ group_2_category | term,
    at = list(term = c(1, 2, 3, 4)),
    interaction = FALSE,
    adjust = "none"
  )
```

The chunk above does not run, but the contrasts can be accessed natively
in the package in the `nl_contrasts` object. The code below tweaks them
a bit for a prettier display:

``` r
nl_contrasts$vviq_2 |>
  as.data.frame() |>
  tidyr::separate_wider_delim(
    contrast, " - ", names = c("group_cat_1", "group_cat_2")
  ) |>
  tidyr::separate_wider_delim(
    group_cat_1, ".", names = c("group_1", "category_1")
  ) |>
  tidyr::separate_wider_delim(
    group_cat_2, ".", names = c("group_2", "category_2")
  ) |>
  dplyr::filter(group_1 == group_2 & p.value < 1) |>
  dplyr::select(!c(
    tidyselect::contains("group_2"),
    tidyselect::contains("cluster_2"),
    tidyselect::contains("SE"),
    tidyselect::contains("df")
  )) |>
  dplyr::mutate(across(c(estimate:p.value), ~round(., 3))) |>
  tidyr::unite(`Category contrast`, category_1, category_2, sep = " - ") |>
  dplyr::rename(group = group_1, `RT difference` = estimate) |>
  dplyr::arrange(term, group) |>
  dplyr::mutate(
    term = term |>
      as.character() |>
      dplyr::case_match(
        "1" ~ "Premise 1",
        "2" ~ "Premise 2",
        "3" ~ "Premise 3",
        "4" ~ "Conclusion"
      ),
  ) |>
  knitr::kable(digits = 3)
#> Loading required namespace: emmeans
```

| group      | Category contrast | term       | RT difference | t.ratio | p.value |
|:-----------|:------------------|:-----------|--------------:|--------:|--------:|
| Aphantasia | Control - Spatial | Premise 1  |         0.239 |   0.495 |   0.621 |
| Aphantasia | Control - Visual  | Premise 1  |        -0.095 |  -0.185 |   0.853 |
| Aphantasia | Spatial - Visual  | Premise 1  |        -0.334 |  -0.668 |   0.504 |
| Typical    | Control - Spatial | Premise 1  |         0.028 |   0.061 |   0.951 |
| Typical    | Control - Visual  | Premise 1  |        -0.656 |  -1.323 |   0.186 |
| Typical    | Spatial - Visual  | Premise 1  |        -0.685 |  -1.409 |   0.159 |
| Aphantasia | Control - Spatial | Premise 2  |        -0.681 |  -1.707 |   0.088 |
| Aphantasia | Control - Visual  | Premise 2  |        -0.015 |  -0.037 |   0.971 |
| Aphantasia | Spatial - Visual  | Premise 2  |         0.666 |   1.610 |   0.107 |
| Typical    | Control - Spatial | Premise 2  |        -1.177 |  -3.070 |   0.002 |
| Typical    | Control - Visual  | Premise 2  |        -0.573 |  -1.448 |   0.148 |
| Typical    | Spatial - Visual  | Premise 2  |         0.604 |   1.511 |   0.131 |
| Aphantasia | Control - Spatial | Premise 3  |         0.561 |   1.341 |   0.180 |
| Aphantasia | Control - Visual  | Premise 3  |        -0.765 |  -1.704 |   0.088 |
| Aphantasia | Spatial - Visual  | Premise 3  |        -1.327 |  -3.015 |   0.003 |
| Typical    | Control - Spatial | Premise 3  |         0.850 |   2.104 |   0.035 |
| Typical    | Control - Visual  | Premise 3  |        -0.586 |  -1.354 |   0.176 |
| Typical    | Spatial - Visual  | Premise 3  |        -1.436 |  -3.398 |   0.001 |
| Aphantasia | Control - Spatial | Conclusion |        -0.069 |  -0.152 |   0.879 |
| Aphantasia | Control - Visual  | Conclusion |        -0.598 |  -1.226 |   0.220 |
| Aphantasia | Spatial - Visual  | Conclusion |        -0.529 |  -1.113 |   0.266 |
| Typical    | Control - Spatial | Conclusion |        -0.072 |  -0.164 |   0.870 |
| Typical    | Control - Visual  | Conclusion |        -0.472 |  -1.004 |   0.315 |
| Typical    | Spatial - Visual  | Conclusion |        -0.400 |  -0.869 |   0.385 |

Let’s do the same for the other two classifications.

### VVIQ 3 groups

``` r
m_nl_vviq_3 <-
  mgcv::bam(
    formula = rt ~
      group_3_category +
      s(term, by = group_3_category,  bs = "tp", k = 4) +
      s(term, problem, by = group_3,  bs = "fs", m = 1, k = 4) +
      s(term, id,      by = category, bs = "fs", m = 1, k = 4),
    family  = Gamma(link = "identity"),
    data    = df_rt_long,
    method  = "fREML"
  )

contrasts_vviq_3 <-
  m_nl_vviq_3 |>
  get_contrast(
    ~ group_3_category | term,
    at = list(term = c(1, 2, 3, 4)),
    interaction = FALSE,
    adjust = "none"
  )
```

Same process as above, contrasts are saved in the package:

``` r
nl_contrasts$vviq_3 |>
  as.data.frame() |>
  tidyr::separate_wider_delim(
    contrast, " - ", names = c("group_cat_1", "group_cat_2")
  ) |>
  tidyr::separate_wider_delim(
    group_cat_1, ".", names = c("group_1", "category_1")
  ) |>
  tidyr::separate_wider_delim(
    group_cat_2, ".", names = c("group_2", "category_2")
  ) |>
  dplyr::filter(group_1 == group_2 & p.value < 1) |>
  dplyr::select(!c(
    tidyselect::contains("group_2"),
    tidyselect::contains("cluster_2"),
    tidyselect::contains("SE"),
    tidyselect::contains("df")
  )) |>
  dplyr::mutate(across(c(estimate:p.value), ~round(., 3))) |>
  tidyr::unite(`Category contrast`, category_1, category_2, sep = " - ") |>
  dplyr::rename(group = group_1, `RT difference` = estimate) |>
  dplyr::arrange(term, group) |>
  dplyr::mutate(
    term = term |>
      as.character() |>
      dplyr::case_match(
        "1" ~ "Premise 1",
        "2" ~ "Premise 2",
        "3" ~ "Premise 3",
        "4" ~ "Conclusion"
      ),
  ) |>
  knitr::kable(digits = 3)
```

| group         | Category contrast | term       | RT difference | t.ratio | p.value |
|:--------------|:------------------|:-----------|--------------:|--------:|--------:|
| Aphantasia    | Control - Spatial | Premise 1  |         0.464 |   0.782 |   0.434 |
| Aphantasia    | Control - Visual  | Premise 1  |         0.135 |   0.212 |   0.832 |
| Aphantasia    | Spatial - Visual  | Premise 1  |        -0.329 |  -0.534 |   0.593 |
| Hypophantasia | Control - Spatial | Premise 1  |        -0.058 |  -0.083 |   0.934 |
| Hypophantasia | Control - Visual  | Premise 1  |        -0.494 |  -0.645 |   0.519 |
| Hypophantasia | Spatial - Visual  | Premise 1  |        -0.436 |  -0.587 |   0.557 |
| Typical       | Control - Spatial | Premise 1  |         0.029 |   0.063 |   0.950 |
| Typical       | Control - Visual  | Premise 1  |        -0.659 |  -1.331 |   0.183 |
| Typical       | Spatial - Visual  | Premise 1  |        -0.688 |  -1.418 |   0.156 |
| Aphantasia    | Control - Spatial | Premise 2  |        -0.984 |  -2.026 |   0.043 |
| Aphantasia    | Control - Visual  | Premise 2  |        -0.099 |  -0.197 |   0.844 |
| Aphantasia    | Spatial - Visual  | Premise 2  |         0.885 |   1.745 |   0.081 |
| Hypophantasia | Control - Spatial | Premise 2  |        -0.328 |  -0.554 |   0.580 |
| Hypophantasia | Control - Visual  | Premise 2  |         0.097 |   0.155 |   0.877 |
| Hypophantasia | Spatial - Visual  | Premise 2  |         0.425 |   0.685 |   0.493 |
| Typical       | Control - Spatial | Premise 2  |        -1.176 |  -3.088 |   0.002 |
| Typical       | Control - Visual  | Premise 2  |        -0.574 |  -1.456 |   0.145 |
| Typical       | Spatial - Visual  | Premise 2  |         0.601 |   1.508 |   0.132 |
| Aphantasia    | Control - Spatial | Premise 3  |         0.435 |   0.842 |   0.400 |
| Aphantasia    | Control - Visual  | Premise 3  |        -0.828 |  -1.486 |   0.137 |
| Aphantasia    | Spatial - Visual  | Premise 3  |        -1.263 |  -2.313 |   0.021 |
| Hypophantasia | Control - Spatial | Premise 3  |         0.785 |   1.287 |   0.198 |
| Hypophantasia | Control - Visual  | Premise 3  |        -0.648 |  -0.961 |   0.337 |
| Hypophantasia | Spatial - Visual  | Premise 3  |        -1.434 |  -2.202 |   0.028 |
| Typical       | Control - Spatial | Premise 3  |         0.851 |   2.120 |   0.034 |
| Typical       | Control - Visual  | Premise 3  |        -0.587 |  -1.358 |   0.175 |
| Typical       | Spatial - Visual  | Premise 3  |        -1.438 |  -3.412 |   0.001 |
| Aphantasia    | Control - Spatial | Conclusion |        -0.159 |  -0.295 |   0.768 |
| Aphantasia    | Control - Visual  | Conclusion |        -0.535 |  -0.913 |   0.361 |
| Aphantasia    | Spatial - Visual  | Conclusion |        -0.376 |  -0.657 |   0.511 |
| Hypophantasia | Control - Spatial | Conclusion |         0.111 |   0.164 |   0.869 |
| Hypophantasia | Control - Visual  | Conclusion |        -0.682 |  -0.910 |   0.363 |
| Hypophantasia | Spatial - Visual  | Conclusion |        -0.794 |  -1.097 |   0.273 |
| Typical       | Control - Spatial | Conclusion |        -0.070 |  -0.160 |   0.873 |
| Typical       | Control - Visual  | Conclusion |        -0.473 |  -1.009 |   0.313 |
| Typical       | Spatial - Visual  | Conclusion |        -0.403 |  -0.877 |   0.381 |

### OSIVQ 3 clusters

``` r
m_nl_osivq <-
  mgcv::bam(
    formula = rt ~
      cluster_category +
      s(term, by = cluster_category,    bs = "tp", k = 4) +
      s(term, problem, by = cluster,    bs = "fs", m = 1, k = 4) +
      s(term, id,      by = category, bs = "fs", m = 1, k = 4),
    family  = Gamma(link = "identity"),
    data    = df_rt_long,
    method  = "fREML"
  )

contrasts_osivq <-
  m_nl_osivq |>
  get_contrast(
    ~ cluster_category | term,
    at = list(term = c(1, 2, 3, 4)),
    interaction = TRUE
  )
```

Same process as above, contrasts are saved in the package:

``` r
nl_contrasts$osivq |>
  as.data.frame() |>
  tidyr::separate_wider_delim(
    cluster_category_pairwise, " - ", names = c("cluster_cat_1", "cluster_cat_2")
  ) |>
  tidyr::separate_wider_delim(
    cluster_cat_1, ".", names = c("cluster_1", "category_1")
  ) |>
  tidyr::separate_wider_delim(
    cluster_cat_2, ".", names = c("cluster_2", "category_2")
  ) |>
  dplyr::filter(cluster_1 == cluster_2 & p.value < 1) |>
  dplyr::select(!c(
    tidyselect::contains("group_2"),
    tidyselect::contains("cluster_2"),
    tidyselect::contains("SE"),
    tidyselect::contains("df")
  )) |>
  dplyr::mutate(across(c(estimate:p.value), ~round(., 3))) |>
  tidyr::unite(`Category contrast`, category_1, category_2, sep = " - ") |>
  dplyr::rename(cluster = cluster_1, `RT difference` = estimate) |>
  dplyr::arrange(term, cluster) |>
  dplyr::mutate(
    term = dplyr::case_match(
      term,
      1 ~ "Premise 1",
      2 ~ "Premise 2",
      3 ~ "Premise 3",
      4 ~ "Conclusion"
    ),
  ) |>
  knitr::kable(digits = 3)
```

| cluster     | Category contrast | term       | RT difference | t.ratio | p.value |
|:------------|:------------------|:-----------|--------------:|--------:|--------:|
| Spatialiser | Control - Spatial | Premise 1  |         0.157 |   0.229 |   0.819 |
| Spatialiser | Control - Visual  | Premise 1  |        -0.297 |  -0.399 |   0.690 |
| Spatialiser | Spatial - Visual  | Premise 1  |        -0.454 |  -0.633 |   0.527 |
| Verbaliser  | Control - Spatial | Premise 1  |         0.447 |   0.867 |   0.386 |
| Verbaliser  | Control - Visual  | Premise 1  |         0.164 |   0.299 |   0.765 |
| Verbaliser  | Spatial - Visual  | Premise 1  |        -0.283 |  -0.533 |   0.594 |
| Visualiser  | Control - Spatial | Premise 1  |        -0.158 |  -0.301 |   0.763 |
| Visualiser  | Control - Visual  | Premise 1  |        -0.993 |  -1.766 |   0.077 |
| Visualiser  | Spatial - Visual  | Premise 1  |        -0.835 |  -1.514 |   0.130 |
| Spatialiser | Control - Spatial | Premise 2  |        -0.681 |  -1.172 |   0.241 |
| Spatialiser | Control - Visual  | Premise 2  |        -0.435 |  -0.714 |   0.475 |
| Spatialiser | Spatial - Visual  | Premise 2  |         0.245 |   0.402 |   0.688 |
| Verbaliser  | Control - Spatial | Premise 2  |        -0.786 |  -1.862 |   0.063 |
| Verbaliser  | Control - Visual  | Premise 2  |        -0.012 |  -0.028 |   0.978 |
| Verbaliser  | Spatial - Visual  | Premise 2  |         0.774 |   1.769 |   0.077 |
| Visualiser  | Control - Spatial | Premise 2  |        -1.288 |  -3.001 |   0.003 |
| Visualiser  | Control - Visual  | Premise 2  |        -0.562 |  -1.267 |   0.205 |
| Visualiser  | Spatial - Visual  | Premise 2  |         0.726 |   1.623 |   0.105 |
| Spatialiser | Control - Spatial | Premise 3  |         0.600 |   0.983 |   0.326 |
| Spatialiser | Control - Visual  | Premise 3  |        -0.346 |  -0.528 |   0.597 |
| Spatialiser | Spatial - Visual  | Premise 3  |        -0.946 |  -1.483 |   0.138 |
| Verbaliser  | Control - Spatial | Premise 3  |         0.539 |   1.215 |   0.224 |
| Verbaliser  | Control - Visual  | Premise 3  |        -0.785 |  -1.649 |   0.099 |
| Verbaliser  | Spatial - Visual  | Premise 3  |        -1.324 |  -2.834 |   0.005 |
| Visualiser  | Control - Spatial | Premise 3  |         1.003 |   2.212 |   0.027 |
| Visualiser  | Control - Visual  | Premise 3  |        -0.693 |  -1.409 |   0.159 |
| Visualiser  | Spatial - Visual  | Premise 3  |        -1.696 |  -3.565 |   0.000 |
| Spatialiser | Control - Spatial | Conclusion |         0.069 |   0.105 |   0.916 |
| Spatialiser | Control - Visual  | Conclusion |        -0.406 |  -0.571 |   0.568 |
| Spatialiser | Spatial - Visual  | Conclusion |        -0.475 |  -0.691 |   0.490 |
| Verbaliser  | Control - Spatial | Conclusion |        -0.008 |  -0.016 |   0.987 |
| Verbaliser  | Control - Visual  | Conclusion |        -0.657 |  -1.269 |   0.205 |
| Verbaliser  | Spatial - Visual  | Conclusion |        -0.649 |  -1.287 |   0.198 |
| Visualiser  | Control - Spatial | Conclusion |        -0.189 |  -0.380 |   0.704 |
| Visualiser  | Control - Visual  | Conclusion |        -0.478 |  -0.906 |   0.365 |
| Visualiser  | Spatial - Visual  | Conclusion |        -0.289 |  -0.560 |   0.576 |

## Visualisation

The non-linear dynamics were represented using model predictions for
each term, grouping and category. These predictions have been computed
with the
[`modelbased::estimate_relation()`](https://easystats.github.io/modelbased/reference/estimate_expectation.html)
function and the code below (which does not run here):

``` r
# Getting model predictions (for plotting)
preds_2 <-
  modelbased::estimate_relation(
    m_nl_vviq_2,
    by = c("group_2_category", "term")
  ) |>
  as.data.frame() |>
  dplyr::select(group_2_category, term, Predicted, CI_low, CI_high) |>
  tidyr::separate_wider_delim(
    group_2_category,
    delim = ".",
    names = c("group", "category")
  )

preds_3 <-
  modelbased::estimate_relation(
    m_nl_vviq_3,
    by = c("group_3_category", "term")
  ) |>
  as.data.frame() |>
  dplyr::select(group_3_category, term, Predicted, CI_low, CI_high) |>
  tidyr::separate_wider_delim(
    group_3_category,
    delim = ".",
    names = c("group", "category")
  )

preds_osivq <-
  modelbased::estimate_relation(
    m_nl_osivq,
    by = c("cluster_category", "term")
  ) |>
  as.data.frame() |>
  dplyr::select(cluster_category, term, Predicted, CI_low, CI_high) |>
  tidyr::separate_wider_delim(
    cluster_category,
    delim = ".",
    names = c("group", "category")
  )
```

… And have been saved as package data in the `nl_predictions` object. We
created a
[`plot_nl()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_nl.md)
function to visualise the data easily and added significance labels
based on the contrasts with the
[`add_significance()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_significance.md)
helper function (which is why the code below is so lengthy).

``` r
library(patchwork)

pnl1 <-
  plot_nl(
    nl_predictions$vviq_2, title = "VVIQ 2 groups",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  )  +

  # 2nd premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Aphantasia")),
      x_star = 1.93,
      y_star = 5.97,
      stars  = "°",
      x_line = .data$x_star - 0.06,
      x_line_end = .data$x_star + 0.06,
      y_line = 5.9
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Typical")),
      x_star = 1.93,
      y_star = 6.24,
      stars  = "**",
      x_line = .data$x_star - 0.06,
      x_line_end = .data$x_star + 0.06,
      y_line = 6.17
    )
  ) +

  # 3rd premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Aphantasia")),
      x_star = 3.06,
      y_star = 7.87,
      stars  = "***",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 7.8
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Aphantasia")),
      x_star = 3.01,
      y_star = 8.27,
      stars  = "°",
      x_line = .data$x_star - 0.1,
      x_line_end = .data$x_star + 0.1,
      y_line = 8.2
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Typical")),
      x_star = 3.06,
      y_star = 8.47,
      stars  = "***",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 8.4
    )
  )  +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Typical")),
      x_star = 2.93,
      y_star = 7.77,
      stars  = "*",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 7.7
    )
  )

pnl2 <-
  plot_nl(
    nl_predictions$vviq_3,
    title = "VVIQ 3 groups",
    plot.margin = ggplot2::margin(t = 10),
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  ) +

  # 2nd premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Aphantasia")),
      x_star = 1.93,
      y_star = 6.63,
      stars  = "*",
      x_line = .data$x_star - 0.06,
      x_line_end = .data$x_star + 0.06,
      y_line = 6.57
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Typical")),
      x_star = 1.93,
      y_star = 6.24,
      stars  = "**",
      x_line = .data$x_star - 0.06,
      x_line_end = .data$x_star + 0.06,
      y_line = 6.17
    )
  )  +

  # 3rd premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Aphantasia")),
      x_star = 3.06,
      y_star = 8.32,
      stars  = "*",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 8.25
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Hypophantasia")),
      x_star = 3.06,
      y_star = 7.62,
      stars  = "*",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 7.55
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Typical")),
      x_star = 3.06,
      y_star = 8.47,
      stars  = "***",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 8.4
    )
  )  +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Typical")),
      x_star = 2.93,
      y_star = 7.77,
      stars  = "*",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 7.7
    )
  )

pnl3 <-
  plot_nl(
    nl_predictions$osivq,
    title = "OSIVQ clusters",
    plot.margin = ggplot2::margin(t = 10),
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA)
  ) +

  # 1st premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Visualiser")),
      x_star = 1,
      y_star = 8.47,
      stars  = "°",
      x_line = .data$x_star - 0.1,
      x_line_end = .data$x_star + 0.1,
      y_line = 8.4
    )
  ) +

  # 2nd premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Visualiser")),
      x_star = 1.93,
      y_star = 6.37,
      stars  = "**",
      x_line = .data$x_star - 0.06,
      x_line_end = .data$x_star + 0.06,
      y_line = 6.3
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Verbaliser")),
      x_star = 1.93,
      y_star = 6.17,
      stars  = "°",
      x_line = .data$x_star - 0.06,
      x_line_end = .data$x_star + 0.06,
      y_line = 6.1
    )
  ) +

  # 3rd premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Visualiser")),
      x_star = 3.06,
      y_star = 8.62,
      stars  = "***",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 8.55
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Visualiser")),
      x_star = 2.93,
      y_star = 7.77,
      stars  = "*",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 7.7
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Verbaliser")),
      x_star = 3.06,
      y_star = 8.04,
      stars  = "*",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 7.97
    )
  )

pnl <- pnl1 / pnl2 / pnl3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

plot(pnl)
```

![Plots showing the mean RTs in each trial term and category for several
different grouping variables.
](nl_modelling_files/figure-html/plot-nl-1.png)

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
    #>  ! package                 * version  date (UTC) lib source
    #>    abind                     1.4-8    2024-09-12 [1] RSPM
    #>    aphantasiaReasoningViie * 1.0      2025-11-12 [1] local
    #>    assertthat                0.2.1    2019-03-21 [1] RSPM
    #>    bslib                     0.9.0    2025-01-30 [1] RSPM
    #>    cachem                    1.1.0    2024-05-16 [1] RSPM
    #>  P class                     7.3-23   2025-01-01 [?] CRAN (R 4.5.2)
    #>    cli                       3.6.5    2025-04-23 [1] RSPM
    #>    clue                      0.3-66   2024-11-13 [1] RSPM
    #>  P cluster                   2.1.8.1  2025-03-12 [?] CRAN (R 4.5.2)
    #>    clusterCrit               1.3.0    2023-11-23 [1] RSPM
    #>    clValid                   0.7      2021-02-14 [1] RSPM
    #>    coda                      0.19-4.1 2024-01-31 [1] RSPM
    #>    combinat                  0.0-8    2012-10-29 [1] RSPM
    #>    crayon                    1.5.3    2024-06-20 [1] RSPM
    #>    curl                      7.0.0    2025-08-19 [1] RSPM
    #>    desc                      1.4.3    2023-12-10 [1] RSPM
    #>  P devtools                * 2.4.6    2025-10-03 [?] RSPM
    #>    diceR                     3.1.0    2025-06-19 [1] RSPM
    #>    digest                    0.6.37   2024-08-19 [1] RSPM
    #>    dplyr                     1.1.4    2023-11-17 [1] RSPM
    #>    e1071                     1.7-16   2024-09-16 [1] RSPM
    #>  P ellipsis                  0.3.2    2021-04-29 [?] RSPM
    #>    emmeans                   2.0.0    2025-10-29 [1] RSPM
    #>    estimability              1.5.1    2024-05-12 [1] RSPM
    #>    evaluate                  1.0.5    2025-08-27 [1] RSPM
    #>    farver                    2.1.2    2024-05-13 [1] RSPM
    #>    fastmap                   1.2.0    2024-05-15 [1] RSPM
    #>    forcats                   1.0.1    2025-09-25 [1] RSPM
    #>    fs                        1.6.6    2025-04-12 [1] RSPM
    #>    generics                  0.1.4    2025-05-09 [1] RSPM
    #>    ggplot2                   4.0.0    2025-09-11 [1] RSPM
    #>    glue                      1.8.0    2024-09-30 [1] RSPM
    #>    gtable                    0.3.6    2024-10-25 [1] RSPM
    #>    haven                     2.5.5    2025-05-30 [1] RSPM
    #>    highr                     0.11     2024-05-26 [1] RSPM
    #>    hms                       1.1.4    2025-10-17 [1] RSPM
    #>    htmltools                 0.5.8.1  2024-04-04 [1] RSPM
    #>    htmlwidgets               1.6.4    2023-12-06 [1] RSPM
    #>    httpuv                    1.6.16   2025-04-16 [1] RSPM
    #>    jquerylib                 0.1.4    2021-04-26 [1] RSPM
    #>    jsonlite                  2.0.0    2025-03-27 [1] RSPM
    #>    klaR                      1.7-3    2023-12-13 [1] RSPM
    #>    knitr                     1.50     2025-03-16 [1] RSPM
    #>    labelled                  2.16.0   2025-10-22 [1] RSPM
    #>    later                     1.4.4    2025-08-27 [1] RSPM
    #>  P lattice                   0.22-7   2025-04-02 [?] CRAN (R 4.5.2)
    #>    lifecycle                 1.0.4    2023-11-07 [1] RSPM
    #>    magrittr                  2.0.4    2025-09-12 [1] RSPM
    #>  P MASS                      7.3-65   2025-02-28 [?] CRAN (R 4.5.2)
    #>  P Matrix                    1.7-4    2025-08-28 [?] CRAN (R 4.5.2)
    #>    mclust                    6.1.2    2025-10-31 [1] RSPM
    #>    memoise                   2.0.1    2021-11-26 [1] RSPM
    #>  P mgcv                      1.9-3    2025-04-04 [?] CRAN (R 4.5.2)
    #>    mime                      0.13     2025-03-17 [1] RSPM
    #>    miniUI                    0.1.2    2025-04-17 [1] RSPM
    #>    mvtnorm                   1.3-3    2025-01-10 [1] RSPM
    #>  P nlme                      3.1-168  2025-03-31 [?] CRAN (R 4.5.2)
    #>    otel                      0.2.0    2025-08-29 [1] RSPM
    #>    patchwork               * 1.3.2    2025-08-25 [1] RSPM
    #>    pillar                    1.11.1   2025-09-17 [1] RSPM
    #>    pkgbuild                  1.4.8    2025-05-26 [1] RSPM
    #>    pkgconfig                 2.0.3    2019-09-22 [1] RSPM
    #>    pkgdown                   2.2.0    2025-11-06 [1] any (@2.2.0)
    #>    pkgload                   1.4.1    2025-09-23 [1] RSPM
    #>    promises                  1.5.0    2025-11-01 [1] RSPM
    #>    proxy                     0.4-27   2022-06-09 [1] RSPM
    #>    purrr                     1.2.0    2025-11-04 [1] RSPM
    #>    questionr                 0.8.1    2025-06-10 [1] RSPM
    #>    R6                        2.6.1    2025-02-15 [1] RSPM
    #>    ragg                      1.5.0    2025-09-02 [1] RSPM
    #>    RColorBrewer              1.1-3    2022-04-03 [1] RSPM
    #>    Rcpp                      1.1.0    2025-07-02 [1] RSPM
    #>  P remotes                   2.5.0    2024-03-17 [?] RSPM
    #>    renv                      1.1.4    2025-03-20 [1] RSPM (R 4.5.0)
    #>    rlang                     1.1.6    2025-04-11 [1] RSPM
    #>    rmarkdown                 2.30     2025-09-28 [1] RSPM
    #>    rstudioapi                0.17.1   2024-10-22 [1] RSPM
    #>    S7                        0.2.0    2024-11-07 [1] RSPM
    #>    sass                      0.4.10   2025-04-11 [1] RSPM
    #>    scales                    1.4.0    2025-04-24 [1] RSPM
    #>    sessioninfo               1.2.3    2025-02-05 [1] RSPM
    #>    shiny                     1.11.1   2025-07-03 [1] RSPM
    #>    showtext                  0.9-7    2024-03-02 [1] RSPM
    #>    showtextdb                3.0      2020-06-04 [1] RSPM
    #>    stringi                   1.8.7    2025-03-27 [1] RSPM
    #>    stringr                   1.6.0    2025-11-04 [1] RSPM
    #>    sysfonts                  0.8.9    2024-03-02 [1] RSPM
    #>    systemfonts               1.3.1    2025-10-01 [1] RSPM
    #>    textshaping               1.0.4    2025-10-10 [1] RSPM
    #>    tibble                    3.3.0    2025-06-08 [1] RSPM
    #>    tidyr                     1.3.1    2024-01-24 [1] RSPM
    #>    tidyselect                1.2.1    2024-03-11 [1] RSPM
    #>  P usethis                 * 3.2.1    2025-09-06 [?] RSPM
    #>    vctrs                     0.6.5    2023-12-01 [1] RSPM
    #>    withr                     3.0.2    2024-10-28 [1] RSPM
    #>    xfun                      0.54     2025-10-30 [1] RSPM
    #>    xtable                    1.8-4    2019-04-21 [1] RSPM
    #>    yaml                      2.3.10   2024-07-26 [1] RSPM
    #> 
    #>  [1] /home/runner/.cache/R/renv/library/aphantasiaReasoningViie-b75da44b/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu
    #>  [2] /home/runner/.cache/R/renv/sandbox/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/8f3cef43
    #> 
    #>  * ── Packages attached to the search path.
    #>  P ── Loaded and on-disk path mismatch.
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────
