# Strategy use analyses

This vignette contains a full breakdown of the analyses of the
self-reported (using Likert scales) mental strategies that participants
used to solve the reasoning problems. Only Bayesian analyses were
reported in the manuscript for brevity, but equivalent frequentist
analyses were also conducted to test the convergence of the two
approaches on similar results. They are reported in this vignette
alongside the Bayesian analyses for completeness.

``` r
library(aphantasiaReasoningViie)
#> Welcome to aphantasiaReasoningViie.
#> See https://osf.io/hfbcp/ for the associated study.
```

## Data preparation

First, let’s get the clean, analysis-ready and clustered data (see
[`vignette("preparing_data")`](https://m-delem.github.io/aphantasiaReasoningViie/articles/preparing_data.md)
and
[`vignette("osivq_clusters")`](https://m-delem.github.io/aphantasiaReasoningViie/articles/osivq_clusters.md)
for details).

``` r
df_survey <- get_clustered_data("survey") 
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
df_strats_long <- pivot_strategies_longer(df_survey, ordered = TRUE)

dplyr::glimpse(df_strats_long)
#> Rows: 520
#> Columns: 11
#> $ id             <fct> acdn247721443631359lzxb, acdn247721443631359lzxb, acdn2…
#> $ language       <fct> fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr, fr,…
#> $ age            <int> 24, 24, 24, 24, 24, 26, 26, 26, 26, 26, 23, 23, 23, 23,…
#> $ gender         <fct> f, f, f, f, f, f, f, f, f, f, m, m, m, m, m, f, f, f, f…
#> $ group_4        <fct> Typical, Typical, Typical, Typical, Typical, Aphantasia…
#> $ group_2        <fct> Typical, Typical, Typical, Typical, Typical, Aphantasia…
#> $ group_3        <fct> Typical, Typical, Typical, Typical, Typical, Aphantasia…
#> $ strategy_group <fct> No_visual_strategy, No_visual_strategy, No_visual_strat…
#> $ cluster        <fct> Visualiser, Visualiser, Visualiser, Visualiser, Visuali…
#> $ strategy       <fct> Visual, Verbal, Spatial, Semantic, Sensorimotor, Visual…
#> $ score          <ord> no_use, mainly_this_strat, secondary_strat, no_use, onl…
```

## Method

Ordinal cumulative link regression models were fitted, using the *brms*
package for Bayesian models (Bürkner, 2017) or the *ordinal* package
(Christensen, 2023) for frequentist models, to predict the score (on a
question about the use of a given strategy) with a grouping variable
(VVIQ groups, OSIVQ clusters), Strategy (visual, verbal, spatial,
semantic or sensorimotor) and their two-way interaction as fixed
categorical predictors. We planned to analyse the contrasts between
groups for each strategy separately.

Let’s break this down.

### Grouping variables

We used several grouping variables to classify participants, all of
which are in the `df_strats_long` data frame:

- `group_4` is the 4-group VVIQ classification with an “aphantasia”
  group (VVIQ = 16), “hypophantasia” group (VVIQ $\in$ \[17, 32\]),
  “typical” group (VVIQ $\in$ \[33, 74\]), and “hyperphantasia” group
  (VVIQ $\in$ \[75, 80\]). It was not used in the analyses because the
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

### Bayesian models

Bayesian models were fitted using the
[`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)
function through a custom wrapper,
[`fit_brms_model()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/fit_brms_model.md),
that sets several default options for us. 24000 post-warmup iterations
were spread across all available CPU for parallel processing[¹](#fn1),
with 2000 additional warmup iterations per chain. A fixed seed was used
for reproducibility. The [CmdStanR](https://mc-stan.org/cmdstanr/)
back-end was used for better performance. CmdStanR needs a special
installation command which is provided in the next chunk if needed.
Since the models were very simple and easy to fit, default priors were
used. To avoid having to refit the models each time the vignette is
built and improve reproducibility, fitted models are saved in the
`vignettes/models` folder and loaded in the R chunks.

After model fit, we checked for potential singularity issues and model
performance using the
[`get_singularity()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_singularity.md)
and
[`get_performance()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_performance.md)
functions created for the occasion (which are convenient wrappers around
the *performance* package). Posterior predictive checks were also
performed with the
[`performance::check_predictions()`](https://easystats.github.io/performance/reference/check_predictions.html)
function to check that the models were able to reproduce the data well.

Finally, we tested our hypotheses with marginal contrasts. This task was
performed with functions from the *marginaleffects* package. We used the
[`marginaleffects::avg_comparisons()`](https://marginaleffects.com/man/r/comparisons.html)
function to compute contrasts and a custom
[`report_rope()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/report_rope.md)
wrapper (around
[`marginaleffects::posterior_draws()`](https://marginaleffects.com/man/r/posterior_draws.html),
[`bayestestR::rope_range()`](https://easystats.github.io/bayestestR/reference/rope_range.html)
and
[`bayestestR::p_direction()`](https://easystats.github.io/bayestestR/reference/p_direction.html))
to summarise their posterior distributions. We computed the probability
of direction (PD) of the contrasts and the proportions of their
posterior distributions below, inside or above a region of practical
equivalence to the null (ROPE) (see Makowski et al., 2019). We computed
strategy use contrasts between groups for each strategy, contrasts
between strategies within each group, and interaction contrasts
(differences in strategy contrasts between groups).

The setup of the CmdStanR back-end and marginaleffects options is done
in the chunk below.

``` r
# if(!requireNamespace("cmdstanr", quietly = TRUE)) {
#   install.packages(
#     "cmdstanr",
#     repos = c('https://stan-dev.r-universe.dev', getOption("repos"))
#   )
# }
options("marginaleffects_safe" = FALSE)
draws <- seq(1, 10000, 1) # To limit draws that will be used for marginaleffects
```

### Frequentist models

Frequentist ordinal models were fitted using the
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
package[²](#fn2). We used it to compute strategy use contrasts between
groups for each strategy, contrasts between strategies within each
group, and interaction contrasts (differences in strategy contrasts
between groups).

Here we go!

## Results

### VVIQ 2 groups

#### Bayesian

``` r
mb_strats_vviq_2 <-
  fit_brms_model(
    formula = score ~ group_2 * strategy,
    data    = df_strats_long,
    family  = brms::cumulative(link = "probit"),
    file    = "models/m_strat_vviq_2.rds"
  )

# Singularity check
mb_strats_vviq_2 |> get_singularity()
# Model performance indices
mb_strats_vviq_2  |>
  get_performance(metrics = c("WAIC", "R2", "RMSE")) |>
  knitr::kable(align = "c")
```

|  WAIC  |  R2   |
|:------:|:-----:|
| 1269.7 | 0.353 |

``` r
# Posterior predictive check (best model performance indicator)
mb_strats_vviq_2 |> 
  performance::check_predictions() |> plot() + theme_pdf(base_size = 12)
```

![Posterior predictive
distributions.](analysing_strategies_files/figure-html/bayesian-vviq-2-groups-1.png)

``` r
# Group contrasts by strategy
mb_strats_vviq_2 |> 
  marginaleffects::avg_comparisons(
    variables = list("group_2" = "pairwise"), by = "strategy", 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(strategy, contrast) |> knitr::kable()
```

| strategy     | contrast             | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:-------------|:---------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Visual       | Typical - Aphantasia |    1.695 | \[1.213, 2.2\]    | 1.000 |      0.000 |       0.000 |      1.000 |
| Spatial      | Typical - Aphantasia |    0.403 | \[-0.035, 0.819\] | 0.965 |      0.013 |       0.070 |      0.917 |
| Verbal       | Typical - Aphantasia |   -0.268 | \[-0.68, 0.147\]  | 0.897 |      0.781 |       0.177 |      0.042 |
| Semantic     | Typical - Aphantasia |   -0.228 | \[-0.739, 0.278\] | 0.811 |      0.694 |       0.202 |      0.104 |
| Sensorimotor | Typical - Aphantasia |    0.137 | \[-0.286, 0.568\] | 0.731 |      0.139 |       0.293 |      0.568 |

``` r
# Strategy contrasts within groups
mb_strats_vviq_2 |> 
  marginaleffects::avg_comparisons(
    variables = list("strategy" = "pairwise"), by = "group_2", 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(group_2, contrast) |> knitr::kable()
```

| group_2    | contrast                | Estimate | 95% CI             |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:-----------|:------------------------|---------:|:-------------------|------:|-----------:|------------:|-----------:|
| Aphantasia | Semantic - Spatial      |   -0.778 | \[-1.265, -0.294\] | 0.999 |      0.996 |       0.003 |      0.001 |
| Aphantasia | Semantic - Verbal       |   -2.160 | \[-2.658, -1.665\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Aphantasia | Semantic - Visual       |    0.304 | \[-0.24, 0.87\]    | 0.862 |      0.072 |       0.161 |      0.767 |
| Aphantasia | Sensorimotor - Semantic |    0.655 | \[0.167, 1.138\]   | 0.996 |      0.001 |       0.012 |      0.987 |
| Aphantasia | Sensorimotor - Spatial  |   -0.123 | \[-0.583, 0.339\]  | 0.699 |      0.538 |       0.289 |      0.173 |
| Aphantasia | Sensorimotor - Verbal   |   -1.503 | \[-1.958, -1.06\]  | 1.000 |      1.000 |       0.000 |      0.000 |
| Aphantasia | Sensorimotor - Visual   |    0.961 | \[0.456, 1.49\]    | 1.000 |      0.000 |       0.001 |      0.999 |
| Aphantasia | Spatial - Visual        |    1.082 | \[0.572, 1.615\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Aphantasia | Verbal - Spatial        |    1.379 | \[0.928, 1.846\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Aphantasia | Verbal - Visual         |    2.462 | \[1.945, 2.999\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Typical    | Semantic - Spatial      |   -1.410 | \[-1.863, -0.96\]  | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical    | Semantic - Verbal       |   -2.124 | \[-2.595, -1.66\]  | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical    | Semantic - Visual       |   -1.621 | \[-2.067, -1.185\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical    | Sensorimotor - Semantic |    1.022 | \[0.572, 1.48\]    | 1.000 |      0.000 |       0.000 |      1.000 |
| Typical    | Sensorimotor - Spatial  |   -0.386 | \[-0.782, 0.016\]  | 0.971 |      0.922 |       0.070 |      0.008 |
| Typical    | Sensorimotor - Verbal   |   -1.097 | \[-1.513, -0.693\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical    | Sensorimotor - Visual   |   -0.602 | \[-0.991, -0.201\] | 0.999 |      0.993 |       0.006 |      0.000 |
| Typical    | Spatial - Visual        |   -0.211 | \[-0.613, 0.169\]  | 0.857 |      0.712 |       0.234 |      0.054 |
| Typical    | Verbal - Spatial        |    0.710 | \[0.321, 1.118\]   | 1.000 |      0.000 |       0.001 |      0.999 |
| Typical    | Verbal - Visual         |    0.501 | \[0.115, 0.887\]   | 0.995 |      0.001 |       0.019 |      0.980 |

``` r
# Interaction contrasts
mb_strats_vviq_2 |> 
  marginaleffects::avg_comparisons(
    variables = list("strategy" = "pairwise"),
    by = "group_2",
    hypothesis = ~revpairwise, # for the interaction
    type = "link",
    draw_ids = draws
  ) |> 
  report_rope(hypothesis) |> knitr::kable()
```

| Category contrast       | Grouping contrast    | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:------------------------|:---------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Semantic - Spatial      | Aphantasia - Typical |    0.630 | \[-0.018, 1.289\] | 0.971 |      0.015 |       0.041 |      0.944 |
| Semantic - Verbal       | Aphantasia - Typical |   -0.034 | \[-0.705, 0.624\] | 0.543 |      0.422 |       0.236 |      0.342 |
| Semantic - Visual       | Aphantasia - Typical |    1.925 | \[1.231, 2.636\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Sensorimotor - Semantic | Aphantasia - Typical |   -0.363 | \[-1.044, 0.305\] | 0.864 |      0.788 |       0.129 |      0.083 |
| Sensorimotor - Spatial  | Aphantasia - Typical |    0.267 | \[-0.344, 0.852\] | 0.807 |      0.121 |       0.172 |      0.706 |
| Sensorimotor - Verbal   | Aphantasia - Typical |   -0.405 | \[-1.013, 0.207\] | 0.906 |      0.841 |       0.106 |      0.052 |
| Sensorimotor - Visual   | Aphantasia - Typical |    1.556 | \[0.918, 2.215\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Spatial - Visual        | Aphantasia - Typical |    1.294 | \[0.669, 1.949\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Verbal - Spatial        | Aphantasia - Typical |    0.671 | \[0.076, 1.272\]  | 0.986 |      0.007 |       0.023 |      0.970 |
| Verbal - Visual         | Aphantasia - Typical |    1.960 | \[1.339, 2.629\]  | 1.000 |      0.000 |       0.000 |      1.000 |

#### Frequentist

``` r
mf_strats_vviq_2 <- fit_clm(score ~ group_2 * strategy, df_strats_long)

# Singularity
mf_strats_vviq_2 |> get_singularity()

# Performance
mf_strats_vviq_2 |> get_performance() |> knitr::kable(align = "c")
```

|  AIC   |  BIC   | RMSE  |
|:------:|:------:|:-----:|
| 1268.9 | 1324.2 | 2.400 |

``` r
# Group contrasts by strategy
mf_strats_vviq_2 |> report_contrast(~ group_2 | strategy) |> knitr::kable()
```

| Contrast             | Strategy     | Difference | 95% CI          | p.value |
|:---------------------|:-------------|-----------:|:----------------|--------:|
| Typical - Aphantasia | Visual       |      1.689 | \[1.2, 2.18\]   |   0.000 |
| Typical - Aphantasia | Spatial      |      0.397 | \[-0.03, 0.82\] |   0.068 |
| Typical - Aphantasia | Verbal       |     -0.267 | \[-0.68, 0.14\] |   0.203 |
| Typical - Aphantasia | Semantic     |     -0.229 | \[-0.74, 0.28\] |   0.379 |
| Typical - Aphantasia | Sensorimotor |      0.137 | \[-0.29, 0.57\] |   0.531 |

``` r
# Strategy contrasts within groups
mf_strats_vviq_2 |> report_contrast(~ strategy | group_2) |> knitr::kable()
```

| Contrast                | group_2    | Difference | 95% CI           | p.value |
|:------------------------|:-----------|-----------:|:-----------------|--------:|
| Spatial - Visual        | Aphantasia |      1.078 | \[0.36, 1.8\]    |   0.000 |
| Verbal - Visual         | Aphantasia |      2.454 | \[1.73, 3.18\]   |   0.000 |
| Verbal - Spatial        | Aphantasia |      1.376 | \[0.75, 2\]      |   0.000 |
| Semantic - Visual       | Aphantasia |      0.302 | \[-0.46, 1.06\]  |   0.814 |
| Semantic - Spatial      | Aphantasia |     -0.775 | \[-1.46, -0.09\] |   0.017 |
| Semantic - Verbal       | Aphantasia |     -2.152 | \[-2.84, -1.47\] |   0.000 |
| Sensorimotor - Visual   | Aphantasia |      0.953 | \[0.24, 1.67\]   |   0.003 |
| Sensorimotor - Spatial  | Aphantasia |     -0.125 | \[-0.76, 0.51\]  |   0.983 |
| Sensorimotor - Verbal   | Aphantasia |     -1.501 | \[-2.13, -0.87\] |   0.000 |
| Sensorimotor - Semantic | Aphantasia |      0.651 | \[-0.03, 1.33\]  |   0.069 |
| Spatial - Visual        | Typical    |     -0.214 | \[-0.76, 0.33\]  |   0.822 |
| Verbal - Visual         | Typical    |      0.498 | \[-0.04, 1.04\]  |   0.088 |
| Verbal - Spatial        | Typical    |      0.713 | \[0.17, 1.26\]   |   0.004 |
| Semantic - Visual       | Typical    |     -1.616 | \[-2.25, -0.98\] |   0.000 |
| Semantic - Spatial      | Typical    |     -1.401 | \[-2.03, -0.77\] |   0.000 |
| Semantic - Verbal       | Typical    |     -2.114 | \[-2.75, -1.48\] |   0.000 |
| Sensorimotor - Visual   | Typical    |     -0.599 | \[-1.15, -0.05\] |   0.026 |
| Sensorimotor - Spatial  | Typical    |     -0.385 | \[-0.94, 0.17\]  |   0.323 |
| Sensorimotor - Verbal   | Typical    |     -1.097 | \[-1.65, -0.54\] |   0.000 |
| Sensorimotor - Semantic | Typical    |      1.017 | \[0.38, 1.65\]   |   0.000 |

``` r
# Interaction contrasts
mf_strats_vviq_2 |> report_contrast(~ group_2 * strategy, interaction = TRUE) |>
  knitr::kable()
```

| group_2_revpairwise  | strategy_revpairwise    | Difference | 95% CI           | p.value |
|:---------------------|:------------------------|-----------:|:-----------------|--------:|
| Typical - Aphantasia | Spatial - Visual        |     -1.292 | \[-1.94, -0.64\] |   0.000 |
| Typical - Aphantasia | Verbal - Visual         |     -1.956 | \[-2.6, -1.31\]  |   0.000 |
| Typical - Aphantasia | Verbal - Spatial        |     -0.664 | \[-1.26, -0.07\] |   0.028 |
| Typical - Aphantasia | Semantic - Visual       |     -1.918 | \[-2.63, -1.21\] |   0.000 |
| Typical - Aphantasia | Semantic - Spatial      |     -0.626 | \[-1.29, 0.04\]  |   0.065 |
| Typical - Aphantasia | Semantic - Verbal       |      0.038 | \[-0.62, 0.69\]  |   0.910 |
| Typical - Aphantasia | Sensorimotor - Visual   |     -1.552 | \[-2.2, -0.9\]   |   0.000 |
| Typical - Aphantasia | Sensorimotor - Spatial  |     -0.260 | \[-0.86, 0.34\]  |   0.399 |
| Typical - Aphantasia | Sensorimotor - Verbal   |      0.404 | \[-0.19, 1\]     |   0.182 |
| Typical - Aphantasia | Sensorimotor - Semantic |      0.366 | \[-0.3, 1.03\]   |   0.281 |

### VVIQ 3 groups

#### Bayesian

``` r
mb_strats_vviq_3 <-
  fit_brms_model(
    formula = score ~ group_3 * strategy,
    data    = df_strats_long,
    family  = brms::cumulative(link = "probit"),
    file    = "models/m_strat_vviq_3.rds"
  )

# Singularity check
mb_strats_vviq_3 |> get_singularity()
# Model performance indices
mb_strats_vviq_3  |>
  get_performance(metrics = c("WAIC", "R2", "RMSE")) |>
  knitr::kable(align = "c")
```

|  WAIC  |  R2   |
|:------:|:-----:|
| 1266.0 | 0.362 |

``` r
# Posterior predictive check (best model performance indicator)
mb_strats_vviq_3 |> 
  performance::check_predictions() |> plot() + theme_pdf(base_size = 12)
```

![Posterior predictive
distributions.](analysing_strategies_files/figure-html/bayesian-vviq-3-groups-1.png)

``` r
# Group contrasts by strategy
mb_strats_vviq_3 |> 
  marginaleffects::avg_comparisons(
    variables = list("group_3" = "pairwise"), by = "strategy", 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(strategy, contrast) |> knitr::kable()
```

| strategy     | contrast                   | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:-------------|:---------------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Visual       | Hypophantasia - Aphantasia |    1.699 | \[0.7, 2.886\]    | 1.000 |      0.000 |       0.000 |      1.000 |
| Visual       | Typical - Aphantasia       |    2.734 | \[1.882, 3.849\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Visual       | Typical - Hypophantasia    |    1.042 | \[0.437, 1.663\]  | 1.000 |      0.000 |       0.001 |      0.999 |
| Spatial      | Hypophantasia - Aphantasia |   -0.600 | \[-1.318, 0.076\] | 0.959 |      0.930 |       0.049 |      0.021 |
| Spatial      | Typical - Aphantasia       |    0.199 | \[-0.277, 0.677\] | 0.800 |      0.110 |       0.232 |      0.658 |
| Spatial      | Typical - Hypophantasia    |    0.807 | \[0.182, 1.45\]   | 0.994 |      0.002 |       0.011 |      0.987 |
| Verbal       | Hypophantasia - Aphantasia |    0.094 | \[-0.549, 0.73\]  | 0.606 |      0.284 |       0.222 |      0.494 |
| Verbal       | Typical - Aphantasia       |   -0.232 | \[-0.708, 0.237\] | 0.840 |      0.719 |       0.198 |      0.083 |
| Verbal       | Typical - Hypophantasia    |   -0.328 | \[-0.901, 0.256\] | 0.868 |      0.784 |       0.143 |      0.074 |
| Semantic     | Hypophantasia - Aphantasia |    0.199 | \[-0.564, 0.94\]  | 0.699 |      0.217 |       0.183 |      0.600 |
| Semantic     | Typical - Aphantasia       |   -0.154 | \[-0.732, 0.431\] | 0.692 |      0.571 |       0.226 |      0.202 |
| Semantic     | Typical - Hypophantasia    |   -0.353 | \[-1.03, 0.353\]  | 0.841 |      0.762 |       0.137 |      0.101 |
| Sensorimotor | Hypophantasia - Aphantasia |   -0.009 | \[-0.68, 0.653\]  | 0.510 |      0.391 |       0.238 |      0.370 |
| Sensorimotor | Typical - Aphantasia       |    0.140 | \[-0.352, 0.627\] | 0.708 |      0.171 |       0.265 |      0.564 |
| Sensorimotor | Typical - Hypophantasia    |    0.147 | \[-0.465, 0.75\]  | 0.683 |      0.211 |       0.229 |      0.560 |

``` r
# Strategy contrasts within groups
mb_strats_vviq_3 |> 
  marginaleffects::avg_comparisons(
    variables = list("strategy" = "pairwise"), by = "group_3", 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(group_3, contrast) |> knitr::kable()
```

| group_3       | contrast                | Estimate | 95% CI             |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:--------------|:------------------------|---------:|:-------------------|------:|-----------:|------------:|-----------:|
| Aphantasia    | Semantic - Spatial      |   -1.067 | \[-1.677, -0.464\] | 1.000 |      0.999 |       0.001 |      0.000 |
| Aphantasia    | Semantic - Verbal       |   -2.217 | \[-2.826, -1.619\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Aphantasia    | Semantic - Visual       |    1.261 | \[0.311, 2.422\]   | 0.996 |      0.002 |       0.005 |      0.993 |
| Aphantasia    | Sensorimotor - Semantic |    0.741 | \[0.121, 1.359\]   | 0.990 |      0.003 |       0.018 |      0.979 |
| Aphantasia    | Sensorimotor - Spatial  |   -0.326 | \[-0.874, 0.241\]  | 0.870 |      0.783 |       0.149 |      0.068 |
| Aphantasia    | Sensorimotor - Verbal   |   -1.482 | \[-2.043, -0.907\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Aphantasia    | Sensorimotor - Visual   |    2.005 | \[1.072, 3.14\]    | 1.000 |      0.000 |       0.000 |      1.000 |
| Aphantasia    | Spatial - Visual        |    2.323 | \[1.411, 3.456\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Aphantasia    | Verbal - Spatial        |    1.152 | \[0.603, 1.695\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Aphantasia    | Verbal - Visual         |    3.479 | \[2.553, 4.609\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Hypophantasia | Semantic - Spatial      |   -0.265 | \[-1.082, 0.565\]  | 0.741 |      0.657 |       0.150 |      0.193 |
| Hypophantasia | Semantic - Verbal       |   -2.110 | \[-2.923, -1.323\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Hypophantasia | Semantic - Visual       |   -0.242 | \[-1.042, 0.569\]  | 0.722 |      0.636 |       0.159 |      0.204 |
| Hypophantasia | Sensorimotor - Semantic |    0.533 | \[-0.264, 1.346\]  | 0.907 |      0.060 |       0.081 |      0.859 |
| Hypophantasia | Sensorimotor - Spatial  |    0.269 | \[-0.506, 1.04\]   | 0.757 |      0.172 |       0.158 |      0.670 |
| Hypophantasia | Sensorimotor - Verbal   |   -1.576 | \[-2.325, -0.839\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Hypophantasia | Sensorimotor - Visual   |    0.299 | \[-0.454, 1.058\]  | 0.771 |      0.159 |       0.153 |      0.688 |
| Hypophantasia | Spatial - Visual        |    0.026 | \[-0.774, 0.813\]  | 0.525 |      0.381 |       0.192 |      0.427 |
| Hypophantasia | Verbal - Spatial        |    1.844 | \[1.084, 2.627\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Hypophantasia | Verbal - Visual         |    1.875 | \[1.115, 2.62\]    | 1.000 |      0.000 |       0.000 |      1.000 |
| Typical       | Semantic - Spatial      |   -1.416 | \[-1.867, -0.976\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical       | Semantic - Verbal       |   -2.133 | \[-2.599, -1.678\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical       | Semantic - Visual       |   -1.636 | \[-2.091, -1.178\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical       | Sensorimotor - Semantic |    1.031 | \[0.589, 1.481\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Typical       | Sensorimotor - Spatial  |   -0.386 | \[-0.786, 0.015\]  | 0.970 |      0.917 |       0.074 |      0.009 |
| Typical       | Sensorimotor - Verbal   |   -1.106 | \[-1.507, -0.705\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical       | Sensorimotor - Visual   |   -0.603 | \[-0.998, -0.203\] | 0.999 |      0.994 |       0.006 |      0.000 |
| Typical       | Spatial - Visual        |   -0.217 | \[-0.609, 0.173\]  | 0.860 |      0.718 |       0.227 |      0.055 |
| Typical       | Verbal - Spatial        |    0.718 | \[0.33, 1.104\]    | 1.000 |      0.000 |       0.001 |      0.999 |
| Typical       | Verbal - Visual         |    0.501 | \[0.109, 0.883\]   | 0.995 |      0.001 |       0.021 |      0.978 |

``` r
# Interaction contrasts
mb_strats_vviq_3 |> 
  marginaleffects::avg_comparisons(
    variables = list("strategy" = "pairwise"),
    by = "group_3",
    hypothesis = ~revpairwise, # for the interaction
    type = "link",
    draw_ids = draws
  ) |> 
  report_rope(hypothesis) |> knitr::kable()
```

| Category contrast       | Grouping contrast          | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:------------------------|:---------------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Semantic - Spatial      | Aphantasia - Hypophantasia |   -0.800 | \[-1.833, 0.229\] | 0.937 |      0.910 |       0.047 |      0.043 |
| Semantic - Verbal       | Aphantasia - Hypophantasia |   -0.113 | \[-1.084, 0.884\] | 0.583 |      0.510 |       0.150 |      0.340 |
| Semantic - Visual       | Aphantasia - Hypophantasia |    1.517 | \[0.228, 2.906\]  | 0.990 |      0.007 |       0.008 |      0.985 |
| Sensorimotor - Semantic | Aphantasia - Hypophantasia |    0.211 | \[-0.809, 1.222\] | 0.657 |      0.275 |       0.138 |      0.587 |
| Sensorimotor - Spatial  | Aphantasia - Hypophantasia |   -0.596 | \[-1.553, 0.365\] | 0.889 |      0.848 |       0.077 |      0.075 |
| Sensorimotor - Verbal   | Aphantasia - Hypophantasia |    0.105 | \[-0.837, 1.017\] | 0.586 |      0.334 |       0.162 |      0.504 |
| Sensorimotor - Visual   | Aphantasia - Hypophantasia |    1.730 | \[0.496, 3.072\]  | 0.997 |      0.002 |       0.003 |      0.995 |
| Spatial - Visual        | Aphantasia - Hypophantasia |    2.313 | \[1.074, 3.675\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Verbal - Spatial        | Aphantasia - Hypophantasia |   -0.691 | \[-1.655, 0.233\] | 0.928 |      0.895 |       0.059 |      0.046 |
| Verbal - Visual         | Aphantasia - Hypophantasia |    1.618 | \[0.402, 2.967\]  | 0.996 |      0.002 |       0.004 |      0.994 |
| Semantic - Spatial      | Aphantasia - Typical       |    0.349 | \[-0.391, 1.111\] | 0.814 |      0.121 |       0.136 |      0.742 |
| Semantic - Verbal       | Aphantasia - Typical       |   -0.084 | \[-0.825, 0.668\] | 0.587 |      0.484 |       0.201 |      0.315 |
| Semantic - Visual       | Aphantasia - Typical       |    2.896 | \[1.836, 4.144\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Sensorimotor - Semantic | Aphantasia - Typical       |   -0.286 | \[-1.059, 0.46\]  | 0.770 |      0.684 |       0.159 |      0.157 |
| Sensorimotor - Spatial  | Aphantasia - Typical       |    0.056 | \[-0.622, 0.761\] | 0.565 |      0.324 |       0.224 |      0.452 |
| Sensorimotor - Verbal   | Aphantasia - Typical       |   -0.375 | \[-1.047, 0.309\] | 0.856 |      0.784 |       0.130 |      0.086 |
| Sensorimotor - Visual   | Aphantasia - Typical       |    2.605 | \[1.598, 3.831\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Spatial - Visual        | Aphantasia - Typical       |    2.543 | \[1.528, 3.742\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Verbal - Spatial        | Aphantasia - Typical       |    0.438 | \[-0.237, 1.11\]  | 0.898 |      0.060 |       0.104 |      0.836 |
| Verbal - Visual         | Aphantasia - Typical       |    2.987 | \[1.98, 4.177\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Semantic - Spatial      | Hypophantasia - Typical    |    1.151 | \[0.207, 2.11\]   | 0.993 |      0.004 |       0.010 |      0.986 |
| Semantic - Verbal       | Hypophantasia - Typical    |    0.026 | \[-0.9, 0.934\]   | 0.523 |      0.391 |       0.174 |      0.434 |
| Semantic - Visual       | Hypophantasia - Typical    |    1.388 | \[0.473, 2.324\]  | 0.999 |      0.001 |       0.002 |      0.997 |
| Sensorimotor - Semantic | Hypophantasia - Typical    |   -0.499 | \[-1.413, 0.437\] | 0.859 |      0.806 |       0.090 |      0.103 |
| Sensorimotor - Spatial  | Hypophantasia - Typical    |    0.654 | \[-0.215, 1.544\] | 0.932 |      0.043 |       0.058 |      0.899 |
| Sensorimotor - Verbal   | Hypophantasia - Typical    |   -0.478 | \[-1.296, 0.367\] | 0.866 |      0.812 |       0.098 |      0.090 |
| Sensorimotor - Visual   | Hypophantasia - Typical    |    0.896 | \[0.05, 1.765\]   | 0.981 |      0.011 |       0.021 |      0.968 |
| Spatial - Visual        | Hypophantasia - Typical    |    0.240 | \[-0.649, 1.117\] | 0.698 |      0.230 |       0.153 |      0.617 |
| Verbal - Spatial        | Hypophantasia - Typical    |    1.130 | \[0.286, 2.007\]  | 0.996 |      0.003 |       0.006 |      0.991 |
| Verbal - Visual         | Hypophantasia - Typical    |    1.372 | \[0.524, 2.202\]  | 0.999 |      0.000 |       0.001 |      0.998 |

#### Frequentist

``` r
mf_strats_vviq_3 <- fit_clm(score ~ group_3 * strategy, df_strats_long)

mf_strats_vviq_3 |> get_singularity()

mf_strats_vviq_3 |> get_performance() |> knitr::kable(align = "c")
```

|  AIC   |  BIC   | RMSE  |
|:------:|:------:|:-----:|
| 1263.7 | 1340.3 | 2.399 |

``` r
mf_strats_vviq_3 |> report_contrast(~ group_3 | strategy) |> knitr::kable()
```

| Contrast                   | Strategy     | Difference | 95% CI          | p.value |
|:---------------------------|:-------------|-----------:|:----------------|--------:|
| Hypophantasia - Aphantasia | Visual       |      1.606 | \[0.37, 2.84\]  |   0.006 |
| Typical - Aphantasia       | Visual       |      2.643 | \[1.54, 3.74\]  |   0.000 |
| Typical - Hypophantasia    | Visual       |      1.036 | \[0.3, 1.77\]   |   0.003 |
| Hypophantasia - Aphantasia | Spatial      |     -0.598 | \[-1.42, 0.23\] |   0.205 |
| Typical - Aphantasia       | Spatial      |      0.197 | \[-0.38, 0.77\] |   0.703 |
| Typical - Hypophantasia    | Spatial      |      0.795 | \[0.04, 1.55\]  |   0.036 |
| Hypophantasia - Aphantasia | Verbal       |      0.093 | \[-0.67, 0.85\] |   0.956 |
| Typical - Aphantasia       | Verbal       |     -0.234 | \[-0.8, 0.33\]  |   0.595 |
| Typical - Hypophantasia    | Verbal       |     -0.327 | \[-1.02, 0.36\] |   0.507 |
| Hypophantasia - Aphantasia | Semantic     |      0.202 | \[-0.7, 1.11\]  |   0.860 |
| Typical - Aphantasia       | Semantic     |     -0.154 | \[-0.86, 0.55\] |   0.866 |
| Typical - Hypophantasia    | Semantic     |     -0.356 | \[-1.18, 0.47\] |   0.570 |
| Hypophantasia - Aphantasia | Sensorimotor |     -0.004 | \[-0.8, 0.8\]   |   1.000 |
| Typical - Aphantasia       | Sensorimotor |      0.137 | \[-0.45, 0.72\] |   0.849 |
| Typical - Hypophantasia    | Sensorimotor |      0.140 | \[-0.59, 0.87\] |   0.893 |

``` r
mf_strats_vviq_3 |> report_contrast(~ strategy | group_3) |> knitr::kable()
```

| Contrast                | group_3       | Difference | 95% CI           | p.value |
|:------------------------|:--------------|-----------:|:-----------------|--------:|
| Spatial - Visual        | Aphantasia    |      2.230 | \[0.89, 3.57\]   |   0.000 |
| Verbal - Visual         | Aphantasia    |      3.378 | \[2.04, 4.72\]   |   0.000 |
| Verbal - Spatial        | Aphantasia    |      1.148 | \[0.38, 1.92\]   |   0.000 |
| Semantic - Visual       | Aphantasia    |      1.173 | \[-0.21, 2.56\]  |   0.140 |
| Semantic - Spatial      | Aphantasia    |     -1.057 | \[-1.91, -0.2\]  |   0.007 |
| Semantic - Verbal       | Aphantasia    |     -2.205 | \[-3.07, -1.34\] |   0.000 |
| Sensorimotor - Visual   | Aphantasia    |      1.904 | \[0.56, 3.24\]   |   0.001 |
| Sensorimotor - Spatial  | Aphantasia    |     -0.326 | \[-1.11, 0.46\]  |   0.787 |
| Sensorimotor - Verbal   | Aphantasia    |     -1.474 | \[-2.26, -0.69\] |   0.000 |
| Sensorimotor - Semantic | Aphantasia    |      0.731 | \[-0.13, 1.59\]  |   0.140 |
| Spatial - Visual        | Hypophantasia |      0.025 | \[-1.07, 1.12\]  |   1.000 |
| Verbal - Visual         | Hypophantasia |      1.865 | \[0.82, 2.91\]   |   0.000 |
| Verbal - Spatial        | Hypophantasia |      1.839 | \[0.77, 2.91\]   |   0.000 |
| Semantic - Visual       | Hypophantasia |     -0.231 | \[-1.35, 0.89\]  |   0.980 |
| Semantic - Spatial      | Hypophantasia |     -0.257 | \[-1.4, 0.88\]   |   0.973 |
| Semantic - Verbal       | Hypophantasia |     -2.096 | \[-3.2, -1\]     |   0.000 |
| Sensorimotor - Visual   | Hypophantasia |      0.294 | \[-0.77, 1.36\]  |   0.944 |
| Sensorimotor - Spatial  | Hypophantasia |      0.268 | \[-0.82, 1.35\]  |   0.962 |
| Sensorimotor - Verbal   | Hypophantasia |     -1.571 | \[-2.61, -0.53\] |   0.000 |
| Sensorimotor - Semantic | Hypophantasia |      0.525 | \[-0.59, 1.64\]  |   0.699 |
| Spatial - Visual        | Typical       |     -0.216 | \[-0.76, 0.33\]  |   0.818 |
| Verbal - Visual         | Typical       |      0.501 | \[-0.04, 1.04\]  |   0.085 |
| Verbal - Spatial        | Typical       |      0.717 | \[0.17, 1.26\]   |   0.003 |
| Semantic - Visual       | Typical       |     -1.624 | \[-2.25, -0.99\] |   0.000 |
| Semantic - Spatial      | Typical       |     -1.408 | \[-2.04, -0.78\] |   0.000 |
| Semantic - Verbal       | Typical       |     -2.125 | \[-2.76, -1.49\] |   0.000 |
| Sensorimotor - Visual   | Typical       |     -0.602 | \[-1.15, -0.05\] |   0.024 |
| Sensorimotor - Spatial  | Typical       |     -0.386 | \[-0.94, 0.17\]  |   0.319 |
| Sensorimotor - Verbal   | Typical       |     -1.103 | \[-1.66, -0.55\] |   0.000 |
| Sensorimotor - Semantic | Typical       |      1.021 | \[0.39, 1.65\]   |   0.000 |

``` r
mf_strats_vviq_3 |> report_contrast(~ group_3 * strategy, interaction = TRUE) |>
  knitr::kable()
```

| group_3_revpairwise        | strategy_revpairwise    | Difference | 95% CI           | p.value |
|:---------------------------|:------------------------|-----------:|:-----------------|--------:|
| Hypophantasia - Aphantasia | Spatial - Visual        |     -2.204 | \[-3.44, -0.96\] |   0.000 |
| Typical - Aphantasia       | Spatial - Visual        |     -2.446 | \[-3.48, -1.41\] |   0.000 |
| Typical - Hypophantasia    | Spatial - Visual        |     -0.241 | \[-1.12, 0.64\]  |   0.590 |
| Hypophantasia - Aphantasia | Verbal - Visual         |     -1.513 | \[-2.72, -0.3\]  |   0.014 |
| Typical - Aphantasia       | Verbal - Visual         |     -2.877 | \[-3.91, -1.84\] |   0.000 |
| Typical - Hypophantasia    | Verbal - Visual         |     -1.363 | \[-2.21, -0.52\] |   0.002 |
| Hypophantasia - Aphantasia | Verbal - Spatial        |      0.691 | \[-0.25, 1.63\]  |   0.149 |
| Typical - Aphantasia       | Verbal - Spatial        |     -0.431 | \[-1.11, 0.24\]  |   0.211 |
| Typical - Hypophantasia    | Verbal - Spatial        |     -1.122 | \[-1.98, -0.27\] |   0.010 |
| Hypophantasia - Aphantasia | Semantic - Visual       |     -1.404 | \[-2.68, -0.13\] |   0.031 |
| Typical - Aphantasia       | Semantic - Visual       |     -2.796 | \[-3.89, -1.7\]  |   0.000 |
| Typical - Hypophantasia    | Semantic - Visual       |     -1.392 | \[-2.32, -0.47\] |   0.003 |
| Hypophantasia - Aphantasia | Semantic - Spatial      |      0.800 | \[-0.22, 1.82\]  |   0.126 |
| Typical - Aphantasia       | Semantic - Spatial      |     -0.351 | \[-1.11, 0.41\]  |   0.367 |
| Typical - Hypophantasia    | Semantic - Spatial      |     -1.151 | \[-2.09, -0.21\] |   0.016 |
| Hypophantasia - Aphantasia | Semantic - Verbal       |      0.109 | \[-0.88, 1.1\]   |   0.829 |
| Typical - Aphantasia       | Semantic - Verbal       |      0.080 | \[-0.67, 0.83\]  |   0.835 |
| Typical - Hypophantasia    | Semantic - Verbal       |     -0.029 | \[-0.93, 0.87\]  |   0.950 |
| Hypophantasia - Aphantasia | Sensorimotor - Visual   |     -1.610 | \[-2.84, -0.38\] |   0.010 |
| Typical - Aphantasia       | Sensorimotor - Visual   |     -2.506 | \[-3.55, -1.46\] |   0.000 |
| Typical - Hypophantasia    | Sensorimotor - Visual   |     -0.896 | \[-1.76, -0.03\] |   0.042 |
| Hypophantasia - Aphantasia | Sensorimotor - Spatial  |      0.594 | \[-0.37, 1.56\]  |   0.226 |
| Typical - Aphantasia       | Sensorimotor - Spatial  |     -0.061 | \[-0.75, 0.63\]  |   0.863 |
| Typical - Hypophantasia    | Sensorimotor - Spatial  |     -0.655 | \[-1.53, 0.22\]  |   0.143 |
| Hypophantasia - Aphantasia | Sensorimotor - Verbal   |     -0.097 | \[-1.02, 0.83\]  |   0.837 |
| Typical - Aphantasia       | Sensorimotor - Verbal   |      0.371 | \[-0.31, 1.05\]  |   0.286 |
| Typical - Hypophantasia    | Sensorimotor - Verbal   |      0.467 | \[-0.37, 1.3\]   |   0.274 |
| Hypophantasia - Aphantasia | Sensorimotor - Semantic |     -0.206 | \[-1.22, 0.8\]   |   0.690 |
| Typical - Aphantasia       | Sensorimotor - Semantic |      0.290 | \[-0.48, 1.06\]  |   0.458 |
| Typical - Hypophantasia    | Sensorimotor - Semantic |      0.496 | \[-0.42, 1.42\]  |   0.290 |

### OSIVQ 3 clusters

#### Bayesian

``` r
mb_strats_osivq <-
  fit_brms_model(
    formula = score ~ cluster * strategy,
    data    = df_strats_long,
    family  = brms::cumulative(link = "probit"),
    file    = "models/m_strat_osivq.rds"
  )

# Singularity check
mb_strats_osivq |> get_singularity()
# Model performance indices
mb_strats_osivq  |>
  get_performance(metrics = c("WAIC", "R2", "RMSE")) |>
  knitr::kable(align = "c")
```

|  WAIC  |  R2   |
|:------:|:-----:|
| 1293.3 | 0.341 |

``` r
# Posterior predictive check (best model performance indicator)
mb_strats_osivq |> 
  performance::check_predictions() |> plot() + theme_pdf(base_size = 12)
```

![Posterior predictive
distributions.](analysing_strategies_files/figure-html/bayesian-osivq-3-clusters-1.png)

``` r
# Group contrasts by strategy
mb_strats_osivq |> 
  marginaleffects::avg_comparisons(
    variables = list("cluster" = "pairwise"), by = "strategy", 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(strategy, contrast) |> knitr::kable()
```

| strategy     | contrast                 | Estimate | 95% CI             |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:-------------|:-------------------------|---------:|:-------------------|------:|-----------:|------------:|-----------:|
| Visual       | Spatialiser - Visualiser |   -0.824 | \[-1.435, -0.227\] | 0.997 |      0.992 |       0.007 |      0.001 |
| Visual       | Verbaliser - Spatialiser |   -0.708 | \[-1.367, -0.053\] | 0.983 |      0.964 |       0.028 |      0.008 |
| Visual       | Verbaliser - Visualiser  |   -1.531 | \[-2.055, -1.017\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Spatial      | Spatialiser - Visualiser |   -0.047 | \[-0.632, 0.554\]  | 0.564 |      0.429 |       0.261 |      0.310 |
| Spatial      | Verbaliser - Spatialiser |   -0.279 | \[-0.878, 0.306\]  | 0.831 |      0.732 |       0.172 |      0.097 |
| Spatial      | Verbaliser - Visualiser  |   -0.328 | \[-0.796, 0.142\]  | 0.920 |      0.833 |       0.132 |      0.035 |
| Verbal       | Spatialiser - Visualiser |    0.388 | \[-0.171, 0.956\]  | 0.912 |      0.043 |       0.114 |      0.843 |
| Verbal       | Verbaliser - Spatialiser |    0.020 | \[-0.558, 0.587\]  | 0.527 |      0.340 |       0.272 |      0.388 |
| Verbal       | Verbaliser - Visualiser  |    0.407 | \[-0.04, 0.859\]   | 0.962 |      0.013 |       0.079 |      0.908 |
| Semantic     | Spatialiser - Visualiser |   -0.150 | \[-0.908, 0.571\]  | 0.656 |      0.559 |       0.194 |      0.247 |
| Semantic     | Verbaliser - Spatialiser |    0.192 | \[-0.521, 0.957\]  | 0.697 |      0.213 |       0.186 |      0.600 |
| Semantic     | Verbaliser - Visualiser  |    0.045 | \[-0.539, 0.602\]  | 0.564 |      0.303 |       0.275 |      0.422 |
| Sensorimotor | Spatialiser - Visualiser |   -0.105 | \[-0.712, 0.492\]  | 0.640 |      0.507 |       0.244 |      0.248 |
| Sensorimotor | Verbaliser - Spatialiser |   -0.160 | \[-0.761, 0.429\]  | 0.700 |      0.577 |       0.227 |      0.196 |
| Sensorimotor | Verbaliser - Visualiser  |   -0.267 | \[-0.744, 0.209\]  | 0.871 |      0.763 |       0.174 |      0.063 |

``` r
# Strategy contrasts within groups
mb_strats_osivq |> 
  marginaleffects::avg_comparisons(
    variables = list("strategy" = "pairwise"), by = "cluster", 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(cluster, contrast) |> knitr::kable()
```

| cluster     | contrast                | Estimate | 95% CI             |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:------------|:------------------------|---------:|:-------------------|------:|-----------:|------------:|-----------:|
| Visualiser  | Semantic - Spatial      |   -1.253 | \[-1.769, -0.749\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Visualiser  | Semantic - Verbal       |   -1.884 | \[-2.388, -1.378\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Visualiser  | Semantic - Visual       |   -1.555 | \[-2.068, -1.057\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Visualiser  | Sensorimotor - Semantic |    0.968 | \[0.462, 1.487\]   | 1.000 |      0.000 |       0.001 |      0.999 |
| Visualiser  | Sensorimotor - Spatial  |   -0.286 | \[-0.755, 0.175\]  | 0.888 |      0.790 |       0.159 |      0.051 |
| Visualiser  | Sensorimotor - Verbal   |   -0.914 | \[-1.36, -0.467\]  | 1.000 |      1.000 |       0.000 |      0.000 |
| Visualiser  | Sensorimotor - Visual   |   -0.585 | \[-1.045, -0.132\] | 0.993 |      0.982 |       0.016 |      0.002 |
| Visualiser  | Spatial - Visual        |   -0.299 | \[-0.743, 0.149\]  | 0.902 |      0.811 |       0.146 |      0.043 |
| Visualiser  | Verbal - Spatial        |    0.626 | \[0.172, 1.074\]   | 0.996 |      0.001 |       0.010 |      0.989 |
| Visualiser  | Verbal - Visual         |    0.331 | \[-0.123, 0.778\]  | 0.921 |      0.033 |       0.123 |      0.843 |
| Spatialiser | Semantic - Spatial      |   -1.365 | \[-2.163, -0.584\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Spatialiser | Semantic - Verbal       |   -2.428 | \[-3.23, -1.667\]  | 1.000 |      1.000 |       0.000 |      0.000 |
| Spatialiser | Semantic - Visual       |   -0.880 | \[-1.702, -0.085\] | 0.985 |      0.972 |       0.021 |      0.007 |
| Spatialiser | Sensorimotor - Semantic |    1.013 | \[0.235, 1.834\]   | 0.995 |      0.002 |       0.007 |      0.991 |
| Spatialiser | Sensorimotor - Spatial  |   -0.351 | \[-1.044, 0.332\]  | 0.835 |      0.757 |       0.141 |      0.101 |
| Spatialiser | Sensorimotor - Verbal   |   -1.417 | \[-2.093, -0.728\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Spatialiser | Sensorimotor - Visual   |    0.130 | \[-0.581, 0.855\]  | 0.641 |      0.268 |       0.198 |      0.534 |
| Spatialiser | Spatial - Visual        |    0.486 | \[-0.214, 1.177\]  | 0.911 |      0.050 |       0.101 |      0.849 |
| Spatialiser | Verbal - Spatial        |    1.068 | \[0.385, 1.741\]   | 0.999 |      0.001 |       0.002 |      0.997 |
| Spatialiser | Verbal - Visual         |    1.544 | \[0.844, 2.251\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Verbaliser  | Semantic - Spatial      |   -0.881 | \[-1.403, -0.363\] | 1.000 |      0.999 |       0.001 |      0.000 |
| Verbaliser  | Semantic - Verbal       |   -2.245 | \[-2.778, -1.731\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Verbaliser  | Semantic - Visual       |    0.017 | \[-0.554, 0.591\]  | 0.522 |      0.342 |       0.271 |      0.387 |
| Verbaliser  | Sensorimotor - Semantic |    0.656 | \[0.132, 1.196\]   | 0.992 |      0.003 |       0.018 |      0.980 |
| Verbaliser  | Sensorimotor - Spatial  |   -0.226 | \[-0.703, 0.253\]  | 0.826 |      0.702 |       0.212 |      0.086 |
| Verbaliser  | Sensorimotor - Verbal   |   -1.595 | \[-2.072, -1.118\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Verbaliser  | Sensorimotor - Visual   |    0.673 | \[0.152, 1.212\]   | 0.994 |      0.002 |       0.013 |      0.985 |
| Verbaliser  | Spatial - Visual        |    0.901 | \[0.38, 1.431\]    | 1.000 |      0.000 |       0.001 |      0.999 |
| Verbaliser  | Verbal - Spatial        |    1.368 | \[0.898, 1.833\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Verbaliser  | Verbal - Visual         |    2.268 | \[1.745, 2.804\]   | 1.000 |      0.000 |       0.000 |      1.000 |

``` r
# Interaction contrasts
mb_strats_osivq |> 
  marginaleffects::avg_comparisons(
    variables = list("strategy" = "pairwise"),
    by = "cluster",
    hypothesis = ~revpairwise, # for the interaction
    type = "link",
    draw_ids = draws
  ) |> 
  report_rope(hypothesis) |> knitr::kable()
```

| Category contrast       | Grouping contrast        | Estimate | 95% CI             |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:------------------------|:-------------------------|---------:|:-------------------|------:|-----------:|------------:|-----------:|
| Semantic - Spatial      | Spatialiser - Verbaliser |   -0.482 | \[-1.443, 0.447\]  | 0.847 |      0.790 |       0.102 |      0.108 |
| Semantic - Verbal       | Spatialiser - Verbaliser |   -0.172 | \[-1.115, 0.741\]  | 0.645 |      0.560 |       0.159 |      0.281 |
| Semantic - Visual       | Spatialiser - Verbaliser |   -0.901 | \[-1.906, 0.082\]  | 0.965 |      0.949 |       0.028 |      0.023 |
| Sensorimotor - Semantic | Spatialiser - Verbaliser |    0.357 | \[-0.56, 1.327\]   | 0.772 |      0.172 |       0.123 |      0.705 |
| Sensorimotor - Spatial  | Spatialiser - Verbaliser |   -0.122 | \[-0.961, 0.72\]   | 0.613 |      0.519 |       0.182 |      0.298 |
| Sensorimotor - Verbal   | Spatialiser - Verbaliser |    0.179 | \[-0.629, 0.998\]  | 0.662 |      0.253 |       0.173 |      0.574 |
| Sensorimotor - Visual   | Spatialiser - Verbaliser |   -0.541 | \[-1.409, 0.352\]  | 0.882 |      0.834 |       0.088 |      0.078 |
| Spatial - Visual        | Spatialiser - Verbaliser |   -0.418 | \[-1.304, 0.444\]  | 0.824 |      0.761 |       0.115 |      0.123 |
| Verbal - Spatial        | Spatialiser - Verbaliser |   -0.301 | \[-1.122, 0.517\]  | 0.768 |      0.686 |       0.147 |      0.166 |
| Verbal - Visual         | Spatialiser - Verbaliser |   -0.721 | \[-1.616, 0.146\]  | 0.951 |      0.921 |       0.048 |      0.031 |
| Semantic - Spatial      | Visualiser - Spatialiser |    0.107 | \[-0.827, 1.064\]  | 0.589 |      0.329 |       0.167 |      0.504 |
| Semantic - Verbal       | Visualiser - Spatialiser |    0.543 | \[-0.381, 1.498\]  | 0.877 |      0.084 |       0.084 |      0.832 |
| Semantic - Visual       | Visualiser - Spatialiser |   -0.668 | \[-1.607, 0.294\]  | 0.916 |      0.880 |       0.062 |      0.058 |
| Sensorimotor - Semantic | Visualiser - Spatialiser |   -0.045 | \[-0.995, 0.893\]  | 0.537 |      0.454 |       0.163 |      0.383 |
| Sensorimotor - Spatial  | Visualiser - Spatialiser |    0.061 | \[-0.766, 0.922\]  | 0.557 |      0.357 |       0.183 |      0.460 |
| Sensorimotor - Verbal   | Visualiser - Spatialiser |    0.496 | \[-0.322, 1.315\]  | 0.889 |      0.071 |       0.092 |      0.837 |
| Sensorimotor - Visual   | Visualiser - Spatialiser |   -0.718 | \[-1.581, 0.133\]  | 0.951 |      0.921 |       0.049 |      0.029 |
| Spatial - Visual        | Visualiser - Spatialiser |   -0.777 | \[-1.599, 0.065\]  | 0.964 |      0.947 |       0.033 |      0.020 |
| Verbal - Spatial        | Visualiser - Spatialiser |   -0.437 | \[-1.269, 0.398\]  | 0.853 |      0.788 |       0.115 |      0.097 |
| Verbal - Visual         | Visualiser - Spatialiser |   -1.217 | \[-2.035, -0.394\] | 0.999 |      0.996 |       0.003 |      0.000 |
| Semantic - Spatial      | Visualiser - Verbaliser  |   -0.373 | \[-1.104, 0.352\]  | 0.842 |      0.769 |       0.131 |      0.100 |
| Semantic - Verbal       | Visualiser - Verbaliser  |    0.364 | \[-0.351, 1.088\]  | 0.843 |      0.097 |       0.135 |      0.768 |
| Semantic - Visual       | Visualiser - Verbaliser  |   -1.577 | \[-2.33, -0.794\]  | 1.000 |      1.000 |       0.000 |      0.000 |
| Sensorimotor - Semantic | Visualiser - Verbaliser  |    0.316 | \[-0.428, 1.055\]  | 0.801 |      0.134 |       0.148 |      0.718 |
| Sensorimotor - Spatial  | Visualiser - Verbaliser  |   -0.058 | \[-0.726, 0.601\]  | 0.570 |      0.450 |       0.234 |      0.316 |
| Sensorimotor - Verbal   | Visualiser - Verbaliser  |    0.674 | \[0.049, 1.317\]   | 0.983 |      0.008 |       0.026 |      0.965 |
| Sensorimotor - Visual   | Visualiser - Verbaliser  |   -1.257 | \[-1.98, -0.561\]  | 1.000 |      1.000 |       0.000 |      0.000 |
| Spatial - Visual        | Visualiser - Verbaliser  |   -1.201 | \[-1.896, -0.504\] | 1.000 |      0.999 |       0.000 |      0.000 |
| Verbal - Spatial        | Visualiser - Verbaliser  |   -0.739 | \[-1.394, -0.097\] | 0.989 |      0.974 |       0.020 |      0.005 |
| Verbal - Visual         | Visualiser - Verbaliser  |   -1.940 | \[-2.635, -1.241\] | 1.000 |      1.000 |       0.000 |      0.000 |

#### Frequentist

``` r
mf_strats_osivq  <- fit_clm(score ~ cluster * strategy, df_strats_long)

mf_strats_osivq |> get_singularity()

mf_strats_osivq |> get_performance() |> knitr::kable(align = "c")
```

|  AIC   |  BIC   | RMSE  |
|:------:|:------:|:-----:|
| 1291.3 | 1367.8 | 2.403 |

``` r
mf_strats_osivq |> report_contrast(~ cluster | strategy) |> knitr::kable()
```

| Contrast                 | Strategy     | Difference | 95% CI           | p.value |
|:-------------------------|:-------------|-----------:|:-----------------|--------:|
| Spatialiser - Visualiser | Visual       |     -0.810 | \[-1.53, -0.09\] |   0.024 |
| Verbaliser - Visualiser  | Visual       |     -1.521 | \[-2.13, -0.91\] |   0.000 |
| Verbaliser - Spatialiser | Visual       |     -0.710 | \[-1.49, 0.07\]  |   0.083 |
| Spatialiser - Visualiser | Spatial      |     -0.045 | \[-0.75, 0.66\]  |   0.988 |
| Verbaliser - Visualiser  | Spatial      |     -0.331 | \[-0.89, 0.23\]  |   0.347 |
| Verbaliser - Spatialiser | Spatial      |     -0.286 | \[-1, 0.43\]     |   0.614 |
| Spatialiser - Visualiser | Verbal       |      0.388 | \[-0.3, 1.07\]   |   0.381 |
| Verbaliser - Visualiser  | Verbal       |      0.404 | \[-0.14, 0.95\]  |   0.186 |
| Verbaliser - Spatialiser | Verbal       |      0.016 | \[-0.67, 0.71\]  |   0.998 |
| Spatialiser - Visualiser | Semantic     |     -0.146 | \[-1.02, 0.73\]  |   0.919 |
| Verbaliser - Visualiser  | Semantic     |      0.041 | \[-0.63, 0.71\]  |   0.989 |
| Verbaliser - Spatialiser | Semantic     |      0.187 | \[-0.7, 1.07\]   |   0.874 |
| Spatialiser - Visualiser | Sensorimotor |     -0.104 | \[-0.82, 0.61\]  |   0.937 |
| Verbaliser - Visualiser  | Sensorimotor |     -0.267 | \[-0.83, 0.3\]   |   0.508 |
| Verbaliser - Spatialiser | Sensorimotor |     -0.163 | \[-0.89, 0.56\]  |   0.857 |

``` r
mf_strats_osivq |> report_contrast(~ strategy | cluster) |> knitr::kable()
```

| Contrast                | Cluster     | Difference | 95% CI           | p.value |
|:------------------------|:------------|-----------:|:-----------------|--------:|
| Spatial - Visual        | Visualiser  |     -0.297 | \[-0.92, 0.33\]  |   0.699 |
| Verbal - Visual         | Visualiser  |      0.331 | \[-0.29, 0.95\]  |   0.590 |
| Verbal - Spatial        | Visualiser  |      0.628 | \[0, 1.26\]      |   0.050 |
| Semantic - Visual       | Visualiser  |     -1.542 | \[-2.24, -0.84\] |   0.000 |
| Semantic - Spatial      | Visualiser  |     -1.245 | \[-1.95, -0.54\] |   0.000 |
| Semantic - Verbal       | Visualiser  |     -1.873 | \[-2.58, -1.17\] |   0.000 |
| Sensorimotor - Visual   | Visualiser  |     -0.582 | \[-1.21, 0.05\]  |   0.087 |
| Sensorimotor - Spatial  | Visualiser  |     -0.285 | \[-0.92, 0.35\]  |   0.740 |
| Sensorimotor - Verbal   | Visualiser  |     -0.913 | \[-1.55, -0.28\] |   0.001 |
| Sensorimotor - Semantic | Visualiser  |      0.960 | \[0.25, 1.67\]   |   0.002 |
| Spatial - Visual        | Spatialiser |      0.469 | \[-0.52, 1.46\]  |   0.697 |
| Verbal - Visual         | Spatialiser |      1.529 | \[0.54, 2.52\]   |   0.000 |
| Verbal - Spatial        | Spatialiser |      1.061 | \[0.1, 2.02\]    |   0.021 |
| Semantic - Visual       | Spatialiser |     -0.877 | \[-2, 0.25\]     |   0.207 |
| Semantic - Spatial      | Spatialiser |     -1.346 | \[-2.45, -0.24\] |   0.008 |
| Semantic - Verbal       | Spatialiser |     -2.406 | \[-3.51, -1.3\]  |   0.000 |
| Sensorimotor - Visual   | Spatialiser |      0.124 | \[-0.87, 1.12\]  |   0.997 |
| Sensorimotor - Spatial  | Spatialiser |     -0.344 | \[-1.32, 0.63\]  |   0.871 |
| Sensorimotor - Verbal   | Spatialiser |     -1.405 | \[-2.37, -0.44\] |   0.001 |
| Sensorimotor - Semantic | Spatialiser |      1.002 | \[-0.11, 2.11\]  |   0.099 |
| Spatial - Visual        | Verbaliser  |      0.893 | \[0.16, 1.62\]   |   0.007 |
| Verbal - Visual         | Verbaliser  |      2.256 | \[1.52, 2.99\]   |   0.000 |
| Verbal - Spatial        | Verbaliser  |      1.363 | \[0.7, 2.02\]    |   0.000 |
| Semantic - Visual       | Verbaliser  |      0.020 | \[-0.77, 0.81\]  |   1.000 |
| Semantic - Spatial      | Verbaliser  |     -0.873 | \[-1.61, -0.14\] |   0.010 |
| Semantic - Verbal       | Verbaliser  |     -2.236 | \[-2.97, -1.5\]  |   0.000 |
| Sensorimotor - Visual   | Verbaliser  |      0.672 | \[-0.06, 1.4\]   |   0.090 |
| Sensorimotor - Spatial  | Verbaliser  |     -0.221 | \[-0.89, 0.45\]  |   0.896 |
| Sensorimotor - Verbal   | Verbaliser  |     -1.584 | \[-2.25, -0.91\] |   0.000 |
| Sensorimotor - Semantic | Verbaliser  |      0.652 | \[-0.08, 1.39\]  |   0.111 |

``` r
mf_strats_osivq |> report_contrast(~ cluster * strategy, interaction = TRUE) |>
  knitr::kable()
```

| cluster_revpairwise      | strategy_revpairwise    | Difference | 95% CI           | p.value |
|:-------------------------|:------------------------|-----------:|:-----------------|--------:|
| Spatialiser - Visualiser | Spatial - Visual        |      0.765 | \[-0.08, 1.61\]  |   0.075 |
| Verbaliser - Visualiser  | Spatial - Visual        |      1.190 | \[0.5, 1.88\]    |   0.001 |
| Verbaliser - Spatialiser | Spatial - Visual        |      0.424 | \[-0.46, 1.31\]  |   0.346 |
| Spatialiser - Visualiser | Verbal - Visual         |      1.198 | \[0.36, 2.03\]   |   0.005 |
| Verbaliser - Visualiser  | Verbal - Visual         |      1.925 | \[1.24, 2.61\]   |   0.000 |
| Verbaliser - Spatialiser | Verbal - Visual         |      0.727 | \[-0.14, 1.6\]   |   0.102 |
| Spatialiser - Visualiser | Verbal - Spatial        |      0.433 | \[-0.39, 1.25\]  |   0.301 |
| Verbaliser - Visualiser  | Verbal - Spatial        |      0.735 | \[0.08, 1.39\]   |   0.027 |
| Verbaliser - Spatialiser | Verbal - Spatial        |      0.302 | \[-0.53, 1.13\]  |   0.475 |
| Spatialiser - Visualiser | Semantic - Visual       |      0.665 | \[-0.28, 1.61\]  |   0.170 |
| Verbaliser - Visualiser  | Semantic - Visual       |      1.562 | \[0.8, 2.32\]    |   0.000 |
| Verbaliser - Spatialiser | Semantic - Visual       |      0.897 | \[-0.09, 1.88\]  |   0.075 |
| Spatialiser - Visualiser | Semantic - Spatial      |     -0.101 | \[-1.04, 0.84\]  |   0.833 |
| Verbaliser - Visualiser  | Semantic - Spatial      |      0.372 | \[-0.36, 1.1\]   |   0.317 |
| Verbaliser - Spatialiser | Semantic - Spatial      |      0.473 | \[-0.48, 1.42\]  |   0.329 |
| Spatialiser - Visualiser | Semantic - Verbal       |     -0.534 | \[-1.46, 0.4\]   |   0.260 |
| Verbaliser - Visualiser  | Semantic - Verbal       |     -0.363 | \[-1.08, 0.36\]  |   0.322 |
| Verbaliser - Spatialiser | Semantic - Verbal       |      0.170 | \[-0.77, 1.11\]  |   0.722 |
| Spatialiser - Visualiser | Sensorimotor - Visual   |      0.706 | \[-0.14, 1.55\]  |   0.103 |
| Verbaliser - Visualiser  | Sensorimotor - Visual   |      1.254 | \[0.56, 1.95\]   |   0.000 |
| Verbaliser - Spatialiser | Sensorimotor - Visual   |      0.547 | \[-0.34, 1.44\]  |   0.227 |
| Spatialiser - Visualiser | Sensorimotor - Spatial  |     -0.059 | \[-0.89, 0.78\]  |   0.890 |
| Verbaliser - Visualiser  | Sensorimotor - Spatial  |      0.064 | \[-0.6, 0.73\]   |   0.850 |
| Verbaliser - Spatialiser | Sensorimotor - Spatial  |      0.123 | \[-0.73, 0.97\]  |   0.776 |
| Spatialiser - Visualiser | Sensorimotor - Verbal   |     -0.492 | \[-1.32, 0.33\]  |   0.243 |
| Verbaliser - Visualiser  | Sensorimotor - Verbal   |     -0.671 | \[-1.33, -0.02\] |   0.044 |
| Verbaliser - Spatialiser | Sensorimotor - Verbal   |     -0.180 | \[-1.02, 0.66\]  |   0.674 |
| Spatialiser - Visualiser | Sensorimotor - Semantic |      0.042 | \[-0.9, 0.98\]   |   0.931 |
| Verbaliser - Visualiser  | Sensorimotor - Semantic |     -0.308 | \[-1.04, 0.42\]  |   0.409 |
| Verbaliser - Spatialiser | Sensorimotor - Semantic |     -0.350 | \[-1.3, 0.61\]   |   0.473 |

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
    #>  date     2025-12-11
    #>  pandoc   3.1.11 @ /opt/hostedtoolcache/pandoc/3.1.11/x64/ (via rmarkdown)
    #>  quarto   1.8.26 @ /usr/local/bin/quarto
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  ! package                 * version     date (UTC) lib source
    #>    abind                     1.4-8       2024-09-12 [1] RSPM
    #>    aphantasiaReasoningViie * 1.0         2025-12-11 [1] local
    #>    assertthat                0.2.1       2019-03-21 [1] RSPM
    #>    backports                 1.5.0       2024-05-23 [1] RSPM
    #>    bayesplot                 1.14.0      2025-08-31 [1] RSPM
    #>    bayestestR                0.17.0      2025-08-29 [1] RSPM
    #>    bridgesampling            1.2-1       2025-11-19 [1] RSPM
    #>    brms                      2.23.0      2025-09-09 [1] RSPM
    #>    Brobdingnag               1.2-9       2022-10-19 [1] RSPM
    #>    bslib                     0.9.0       2025-01-30 [1] RSPM
    #>    cachem                    1.1.0       2024-05-16 [1] RSPM
    #>    checkmate                 2.3.3       2025-08-18 [1] RSPM
    #>  P class                     7.3-23      2025-01-01 [?] CRAN (R 4.5.2)
    #>    cli                       3.6.5       2025-04-23 [1] RSPM
    #>    clue                      0.3-66      2024-11-13 [1] RSPM
    #>  P cluster                   2.1.8.1     2025-03-12 [?] CRAN (R 4.5.2)
    #>    clusterCrit               1.3.0       2023-11-23 [1] RSPM
    #>    clValid                   0.7         2021-02-14 [1] RSPM
    #>    coda                      0.19-4.1    2024-01-31 [1] RSPM
    #>  P codetools                 0.2-20      2024-03-31 [?] CRAN (R 4.5.2)
    #>    collapse                  2.1.5       2025-11-19 [1] RSPM
    #>    combinat                  0.0-8       2012-10-29 [1] RSPM
    #>    crayon                    1.5.3       2024-06-20 [1] RSPM
    #>    curl                      7.0.0       2025-08-19 [1] RSPM
    #>    data.table                1.17.8      2025-07-10 [1] RSPM
    #>    datawizard                1.3.0       2025-10-11 [1] RSPM
    #>    desc                      1.4.3       2023-12-10 [1] RSPM
    #>  P devtools                * 2.4.6       2025-10-03 [?] RSPM
    #>    diceR                     3.1.0       2025-06-19 [1] RSPM
    #>    digest                    0.6.39      2025-11-19 [1] RSPM
    #>    distributional            0.5.0       2024-09-17 [1] RSPM
    #>    dplyr                     1.1.4       2023-11-17 [1] RSPM
    #>    e1071                     1.7-16      2024-09-16 [1] RSPM
    #>  P ellipsis                  0.3.2       2021-04-29 [?] RSPM
    #>    emmeans                   2.0.0       2025-10-29 [1] RSPM
    #>    estimability              1.5.1       2024-05-12 [1] RSPM
    #>    evaluate                  1.0.5       2025-08-27 [1] RSPM
    #>    farver                    2.1.2       2024-05-13 [1] RSPM
    #>    fastmap                   1.2.0       2024-05-15 [1] RSPM
    #>    forcats                   1.0.1       2025-09-25 [1] RSPM
    #>    Formula                   1.2-5       2023-02-24 [1] RSPM
    #>    fs                        1.6.6       2025-04-12 [1] RSPM
    #>    generics                  0.1.4       2025-05-09 [1] RSPM
    #>    ggplot2                   4.0.1       2025-11-14 [1] RSPM
    #>    glue                      1.8.0       2024-09-30 [1] RSPM
    #>    gridExtra                 2.3         2017-09-09 [1] RSPM
    #>    gtable                    0.3.6       2024-10-25 [1] RSPM
    #>    haven                     2.5.5       2025-05-30 [1] RSPM
    #>    highr                     0.11        2024-05-26 [1] RSPM
    #>    hms                       1.1.4       2025-10-17 [1] RSPM
    #>    htmltools                 0.5.9       2025-12-04 [1] RSPM
    #>    htmlwidgets               1.6.4       2023-12-06 [1] RSPM
    #>    httpuv                    1.6.16      2025-04-16 [1] RSPM
    #>    inline                    0.3.21      2025-01-09 [1] RSPM
    #>    insight                   1.4.4       2025-12-06 [1] RSPM
    #>    jquerylib                 0.1.4       2021-04-26 [1] RSPM
    #>    jsonlite                  2.0.0       2025-03-27 [1] RSPM
    #>    klaR                      1.7-3       2023-12-13 [1] RSPM
    #>    knitr                     1.50        2025-03-16 [1] RSPM
    #>    labeling                  0.4.3       2023-08-29 [1] RSPM
    #>    labelled                  2.16.0      2025-10-22 [1] RSPM
    #>    later                     1.4.4       2025-08-27 [1] RSPM
    #>  P lattice                   0.22-7      2025-04-02 [?] CRAN (R 4.5.2)
    #>    lifecycle                 1.0.4       2023-11-07 [1] RSPM
    #>    loo                       2.8.0       2024-07-03 [1] RSPM
    #>    magrittr                  2.0.4       2025-09-12 [1] RSPM
    #>    marginaleffects           0.31.0      2025-11-15 [1] RSPM
    #>  P MASS                      7.3-65      2025-02-28 [?] CRAN (R 4.5.2)
    #>  P Matrix                    1.7-4       2025-08-28 [?] CRAN (R 4.5.2)
    #>    matrixStats               1.5.0       2025-01-07 [1] RSPM
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
    #>    performance               0.15.3      2025-12-01 [1] RSPM
    #>    pillar                    1.11.1      2025-09-17 [1] RSPM
    #>    pkgbuild                  1.4.8       2025-05-26 [1] RSPM
    #>    pkgconfig                 2.0.3       2019-09-22 [1] RSPM
    #>    pkgdown                   2.2.0       2025-11-06 [1] any (@2.2.0)
    #>    pkgload                   1.4.1       2025-09-23 [1] RSPM
    #>    plyr                      1.8.9       2023-10-02 [1] RSPM
    #>    posterior                 1.6.1       2025-02-27 [1] RSPM
    #>    promises                  1.5.0       2025-11-01 [1] RSPM
    #>    proxy                     0.4-27      2022-06-09 [1] RSPM
    #>    purrr                     1.2.0       2025-11-04 [1] RSPM
    #>    questionr                 0.8.1       2025-06-10 [1] RSPM
    #>    QuickJSR                  1.8.1       2025-09-20 [1] RSPM
    #>    R6                        2.6.1       2025-02-15 [1] RSPM
    #>    ragg                      1.5.0       2025-09-02 [1] RSPM
    #>    RColorBrewer              1.1-3       2022-04-03 [1] RSPM
    #>    Rcpp                      1.1.0       2025-07-02 [1] RSPM
    #>    RcppParallel              5.1.11-1    2025-08-27 [1] RSPM
    #>  P remotes                   2.5.0       2024-03-17 [?] RSPM
    #>    renv                      1.1.4       2025-03-20 [1] RSPM (R 4.5.0)
    #>    reshape2                  1.4.5       2025-11-12 [1] RSPM
    #>    rlang                     1.1.6       2025-04-11 [1] RSPM
    #>    rmarkdown                 2.30        2025-09-28 [1] RSPM
    #>    rstan                     2.32.7      2025-03-10 [1] RSPM
    #>    rstantools                2.5.0       2025-09-01 [1] RSPM
    #>    rstudioapi                0.17.1      2024-10-22 [1] RSPM
    #>    S7                        0.2.1       2025-11-14 [1] RSPM
    #>    sass                      0.4.10      2025-04-11 [1] RSPM
    #>    scales                    1.4.0       2025-04-24 [1] RSPM
    #>    see                       0.12.0      2025-09-14 [1] RSPM
    #>    sessioninfo               1.2.3       2025-02-05 [1] RSPM
    #>    shiny                     1.12.1      2025-12-09 [1] RSPM
    #>    showtext                  0.9-7       2024-03-02 [1] RSPM
    #>    showtextdb                3.0         2020-06-04 [1] RSPM
    #>    StanHeaders               2.32.10     2024-07-15 [1] RSPM
    #>    stringi                   1.8.7       2025-03-27 [1] RSPM
    #>    stringr                   1.6.0       2025-11-04 [1] RSPM
    #>    sysfonts                  0.8.9       2024-03-02 [1] RSPM
    #>    systemfonts               1.3.1       2025-10-01 [1] RSPM
    #>    tensorA                   0.36.2.1    2023-12-13 [1] RSPM
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
    #>    yaml                      2.3.12      2025-12-10 [1] RSPM
    #> 
    #>  [1] /home/runner/.cache/R/renv/library/aphantasiaReasoningViie-b75da44b/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu
    #>  [2] /home/runner/.cache/R/renv/sandbox/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/8f3cef43
    #> 
    #>  * ── Packages attached to the search path.
    #>  P ── Loaded and on-disk path mismatch.
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────

Bürkner, P.-C. (2017). Brms: An R Package for Bayesian Multilevel Models
Using Stan. *Journal of Statistical Software*, *80*, 1–28.
<https://doi.org/10.18637/jss.v080.i01>

Christensen, R. H. B. (2023). *Ordinal—regression models for ordinal
data*. <https://CRAN.R-project.org/package=ordinal>

Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., & Lüdecke, D. (2019).
Indices of Effect Existence and Significance in the Bayesian Framework.
*Frontiers in Psychology*, *10*.
<https://doi.org/10.3389/fpsyg.2019.02767>

------------------------------------------------------------------------

1.  This specific number was chosen arbitrarily to have an even number
    of iterations on each chain on the two computers that were used for
    the analyses, one with 20 cores and one with 24 cores. The only
    important aspect here is to have enough iterations to ensure
    convergence and a good effective sample size.

2.  The marginaleffects package was not used for frequentist analyses
    because it conflicted with *ordinal* models, whereas the emmeans
    package worked fine.
