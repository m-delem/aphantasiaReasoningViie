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
draws <- seq(1, 8000, 1) # To limit draws that will be used for marginaleffects
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
| Visual       | Typical - Aphantasia |    1.696 | \[1.211, 2.2\]    | 1.000 |      0.000 |       0.000 |      1.000 |
| Spatial      | Typical - Aphantasia |    0.405 | \[-0.032, 0.819\] | 0.966 |      0.013 |       0.068 |      0.920 |
| Verbal       | Typical - Aphantasia |   -0.268 | \[-0.684, 0.151\] | 0.896 |      0.780 |       0.179 |      0.042 |
| Semantic     | Typical - Aphantasia |   -0.229 | \[-0.738, 0.277\] | 0.811 |      0.696 |       0.202 |      0.102 |
| Sensorimotor | Typical - Aphantasia |    0.135 | \[-0.28, 0.567\]  | 0.731 |      0.138 |       0.294 |      0.568 |

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
| Aphantasia | Semantic - Spatial      |   -0.775 | \[-1.265, -0.292\] | 0.999 |      0.996 |       0.003 |      0.001 |
| Aphantasia | Semantic - Verbal       |   -2.159 | \[-2.655, -1.663\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Aphantasia | Semantic - Visual       |    0.305 | \[-0.237, 0.873\]  | 0.864 |      0.072 |       0.160 |      0.769 |
| Aphantasia | Sensorimotor - Semantic |    0.653 | \[0.169, 1.137\]   | 0.996 |      0.001 |       0.011 |      0.988 |
| Aphantasia | Sensorimotor - Spatial  |   -0.122 | \[-0.576, 0.333\]  | 0.700 |      0.537 |       0.292 |      0.171 |
| Aphantasia | Sensorimotor - Verbal   |   -1.502 | \[-1.953, -1.058\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Aphantasia | Sensorimotor - Visual   |    0.960 | \[0.46, 1.486\]    | 1.000 |      0.000 |       0.001 |      0.999 |
| Aphantasia | Spatial - Visual        |    1.081 | \[0.571, 1.611\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Aphantasia | Verbal - Spatial        |    1.380 | \[0.928, 1.846\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Aphantasia | Verbal - Visual         |    2.462 | \[1.945, 2.995\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Typical    | Semantic - Spatial      |   -1.412 | \[-1.862, -0.958\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical    | Semantic - Verbal       |   -2.123 | \[-2.599, -1.658\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical    | Semantic - Visual       |   -1.621 | \[-2.06, -1.184\]  | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical    | Sensorimotor - Semantic |    1.022 | \[0.574, 1.477\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Typical    | Sensorimotor - Spatial  |   -0.385 | \[-0.783, 0.013\]  | 0.971 |      0.925 |       0.066 |      0.009 |
| Typical    | Sensorimotor - Verbal   |   -1.098 | \[-1.512, -0.692\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical    | Sensorimotor - Visual   |   -0.602 | \[-0.986, -0.201\] | 0.998 |      0.993 |       0.007 |      0.000 |
| Typical    | Spatial - Visual        |   -0.209 | \[-0.61, 0.174\]   | 0.857 |      0.710 |       0.235 |      0.055 |
| Typical    | Verbal - Spatial        |    0.710 | \[0.321, 1.117\]   | 1.000 |      0.000 |       0.001 |      0.999 |
| Typical    | Verbal - Visual         |    0.502 | \[0.115, 0.891\]   | 0.995 |      0.001 |       0.020 |      0.979 |

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
| Semantic - Spatial      | Aphantasia - Typical |    0.632 | \[-0.021, 1.292\] | 0.971 |      0.015 |       0.042 |      0.943 |
| Semantic - Verbal       | Aphantasia - Typical |   -0.032 | \[-0.711, 0.621\] | 0.540 |      0.420 |       0.233 |      0.347 |
| Semantic - Visual       | Aphantasia - Typical |    1.928 | \[1.231, 2.636\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Sensorimotor - Semantic | Aphantasia - Typical |   -0.366 | \[-1.036, 0.298\] | 0.865 |      0.791 |       0.127 |      0.082 |
| Sensorimotor - Spatial  | Aphantasia - Typical |    0.269 | \[-0.341, 0.851\] | 0.813 |      0.117 |       0.172 |      0.711 |
| Sensorimotor - Verbal   | Aphantasia - Typical |   -0.405 | \[-1.012, 0.214\] | 0.906 |      0.841 |       0.106 |      0.054 |
| Sensorimotor - Visual   | Aphantasia - Typical |    1.556 | \[0.923, 2.215\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Spatial - Visual        | Aphantasia - Typical |    1.289 | \[0.672, 1.95\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Verbal - Spatial        | Aphantasia - Typical |    0.673 | \[0.071, 1.267\]  | 0.986 |      0.007 |       0.023 |      0.970 |
| Verbal - Visual         | Aphantasia - Typical |    1.959 | \[1.339, 2.633\]  | 1.000 |      0.000 |       0.000 |      1.000 |

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
| Visual       | Hypophantasia - Aphantasia |    1.701 | \[0.692, 2.867\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Visual       | Typical - Aphantasia       |    2.740 | \[1.869, 3.826\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Visual       | Typical - Hypophantasia    |    1.042 | \[0.435, 1.667\]  | 1.000 |      0.000 |       0.001 |      0.999 |
| Spatial      | Hypophantasia - Aphantasia |   -0.601 | \[-1.319, 0.07\]  | 0.960 |      0.931 |       0.048 |      0.021 |
| Spatial      | Typical - Aphantasia       |    0.201 | \[-0.278, 0.676\] | 0.798 |      0.112 |       0.230 |      0.658 |
| Spatial      | Typical - Hypophantasia    |    0.806 | \[0.185, 1.45\]   | 0.995 |      0.002 |       0.010 |      0.988 |
| Verbal       | Hypophantasia - Aphantasia |    0.091 | \[-0.556, 0.724\] | 0.604 |      0.285 |       0.223 |      0.492 |
| Verbal       | Typical - Aphantasia       |   -0.234 | \[-0.709, 0.236\] | 0.843 |      0.723 |       0.198 |      0.080 |
| Verbal       | Typical - Hypophantasia    |   -0.325 | \[-0.894, 0.25\]  | 0.869 |      0.782 |       0.144 |      0.074 |
| Semantic     | Hypophantasia - Aphantasia |    0.197 | \[-0.573, 0.938\] | 0.697 |      0.218 |       0.184 |      0.598 |
| Semantic     | Typical - Aphantasia       |   -0.157 | \[-0.728, 0.425\] | 0.699 |      0.577 |       0.225 |      0.198 |
| Semantic     | Typical - Hypophantasia    |   -0.354 | \[-1.023, 0.361\] | 0.842 |      0.762 |       0.136 |      0.102 |
| Sensorimotor | Hypophantasia - Aphantasia |   -0.008 | \[-0.68, 0.655\]  | 0.509 |      0.391 |       0.235 |      0.374 |
| Sensorimotor | Typical - Aphantasia       |    0.139 | \[-0.346, 0.629\] | 0.702 |      0.173 |       0.266 |      0.561 |
| Sensorimotor | Typical - Hypophantasia    |    0.147 | \[-0.465, 0.749\] | 0.679 |      0.210 |       0.231 |      0.559 |

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
| Aphantasia    | Semantic - Spatial      |   -1.066 | \[-1.672, -0.468\] | 1.000 |      0.999 |       0.001 |      0.000 |
| Aphantasia    | Semantic - Verbal       |   -2.219 | \[-2.819, -1.629\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Aphantasia    | Semantic - Visual       |    1.264 | \[0.311, 2.418\]   | 0.996 |      0.003 |       0.005 |      0.993 |
| Aphantasia    | Sensorimotor - Semantic |    0.739 | \[0.119, 1.361\]   | 0.990 |      0.004 |       0.018 |      0.979 |
| Aphantasia    | Sensorimotor - Spatial  |   -0.326 | \[-0.876, 0.241\]  | 0.870 |      0.782 |       0.148 |      0.070 |
| Aphantasia    | Sensorimotor - Verbal   |   -1.484 | \[-2.044, -0.902\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Aphantasia    | Sensorimotor - Visual   |    2.010 | \[1.067, 3.117\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Aphantasia    | Spatial - Visual        |    2.324 | \[1.408, 3.45\]    | 1.000 |      0.000 |       0.000 |      1.000 |
| Aphantasia    | Verbal - Spatial        |    1.156 | \[0.606, 1.691\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Aphantasia    | Verbal - Visual         |    3.486 | \[2.558, 4.597\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Hypophantasia | Semantic - Spatial      |   -0.265 | \[-1.08, 0.569\]   | 0.742 |      0.658 |       0.149 |      0.192 |
| Hypophantasia | Semantic - Verbal       |   -2.112 | \[-2.921, -1.324\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Hypophantasia | Semantic - Visual       |   -0.244 | \[-1.046, 0.573\]  | 0.721 |      0.637 |       0.157 |      0.206 |
| Hypophantasia | Sensorimotor - Semantic |    0.534 | \[-0.265, 1.354\]  | 0.907 |      0.060 |       0.081 |      0.859 |
| Hypophantasia | Sensorimotor - Spatial  |    0.269 | \[-0.512, 1.046\]  | 0.754 |      0.172 |       0.160 |      0.669 |
| Hypophantasia | Sensorimotor - Verbal   |   -1.576 | \[-2.321, -0.838\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Hypophantasia | Sensorimotor - Visual   |    0.302 | \[-0.454, 1.057\]  | 0.768 |      0.160 |       0.154 |      0.686 |
| Hypophantasia | Spatial - Visual        |    0.028 | \[-0.777, 0.813\]  | 0.528 |      0.380 |       0.192 |      0.428 |
| Hypophantasia | Verbal - Spatial        |    1.842 | \[1.082, 2.624\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Hypophantasia | Verbal - Visual         |    1.873 | \[1.116, 2.621\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Typical       | Semantic - Spatial      |   -1.415 | \[-1.866, -0.985\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical       | Semantic - Verbal       |   -2.134 | \[-2.598, -1.678\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical       | Semantic - Visual       |   -1.638 | \[-2.089, -1.182\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical       | Sensorimotor - Semantic |    1.032 | \[0.59, 1.473\]    | 1.000 |      0.000 |       0.000 |      1.000 |
| Typical       | Sensorimotor - Spatial  |   -0.386 | \[-0.79, 0.013\]   | 0.970 |      0.918 |       0.074 |      0.008 |
| Typical       | Sensorimotor - Verbal   |   -1.107 | \[-1.51, -0.705\]  | 1.000 |      1.000 |       0.000 |      0.000 |
| Typical       | Sensorimotor - Visual   |   -0.604 | \[-1.002, -0.203\] | 0.999 |      0.994 |       0.006 |      0.000 |
| Typical       | Spatial - Visual        |   -0.218 | \[-0.607, 0.173\]  | 0.862 |      0.722 |       0.223 |      0.054 |
| Typical       | Verbal - Spatial        |    0.719 | \[0.329, 1.107\]   | 1.000 |      0.000 |       0.001 |      0.999 |
| Typical       | Verbal - Visual         |    0.501 | \[0.108, 0.878\]   | 0.995 |      0.002 |       0.021 |      0.977 |

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
| Semantic - Spatial      | Aphantasia - Hypophantasia |   -0.796 | \[-1.831, 0.228\] | 0.937 |      0.908 |       0.048 |      0.044 |
| Semantic - Verbal       | Aphantasia - Hypophantasia |   -0.116 | \[-1.073, 0.884\] | 0.585 |      0.513 |       0.149 |      0.338 |
| Semantic - Visual       | Aphantasia - Hypophantasia |    1.520 | \[0.211, 2.91\]   | 0.989 |      0.007 |       0.009 |      0.984 |
| Sensorimotor - Semantic | Aphantasia - Hypophantasia |    0.206 | \[-0.813, 1.218\] | 0.654 |      0.276 |       0.140 |      0.584 |
| Sensorimotor - Spatial  | Aphantasia - Hypophantasia |   -0.598 | \[-1.544, 0.369\] | 0.889 |      0.846 |       0.080 |      0.074 |
| Sensorimotor - Verbal   | Aphantasia - Hypophantasia |    0.101 | \[-0.838, 1.001\] | 0.582 |      0.339 |       0.160 |      0.501 |
| Sensorimotor - Visual   | Aphantasia - Hypophantasia |    1.730 | \[0.486, 3.072\]  | 0.997 |      0.002 |       0.003 |      0.995 |
| Spatial - Visual        | Aphantasia - Hypophantasia |    2.313 | \[1.073, 3.66\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Verbal - Spatial        | Aphantasia - Hypophantasia |   -0.687 | \[-1.643, 0.234\] | 0.927 |      0.894 |       0.059 |      0.047 |
| Verbal - Visual         | Aphantasia - Hypophantasia |    1.620 | \[0.394, 2.958\]  | 0.996 |      0.002 |       0.005 |      0.994 |
| Semantic - Spatial      | Aphantasia - Typical       |    0.354 | \[-0.389, 1.097\] | 0.818 |      0.119 |       0.135 |      0.746 |
| Semantic - Verbal       | Aphantasia - Typical       |   -0.083 | \[-0.819, 0.662\] | 0.585 |      0.484 |       0.203 |      0.314 |
| Semantic - Visual       | Aphantasia - Typical       |    2.902 | \[1.829, 4.128\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Sensorimotor - Semantic | Aphantasia - Typical       |   -0.289 | \[-1.051, 0.456\] | 0.772 |      0.687 |       0.158 |      0.154 |
| Sensorimotor - Spatial  | Aphantasia - Typical       |    0.059 | \[-0.628, 0.761\] | 0.566 |      0.323 |       0.221 |      0.456 |
| Sensorimotor - Verbal   | Aphantasia - Typical       |   -0.378 | \[-1.055, 0.308\] | 0.858 |      0.786 |       0.129 |      0.085 |
| Sensorimotor - Visual   | Aphantasia - Typical       |    2.611 | \[1.584, 3.798\]  | 1.000 |      0.000 |       0.000 |      1.000 |
| Spatial - Visual        | Aphantasia - Typical       |    2.545 | \[1.52, 3.725\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Verbal - Spatial        | Aphantasia - Typical       |    0.442 | \[-0.236, 1.107\] | 0.899 |      0.060 |       0.102 |      0.838 |
| Verbal - Visual         | Aphantasia - Typical       |    2.993 | \[1.975, 4.16\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Semantic - Spatial      | Hypophantasia - Typical    |    1.152 | \[0.207, 2.105\]  | 0.993 |      0.004 |       0.010 |      0.987 |
| Semantic - Verbal       | Hypophantasia - Typical    |    0.028 | \[-0.9, 0.929\]   | 0.525 |      0.389 |       0.176 |      0.435 |
| Semantic - Visual       | Hypophantasia - Typical    |    1.388 | \[0.461, 2.315\]  | 0.998 |      0.001 |       0.003 |      0.996 |
| Sensorimotor - Semantic | Hypophantasia - Typical    |   -0.500 | \[-1.399, 0.447\] | 0.858 |      0.806 |       0.090 |      0.104 |
| Sensorimotor - Spatial  | Hypophantasia - Typical    |    0.655 | \[-0.213, 1.548\] | 0.932 |      0.043 |       0.058 |      0.899 |
| Sensorimotor - Verbal   | Hypophantasia - Typical    |   -0.477 | \[-1.279, 0.368\] | 0.866 |      0.811 |       0.100 |      0.089 |
| Sensorimotor - Visual   | Hypophantasia - Typical    |    0.896 | \[0.057, 1.76\]   | 0.981 |      0.011 |       0.020 |      0.968 |
| Spatial - Visual        | Hypophantasia - Typical    |    0.245 | \[-0.651, 1.116\] | 0.697 |      0.230 |       0.153 |      0.617 |
| Verbal - Spatial        | Hypophantasia - Typical    |    1.127 | \[0.287, 1.993\]  | 0.996 |      0.003 |       0.006 |      0.991 |
| Verbal - Visual         | Hypophantasia - Typical    |    1.373 | \[0.516, 2.193\]  | 0.999 |      0.000 |       0.001 |      0.998 |

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
| Visual       | Spatialiser - Visualiser |   -0.819 | \[-1.431, -0.228\] | 0.997 |      0.992 |       0.007 |      0.001 |
| Visual       | Verbaliser - Spatialiser |   -0.712 | \[-1.369, -0.059\] | 0.983 |      0.966 |       0.026 |      0.008 |
| Visual       | Verbaliser - Visualiser  |   -1.534 | \[-2.057, -1.019\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Spatial      | Spatialiser - Visualiser |   -0.048 | \[-0.625, 0.549\]  | 0.564 |      0.430 |       0.262 |      0.308 |
| Spatial      | Verbaliser - Spatialiser |   -0.276 | \[-0.876, 0.307\]  | 0.827 |      0.730 |       0.171 |      0.099 |
| Spatial      | Verbaliser - Visualiser  |   -0.327 | \[-0.798, 0.145\]  | 0.920 |      0.835 |       0.129 |      0.037 |
| Verbal       | Spatialiser - Visualiser |    0.387 | \[-0.18, 0.957\]   | 0.910 |      0.044 |       0.115 |      0.841 |
| Verbal       | Verbaliser - Spatialiser |    0.022 | \[-0.555, 0.586\]  | 0.530 |      0.337 |       0.275 |      0.388 |
| Verbal       | Verbaliser - Visualiser  |    0.406 | \[-0.034, 0.854\]  | 0.963 |      0.012 |       0.080 |      0.908 |
| Semantic     | Spatialiser - Visualiser |   -0.150 | \[-0.916, 0.568\]  | 0.655 |      0.559 |       0.192 |      0.249 |
| Semantic     | Verbaliser - Spatialiser |    0.193 | \[-0.529, 0.956\]  | 0.697 |      0.212 |       0.187 |      0.601 |
| Semantic     | Verbaliser - Visualiser  |    0.046 | \[-0.542, 0.604\]  | 0.563 |      0.304 |       0.272 |      0.424 |
| Sensorimotor | Spatialiser - Visualiser |   -0.105 | \[-0.698, 0.492\]  | 0.640 |      0.508 |       0.244 |      0.248 |
| Sensorimotor | Verbaliser - Spatialiser |   -0.158 | \[-0.76, 0.43\]    | 0.699 |      0.574 |       0.231 |      0.195 |
| Sensorimotor | Verbaliser - Visualiser  |   -0.265 | \[-0.744, 0.206\]  | 0.870 |      0.762 |       0.175 |      0.063 |

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
| Visualiser  | Semantic - Spatial      |   -1.252 | \[-1.765, -0.752\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Visualiser  | Semantic - Verbal       |   -1.884 | \[-2.386, -1.375\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Visualiser  | Semantic - Visual       |   -1.555 | \[-2.067, -1.053\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Visualiser  | Sensorimotor - Semantic |    0.968 | \[0.463, 1.489\]   | 1.000 |      0.000 |       0.001 |      0.999 |
| Visualiser  | Sensorimotor - Spatial  |   -0.287 | \[-0.755, 0.18\]   | 0.887 |      0.788 |       0.161 |      0.051 |
| Visualiser  | Sensorimotor - Verbal   |   -0.913 | \[-1.358, -0.469\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Visualiser  | Sensorimotor - Visual   |   -0.586 | \[-1.043, -0.132\] | 0.993 |      0.982 |       0.016 |      0.002 |
| Visualiser  | Spatial - Visual        |   -0.300 | \[-0.741, 0.142\]  | 0.905 |      0.814 |       0.145 |      0.041 |
| Visualiser  | Verbal - Spatial        |    0.626 | \[0.178, 1.076\]   | 0.997 |      0.001 |       0.010 |      0.989 |
| Visualiser  | Verbal - Visual         |    0.332 | \[-0.126, 0.772\]  | 0.920 |      0.034 |       0.122 |      0.844 |
| Spatialiser | Semantic - Spatial      |   -1.367 | \[-2.155, -0.585\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Spatialiser | Semantic - Verbal       |   -2.426 | \[-3.23, -1.673\]  | 1.000 |      1.000 |       0.000 |      0.000 |
| Spatialiser | Semantic - Visual       |   -0.886 | \[-1.704, -0.085\] | 0.986 |      0.973 |       0.021 |      0.006 |
| Spatialiser | Sensorimotor - Semantic |    1.014 | \[0.236, 1.836\]   | 0.996 |      0.002 |       0.007 |      0.991 |
| Spatialiser | Sensorimotor - Spatial  |   -0.351 | \[-1.038, 0.335\]  | 0.834 |      0.755 |       0.143 |      0.101 |
| Spatialiser | Sensorimotor - Verbal   |   -1.414 | \[-2.088, -0.733\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Spatialiser | Sensorimotor - Visual   |    0.126 | \[-0.584, 0.855\]  | 0.639 |      0.270 |       0.199 |      0.530 |
| Spatialiser | Spatial - Visual        |    0.480 | \[-0.215, 1.176\]  | 0.911 |      0.051 |       0.100 |      0.850 |
| Spatialiser | Verbal - Spatial        |    1.068 | \[0.39, 1.744\]    | 0.999 |      0.001 |       0.002 |      0.997 |
| Spatialiser | Verbal - Visual         |    1.538 | \[0.841, 2.244\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Verbaliser  | Semantic - Spatial      |   -0.879 | \[-1.411, -0.367\] | 1.000 |      0.999 |       0.001 |      0.000 |
| Verbaliser  | Semantic - Verbal       |   -2.244 | \[-2.781, -1.728\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Verbaliser  | Semantic - Visual       |    0.021 | \[-0.555, 0.598\]  | 0.527 |      0.336 |       0.274 |      0.390 |
| Verbaliser  | Sensorimotor - Semantic |    0.656 | \[0.129, 1.197\]   | 0.991 |      0.003 |       0.018 |      0.979 |
| Verbaliser  | Sensorimotor - Spatial  |   -0.225 | \[-0.709, 0.246\]  | 0.825 |      0.701 |       0.214 |      0.086 |
| Verbaliser  | Sensorimotor - Verbal   |   -1.594 | \[-2.075, -1.121\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Verbaliser  | Sensorimotor - Visual   |    0.675 | \[0.149, 1.217\]   | 0.994 |      0.002 |       0.014 |      0.984 |
| Verbaliser  | Spatial - Visual        |    0.903 | \[0.381, 1.434\]   | 1.000 |      0.000 |       0.001 |      0.999 |
| Verbaliser  | Verbal - Spatial        |    1.367 | \[0.896, 1.832\]   | 1.000 |      0.000 |       0.000 |      1.000 |
| Verbaliser  | Verbal - Visual         |    2.272 | \[1.748, 2.808\]   | 1.000 |      0.000 |       0.000 |      1.000 |

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
| Semantic - Spatial      | Spatialiser - Verbaliser |   -0.482 | \[-1.432, 0.448\]  | 0.845 |      0.789 |       0.102 |      0.110 |
| Semantic - Verbal       | Spatialiser - Verbaliser |   -0.175 | \[-1.107, 0.741\]  | 0.643 |      0.560 |       0.157 |      0.283 |
| Semantic - Visual       | Spatialiser - Verbaliser |   -0.912 | \[-1.903, 0.068\]  | 0.967 |      0.950 |       0.028 |      0.022 |
| Sensorimotor - Semantic | Spatialiser - Verbaliser |    0.358 | \[-0.562, 1.322\]  | 0.774 |      0.172 |       0.121 |      0.707 |
| Sensorimotor - Spatial  | Spatialiser - Verbaliser |   -0.118 | \[-0.957, 0.719\]  | 0.611 |      0.515 |       0.184 |      0.301 |
| Sensorimotor - Verbal   | Spatialiser - Verbaliser |    0.177 | \[-0.62, 1.002\]   | 0.663 |      0.253 |       0.174 |      0.573 |
| Sensorimotor - Visual   | Spatialiser - Verbaliser |   -0.551 | \[-1.422, 0.35\]   | 0.885 |      0.839 |       0.085 |      0.077 |
| Spatial - Visual        | Spatialiser - Verbaliser |   -0.428 | \[-1.311, 0.438\]  | 0.830 |      0.769 |       0.112 |      0.118 |
| Verbal - Spatial        | Spatialiser - Verbaliser |   -0.298 | \[-1.119, 0.514\]  | 0.768 |      0.685 |       0.150 |      0.165 |
| Verbal - Visual         | Spatialiser - Verbaliser |   -0.725 | \[-1.615, 0.134\]  | 0.954 |      0.925 |       0.046 |      0.029 |
| Semantic - Spatial      | Visualiser - Spatialiser |    0.110 | \[-0.825, 1.063\]  | 0.591 |      0.329 |       0.164 |      0.508 |
| Semantic - Verbal       | Visualiser - Spatialiser |    0.542 | \[-0.381, 1.502\]  | 0.876 |      0.086 |       0.083 |      0.831 |
| Semantic - Visual       | Visualiser - Spatialiser |   -0.663 | \[-1.598, 0.306\]  | 0.915 |      0.880 |       0.062 |      0.059 |
| Sensorimotor - Semantic | Visualiser - Spatialiser |   -0.048 | \[-1.008, 0.888\]  | 0.538 |      0.454 |       0.164 |      0.382 |
| Sensorimotor - Spatial  | Visualiser - Spatialiser |    0.061 | \[-0.757, 0.915\]  | 0.556 |      0.357 |       0.182 |      0.461 |
| Sensorimotor - Verbal   | Visualiser - Spatialiser |    0.497 | \[-0.32, 1.308\]   | 0.888 |      0.072 |       0.093 |      0.835 |
| Sensorimotor - Visual   | Visualiser - Spatialiser |   -0.717 | \[-1.572, 0.127\]  | 0.951 |      0.921 |       0.050 |      0.029 |
| Spatial - Visual        | Visualiser - Spatialiser |   -0.774 | \[-1.598, 0.065\]  | 0.964 |      0.948 |       0.032 |      0.020 |
| Verbal - Spatial        | Visualiser - Spatialiser |   -0.437 | \[-1.269, 0.399\]  | 0.856 |      0.789 |       0.114 |      0.097 |
| Verbal - Visual         | Visualiser - Spatialiser |   -1.211 | \[-2.027, -0.394\] | 0.999 |      0.996 |       0.003 |      0.000 |
| Semantic - Spatial      | Visualiser - Verbaliser  |   -0.374 | \[-1.098, 0.355\]  | 0.844 |      0.770 |       0.131 |      0.099 |
| Semantic - Verbal       | Visualiser - Verbaliser  |    0.367 | \[-0.351, 1.091\]  | 0.842 |      0.099 |       0.134 |      0.767 |
| Semantic - Visual       | Visualiser - Verbaliser  |   -1.582 | \[-2.332, -0.793\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Sensorimotor - Semantic | Visualiser - Verbaliser  |    0.315 | \[-0.42, 1.055\]   | 0.800 |      0.134 |       0.149 |      0.717 |
| Sensorimotor - Spatial  | Visualiser - Verbaliser  |   -0.060 | \[-0.727, 0.606\]  | 0.570 |      0.450 |       0.234 |      0.316 |
| Sensorimotor - Verbal   | Visualiser - Verbaliser  |    0.674 | \[0.05, 1.315\]    | 0.983 |      0.008 |       0.027 |      0.965 |
| Sensorimotor - Visual   | Visualiser - Verbaliser  |   -1.262 | \[-1.981, -0.558\] | 1.000 |      1.000 |       0.000 |      0.000 |
| Spatial - Visual        | Visualiser - Verbaliser  |   -1.208 | \[-1.896, -0.501\] | 1.000 |      0.999 |       0.000 |      0.000 |
| Verbal - Spatial        | Visualiser - Verbaliser  |   -0.738 | \[-1.384, -0.097\] | 0.989 |      0.974 |       0.021 |      0.005 |
| Verbal - Visual         | Visualiser - Verbaliser  |   -1.944 | \[-2.636, -1.243\] | 1.000 |      1.000 |       0.000 |      0.000 |

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
