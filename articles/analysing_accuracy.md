# Accuracy analyses

This vignette contains a full breakdown of the analyses of participants’
accuracy on the reasoning problems. Only Bayesian analyses were reported
in the manuscript for brevity, but equivalent frequentist analyses were
also conducted to test the convergence of the two approaches on similar
results. They are reported in this vignette alongside the Bayesian
analyses for completeness.

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
df_expe <-
  get_clustered_data("experiment") |> 
  dplyr::select(id, group_4:strategy_group, problem, category, accuracy)

dplyr::glimpse(df_expe)
#> Rows: 2,808
#> Columns: 9
#> $ id             <fct> acdn247721443631359lzxb, acdn247721443631359lzxb, acdn2…
#> $ group_4        <fct> Typical, Typical, Typical, Typical, Typical, Typical, T…
#> $ cluster        <fct> Visualiser, Visualiser, Visualiser, Visualiser, Visuali…
#> $ group_2        <fct> Typical, Typical, Typical, Typical, Typical, Typical, T…
#> $ group_3        <fct> Typical, Typical, Typical, Typical, Typical, Typical, T…
#> $ strategy_group <fct> No_visual_strategy, No_visual_strategy, No_visual_strat…
#> $ problem        <int> 18, 25, 2, 19, 1, 10, 6, 9, 8, 26, 5, 15, 23, 14, 17, 2…
#> $ category       <fct> Spatial, Control, Visual, Control, Visual, Spatial, Vis…
#> $ accuracy       <int> 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
```

## Method

We fitted hierarchical linear models (also said “multilevel” or “mixed”
models) with binomial distributions and logit links using the *brms*
package for Bayesian models (Bürkner, 2017) or the *glmmTMB* package for
frequentist models (McGillycuddy et al., 2025). The models included a
grouping variable (VVIQ groups, OSIVQ clusters), Category (visual,
spatial, or control) along with their two-way interactions as fixed
categorical predictors. Varying slopes and intercepts (“random effects”)
have been added for each participant by category along with an intercept
for each problem.

Let’s break this down.

### Grouping variables

We used several grouping variables to classify participants, all of
which are in the `df_expe` data frame:

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

- `strategy_group` is a classification based on the self-reported
  strategies used by the participants to solve the problems. It focuses
  on whether participants used a visual mental imagery strategy and
  contains two groups: “Visual strategy user” and “No visual strategy”.
  It is not reported in the manuscript because it was suggested to us
  after submission by colleagues at a conference.

The same modelling pipeline was therefore applied four times, once for
each of the last four grouping variables.

### Model formula

We created a small
[`build_formula()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/build_formula.md)
helper function to write the formula easily, as we used the same model
structure a lot of times:

``` r
build_formula("accuracy", "group_2")
#> accuracy ~ group_2 * category + (category | id) + (1 | problem)
#> <environment: 0x5600e8402f10>

build_formula("accuracy", "cluster")
#> accuracy ~ cluster * category + (category | id) + (1 | problem)
#> <environment: 0x5600ea553f48>
```

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
installation command which is provided in the next chunk if needed. A
regularising normal prior with mean 0 and standard deviation 1 was used
on the fixed effects coefficients to improve convergence and avoid
overfitting. To avoid having to refit the models each time the vignette
is built and improve reproducibility, fitted models are saved in the
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
accuracy contrasts between groups, contrasts between categories within
each group, and interaction contrasts (differences in category contrasts
between groups).

The setup of the CmdStanR back-end, marginaleffects options and the
accuracy models prior is done in the chunk below.

``` r
# if(!requireNamespace("cmdstanr", quietly = TRUE)) {
#   install.packages(
#     "cmdstanr",
#     repos = c('https://stan-dev.r-universe.dev', getOption("repos"))
#   )
# }
options("marginaleffects_safe" = FALSE)
draws <- seq(1, 2000, 1) # To limit draws that will be used for marginaleffects

prior_acc <- c(brms::prior(normal(0, 1), class = "b"))
```

### Frequentist models

Frequentist models were fitted using the
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
functions.

Finally, we tested our hypotheses with marginal contrasts. This task was
performed with the
[`report_contrast()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/report_contrast.md)
function, which is a wrapper around several functions from the *emmeans*
package[²](#fn2). We computed accuracy contrasts between groups,
contrasts between categories within each group, and interaction
contrasts (differences in category contrasts between groups).

Here we go!

## Results

### VVIQ 2 groups

#### Bayesian

``` r
mb_acc_vviq_2 <-
  fit_brms_model(
    formula = build_formula("accuracy", "group_2"),
    data    = df_expe,
    family  = brms::bernoulli(),
    prior   = prior_acc,
    file    = "models/m_acc_vviq_2.rds"
  )

# Singularity check
mb_acc_vviq_2 |> get_singularity()
# Model performance indices
mb_acc_vviq_2  |>
  get_performance(metrics = c("WAIC", "R2", "RMSE")) |>
  knitr::kable(align = "c")
```

|  WAIC  |  R2   | R2 (marg.) | RMSE  |
|:------:|:-----:|:----------:|:-----:|
| 2717.2 | 0.103 |   0.008    | 0.379 |

``` r
# Posterior predictive check (best model performance indicator)
mb_acc_vviq_2 |> 
  performance::check_predictions() |> plot() + theme_pdf(base_size = 12)
```

![Posterior predictive
distributions.](analysing_accuracy_files/figure-html/bayesian-vviq-2-groups-1.png)

``` r
# Group contrasts
mb_acc_vviq_2 |> 
  marginaleffects::avg_comparisons(
    variables = list("group_2" = "pairwise"), 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(contrast) |> knitr::kable()
```

| contrast             | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:---------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Typical - Aphantasia |    0.249 | \[-0.074, 0.571\] | 0.929 |      0.002 |       0.332 |      0.665 |

``` r
# Category contrasts within groups
mb_acc_vviq_2 |> 
  marginaleffects::avg_comparisons(
    variables = list("category" = "pairwise"), by = "group_2", 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(group_2, contrast) |> knitr::kable()
```

| group_2    | contrast          | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:-----------|:------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Aphantasia | Spatial - Control |   -0.031 | \[-0.675, 0.516\] | 0.542 |      0.304 |       0.470 |      0.226 |
| Aphantasia | Visual - Control  |   -0.004 | \[-0.593, 0.606\] | 0.505 |      0.261 |       0.476 |      0.262 |
| Aphantasia | Visual - Spatial  |    0.032 | \[-0.527, 0.636\] | 0.545 |      0.202 |       0.507 |      0.291 |
| Typical    | Spatial - Control |   -0.262 | \[-0.886, 0.311\] | 0.806 |      0.610 |       0.325 |      0.065 |
| Typical    | Visual - Control  |   -0.489 | \[-1.046, 0.098\] | 0.954 |      0.859 |       0.128 |      0.013 |
| Typical    | Visual - Spatial  |   -0.231 | \[-0.771, 0.348\] | 0.780 |      0.557 |       0.360 |      0.082 |

``` r
# Interaction contrasts
mb_acc_vviq_2 |> 
  marginaleffects::avg_comparisons(
    variables = list("category" = "pairwise"),
    by = "group_2",
    hypothesis = ~revpairwise, # for the interaction
    type = "link",
    draw_ids = draws
  ) |> 
  report_rope(hypothesis) |> knitr::kable()
```

| Category contrast | Grouping contrast    | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:------------------|:---------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Spatial - Control | Aphantasia - Typical |    0.223 | \[-0.243, 0.707\] | 0.810 |      0.044 |       0.390 |      0.566 |
| Visual - Control  | Aphantasia - Typical |    0.486 | \[0.013, 0.978\]  | 0.978 |      0.003 |       0.094 |      0.903 |
| Visual - Spatial  | Aphantasia - Typical |    0.266 | \[-0.17, 0.685\]  | 0.873 |      0.024 |       0.328 |      0.647 |

#### Frequentist

``` r
mf_acc_vviq_2 <-
  glmmTMB::glmmTMB(
    data    = df_expe,
    formula = build_formula("accuracy", "group_2"),
    family  = binomial(link = "logit"),
    prior   = set_ranef_prior(55)
  )

# Singularity
mf_acc_vviq_2 |> get_singularity()

# Performance
mf_acc_vviq_2 |> get_performance() |> knitr::kable(align = "c")
```

|  AIC   |  BIC   | R2 (cond.) | R2 (marg.) |  ICC  | RMSE  |
|:------:|:------:|:----------:|:----------:|:-----:|:-----:|
| 2887.4 | 2964.6 |   0.194    |   0.014    | 0.183 | 0.380 |

``` r
# Posterior predictive check (best model performance indicator)
mf_acc_vviq_2 |> 
  performance::check_predictions() |> plot() + theme_pdf(base_size = 12)
```

![Posterior predictive
distributions.](analysing_accuracy_files/figure-html/frequentist-vviq-2-groups-1.png)

``` r
# Group contrasts
mf_acc_vviq_2 |> report_contrast(~ group_2, ratios = FALSE) |> knitr::kable()
```

| Contrast             | Difference | 95% CI          | p.value |
|:---------------------|-----------:|:----------------|--------:|
| Typical - Aphantasia |      0.255 | \[-0.08, 0.59\] |   0.133 |

``` r
 # Category contrasts within groups
mf_acc_vviq_2 |> 
  report_contrast(~ category | group_2, ratios = FALSE) |> knitr::kable()
```

| Contrast          | group_2    | Difference | 95% CI          | p.value |
|:------------------|:-----------|-----------:|:----------------|--------:|
| Spatial - Control | Aphantasia |     -0.046 | \[-0.74, 0.64\] |   0.986 |
| Visual - Control  | Aphantasia |     -0.090 | \[-0.78, 0.6\]  |   0.950 |
| Visual - Spatial  | Aphantasia |     -0.044 | \[-0.73, 0.64\] |   0.988 |
| Spatial - Control | Typical    |     -0.293 | \[-1, 0.42\]    |   0.597 |
| Visual - Control  | Typical    |     -0.651 | \[-1.35, 0.05\] |   0.075 |
| Visual - Spatial  | Typical    |     -0.358 | \[-1.03, 0.32\] |   0.430 |

``` r
# Interaction contrasts
mf_acc_vviq_2 |> 
  report_contrast(~ category * group_2, interaction = TRUE) |>
  knitr::kable()
```

| category_revpairwise | group_2_revpairwise  | Odds ratio | 95% CI         | p.value |
|:---------------------|:---------------------|-----------:|:---------------|--------:|
| Spatial / Control    | Typical / Aphantasia |      0.781 | \[0.47, 1.28\] |   0.331 |
| Visual / Control     | Typical / Aphantasia |      0.571 | \[0.34, 0.95\] |   0.030 |
| Visual / Spatial     | Typical / Aphantasia |      0.730 | \[0.45, 1.18\] |   0.202 |

### VVIQ 3 groups

#### Bayesian

``` r
mb_acc_vviq_3 <-
  fit_brms_model(
    formula = build_formula("accuracy", "group_3"),
    data    = df_expe,
    family  = brms::bernoulli(),
    prior   = prior_acc,
    file    = "models/m_acc_vviq_3.rds"
  )

# Singularity check
mb_acc_vviq_3 |> get_singularity()
# Model performance indices
mb_acc_vviq_3  |>
  get_performance(metrics = c("WAIC", "R2", "RMSE")) |>
  knitr::kable(align = "c")
```

|  WAIC  |  R2   | R2 (marg.) | RMSE  |
|:------:|:-----:|:----------:|:-----:|
| 2720.7 | 0.105 |   0.010    | 0.378 |

``` r
# Posterior predictive check (best model performance indicator)
mb_acc_vviq_3 |> 
  performance::check_predictions() |> plot() + theme_pdf(base_size = 12)
```

![Posterior predictive
distributions.](analysing_accuracy_files/figure-html/bayesian-vviq-3-groups-1.png)

``` r
# Group contrasts
mb_acc_vviq_3 |> 
  marginaleffects::avg_comparisons(
    variables = list("group_3" = "pairwise"), 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(contrast) |> knitr::kable()
```

| contrast                   | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:---------------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Hypophantasia - Aphantasia |   -0.157 | \[-0.648, 0.334\] | 0.735 |      0.459 |       0.452 |      0.089 |
| Typical - Aphantasia       |    0.181 | \[-0.197, 0.557\] | 0.830 |      0.031 |       0.470 |      0.499 |
| Typical - Hypophantasia    |    0.335 | \[-0.087, 0.82\]  | 0.933 |      0.011 |       0.238 |      0.752 |

``` r
# Category contrasts within groups
mb_acc_vviq_3 |> 
  marginaleffects::avg_comparisons(
    variables = list("category" = "pairwise"), by = "group_3", 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(group_3, contrast) |> knitr::kable()
```

| group_3       | contrast          | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:--------------|:------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Aphantasia    | Spatial - Control |    0.059 | \[-0.604, 0.718\] | 0.576 |      0.237 |       0.404 |      0.359 |
| Aphantasia    | Visual - Control  |    0.006 | \[-0.69, 0.676\]  | 0.506 |      0.284 |       0.429 |      0.286 |
| Aphantasia    | Visual - Spatial  |   -0.056 | \[-0.695, 0.585\] | 0.572 |      0.354 |       0.424 |      0.222 |
| Hypophantasia | Spatial - Control |   -0.199 | \[-0.931, 0.55\]  | 0.699 |      0.517 |       0.320 |      0.162 |
| Hypophantasia | Visual - Control  |   -0.078 | \[-0.839, 0.613\] | 0.572 |      0.396 |       0.354 |      0.251 |
| Hypophantasia | Visual - Spatial  |    0.111 | \[-0.608, 0.821\] | 0.617 |      0.206 |       0.370 |      0.424 |
| Typical       | Spatial - Control |   -0.247 | \[-0.881, 0.361\] | 0.800 |      0.587 |       0.334 |      0.079 |
| Typical       | Visual - Control  |   -0.467 | \[-1.106, 0.117\] | 0.950 |      0.845 |       0.140 |      0.015 |
| Typical       | Visual - Spatial  |   -0.217 | \[-0.764, 0.348\] | 0.787 |      0.548 |       0.372 |      0.080 |

``` r
# Interaction contrasts
mb_acc_vviq_3 |> 
  marginaleffects::avg_comparisons(
    variables = list("category" = "pairwise"),
    by = "group_3",
    hypothesis = ~revpairwise, # for the interaction
    type = "link",
    draw_ids = draws
  ) |> 
  report_rope(hypothesis) |> knitr::kable()
```

| Category contrast | Grouping contrast          | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:------------------|:---------------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Spatial - Control | Aphantasia - Hypophantasia |    0.255 | \[-0.436, 0.921\] | 0.753 |      0.120 |       0.293 |      0.588 |
| Visual - Control  | Aphantasia - Hypophantasia |    0.084 | \[-0.614, 0.746\] | 0.592 |      0.228 |       0.392 |      0.380 |
| Visual - Spatial  | Aphantasia - Hypophantasia |   -0.177 | \[-0.838, 0.492\] | 0.695 |      0.495 |       0.352 |      0.154 |
| Spatial - Control | Aphantasia - Typical       |    0.314 | \[-0.221, 0.85\]  | 0.877 |      0.034 |       0.279 |      0.686 |
| Visual - Control  | Aphantasia - Typical       |    0.474 | \[-0.042, 0.99\]  | 0.966 |      0.005 |       0.129 |      0.866 |
| Visual - Spatial  | Aphantasia - Typical       |    0.159 | \[-0.336, 0.667\] | 0.733 |      0.086 |       0.452 |      0.463 |
| Spatial - Control | Hypophantasia - Typical    |    0.062 | \[-0.611, 0.702\] | 0.576 |      0.219 |       0.422 |      0.359 |
| Visual - Control  | Hypophantasia - Typical    |    0.397 | \[-0.201, 0.999\] | 0.884 |      0.034 |       0.216 |      0.751 |
| Visual - Spatial  | Hypophantasia - Typical    |    0.341 | \[-0.274, 0.909\] | 0.859 |      0.046 |       0.246 |      0.708 |

#### Frequentist

``` r
mf_acc_vviq_3 <-
  glmmTMB::glmmTMB(
    data    = df_expe,
    formula = build_formula("accuracy", "group_3"),
    family  = binomial(link = "logit"),
    prior   = set_ranef_prior(20)
  )

mf_acc_vviq_3 |> get_singularity()

mf_acc_vviq_3 |> get_performance() |> knitr::kable(align = "c")
```

|  AIC   |  BIC   | R2 (cond.) | R2 (marg.) |  ICC  | RMSE  |
|:------:|:------:|:----------:|:----------:|:-----:|:-----:|
| 2866.3 | 2961.3 |   0.194    |   0.015    | 0.182 | 0.380 |

``` r
# Posterior predictive check (best model performance indicator)
mf_acc_vviq_3 |> 
  performance::check_predictions() |> plot() + theme_pdf(base_size = 12)
```

![Posterior predictive
distributions.](analysing_accuracy_files/figure-html/frequentist-vviq-3-groups-1.png)

``` r
mf_acc_vviq_3 |> report_contrast(~ group_3, ratios = FALSE) |> knitr::kable()
```

| Contrast                   | Difference | 95% CI          | p.value |
|:---------------------------|-----------:|:----------------|--------:|
| Hypophantasia - Aphantasia |     -0.174 | \[-0.77, 0.42\] |   0.774 |
| Typical - Aphantasia       |      0.190 | \[-0.26, 0.64\] |   0.590 |
| Typical - Hypophantasia    |      0.363 | \[-0.18, 0.91\] |   0.264 |

``` r
mf_acc_vviq_3 |> report_contrast(~ category | group_3, ratios = FALSE) |> knitr::kable()
```

| Contrast          | group_3       | Difference | 95% CI          | p.value |
|:------------------|:--------------|-----------:|:----------------|--------:|
| Spatial - Control | Aphantasia    |      0.046 | \[-0.72, 0.81\] |   0.989 |
| Visual - Control  | Aphantasia    |     -0.076 | \[-0.85, 0.69\] |   0.971 |
| Visual - Spatial  | Aphantasia    |     -0.122 | \[-0.89, 0.64\] |   0.925 |
| Spatial - Control | Hypophantasia |     -0.194 | \[-1.06, 0.67\] |   0.859 |
| Visual - Control  | Hypophantasia |     -0.107 | \[-1, 0.78\]    |   0.957 |
| Visual - Spatial  | Hypophantasia |      0.086 | \[-0.78, 0.95\] |   0.970 |
| Spatial - Control | Typical       |     -0.293 | \[-1, 0.42\]    |   0.597 |
| Visual - Control  | Typical       |     -0.648 | \[-1.35, 0.05\] |   0.076 |
| Visual - Spatial  | Typical       |     -0.355 | \[-1.03, 0.32\] |   0.435 |

``` r
mf_acc_vviq_3 |> report_contrast(~ category * group_3, interaction = TRUE) |>
  knitr::kable()
```

| category_revpairwise | group_3_revpairwise        | Odds ratio | 95% CI         | p.value |
|:---------------------|:---------------------------|-----------:|:---------------|--------:|
| Spatial / Control    | Hypophantasia / Aphantasia |      0.787 | \[0.38, 1.61\] |   0.511 |
| Visual / Control     | Hypophantasia / Aphantasia |      0.969 | \[0.46, 2.05\] |   0.934 |
| Visual / Spatial     | Hypophantasia / Aphantasia |      1.232 | \[0.6, 2.54\]  |   0.572 |
| Spatial / Control    | Typical / Aphantasia       |      0.712 | \[0.4, 1.25\]  |   0.240 |
| Visual / Control     | Typical / Aphantasia       |      0.564 | \[0.32, 1.01\] |   0.052 |
| Visual / Spatial     | Typical / Aphantasia       |      0.792 | \[0.45, 1.38\] |   0.412 |
| Spatial / Control    | Typical / Hypophantasia    |      0.906 | \[0.46, 1.76\] |   0.771 |
| Visual / Control     | Typical / Hypophantasia    |      0.582 | \[0.29, 1.16\] |   0.125 |
| Visual / Spatial     | Typical / Hypophantasia    |      0.643 | \[0.33, 1.24\] |   0.188 |

### OSIVQ 3 clusters

#### Bayesian

``` r
mb_acc_osivq <-
  fit_brms_model(
    formula = build_formula("accuracy", "cluster"),
    data    = df_expe,
    family  = brms::bernoulli(),
    prior   = prior_acc,
    file    = "models/m_acc_osivq.rds"
  )

# Singularity check
mb_acc_osivq |> get_singularity()
# Model performance indices
mb_acc_osivq  |>
  get_performance(metrics = c("WAIC", "R2", "RMSE")) |>
  knitr::kable(align = "c")
```

|  WAIC  |  R2   | R2 (marg.) | RMSE  |
|:------:|:-----:|:----------:|:-----:|
| 2724.1 | 0.104 |   0.008    | 0.378 |

``` r
# Posterior predictive check (best model performance indicator)
mb_acc_osivq |> 
  performance::check_predictions() |> plot() + theme_pdf(base_size = 12)
```

![Posterior predictive
distributions.](analysing_accuracy_files/figure-html/bayesian-osivq-3-clusters-1.png)

``` r
# Group contrasts
mb_acc_osivq |> 
  marginaleffects::avg_comparisons(
    variables = list("cluster" = "pairwise"), 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(contrast) |> knitr::kable()
```

| contrast                 | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:-------------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Spatialiser - Visualiser |   -0.075 | \[-0.524, 0.382\] | 0.625 |      0.324 |       0.539 |      0.136 |
| Verbaliser - Spatialiser |   -0.137 | \[-0.594, 0.326\] | 0.721 |      0.420 |       0.499 |      0.081 |
| Verbaliser - Visualiser  |   -0.208 | \[-0.571, 0.145\] | 0.884 |      0.562 |       0.422 |      0.016 |

``` r
# Category contrasts within groups
mb_acc_osivq |> 
  marginaleffects::avg_comparisons(
    variables = list("category" = "pairwise"), by = "cluster", 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(cluster, contrast) |> knitr::kable()
```

| cluster     | contrast          | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:------------|:------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Visualiser  | Spatial - Control |   -0.292 | \[-0.92, 0.324\]  | 0.829 |      0.638 |       0.293 |      0.068 |
| Visualiser  | Visual - Control  |   -0.467 | \[-1.059, 0.113\] | 0.943 |      0.835 |       0.153 |      0.013 |
| Visualiser  | Visual - Spatial  |   -0.178 | \[-0.747, 0.439\] | 0.733 |      0.494 |       0.390 |      0.116 |
| Spatialiser | Spatial - Control |    0.029 | \[-0.744, 0.83\]  | 0.520 |      0.300 |       0.354 |      0.346 |
| Spatialiser | Visual - Control  |   -0.232 | \[-0.985, 0.495\] | 0.741 |      0.553 |       0.322 |      0.124 |
| Spatialiser | Visual - Spatial  |   -0.271 | \[-0.976, 0.473\] | 0.757 |      0.591 |       0.290 |      0.120 |
| Verbaliser  | Spatial - Control |   -0.078 | \[-0.72, 0.543\]  | 0.601 |      0.378 |       0.420 |      0.203 |
| Verbaliser  | Visual - Control  |   -0.110 | \[-0.728, 0.52\]  | 0.634 |      0.398 |       0.451 |      0.150 |
| Verbaliser  | Visual - Spatial  |   -0.040 | \[-0.64, 0.597\]  | 0.553 |      0.302 |       0.451 |      0.246 |

``` r
# Interaction contrasts
mb_acc_osivq |> 
  marginaleffects::avg_comparisons(
    variables = list("category" = "pairwise"),
    by = "cluster",
    hypothesis = ~revpairwise, # for the interaction
    type = "link",
    draw_ids = draws
  ) |> 
  report_rope(hypothesis) |> knitr::kable()
```

| Category contrast | Grouping contrast        | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:------------------|:-------------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Spatial - Control | Spatialiser - Verbaliser |    0.120 | \[-0.581, 0.769\] | 0.620 |      0.210 |       0.366 |      0.425 |
| Visual - Control  | Spatialiser - Verbaliser |   -0.137 | \[-0.795, 0.532\] | 0.660 |      0.438 |       0.392 |      0.170 |
| Visual - Spatial  | Spatialiser - Verbaliser |   -0.249 | \[-0.887, 0.398\] | 0.766 |      0.579 |       0.322 |      0.098 |
| Spatial - Control | Visualiser - Spatialiser |   -0.311 | \[-1.018, 0.363\] | 0.830 |      0.647 |       0.282 |      0.071 |
| Visual - Control  | Visualiser - Spatialiser |   -0.223 | \[-0.837, 0.393\] | 0.757 |      0.550 |       0.350 |      0.100 |
| Visual - Spatial  | Visualiser - Spatialiser |    0.083 | \[-0.493, 0.699\] | 0.609 |      0.188 |       0.410 |      0.402 |
| Spatial - Control | Visualiser - Verbaliser  |   -0.211 | \[-0.722, 0.321\] | 0.797 |      0.544 |       0.384 |      0.072 |
| Visual - Control  | Visualiser - Verbaliser  |   -0.360 | \[-0.836, 0.159\] | 0.919 |      0.753 |       0.226 |      0.021 |
| Visual - Spatial  | Visualiser - Verbaliser  |   -0.143 | \[-0.634, 0.332\] | 0.707 |      0.441 |       0.464 |      0.094 |

#### Frequentist

``` r
mf_acc_osivq <-
  glmmTMB::glmmTMB(
    data    = df_expe,
    formula = build_formula("accuracy", "cluster"),
    family  = binomial(link = "logit"),
    prior   = set_ranef_prior(15)
  )

mf_acc_osivq  |> get_singularity()

mf_acc_osivq  |> get_performance() |> knitr::kable(align = "c")
```

|  AIC   |  BIC   | R2 (cond.) | R2 (marg.) |  ICC  | RMSE  |
|:------:|:------:|:----------:|:----------:|:-----:|:-----:|
| 2862.0 | 2957.1 |   0.192    |   0.011    | 0.183 | 0.380 |

``` r
# Posterior predictive check (best model performance indicator)
mf_acc_osivq |> 
  performance::check_predictions() |> plot() + theme_pdf(base_size = 12)
```

![Posterior predictive
distributions.](analysing_accuracy_files/figure-html/frequentist-osivq-3-clusters-1.png)

``` r
mf_acc_osivq  |> report_contrast(~ cluster, ratios = FALSE) |> knitr::kable()
```

| Contrast                 | Difference | 95% CI          | p.value |
|:-------------------------|-----------:|:----------------|--------:|
| Spatialiser - Visualiser |     -0.089 | \[-0.65, 0.47\] |   0.926 |
| Verbaliser - Visualiser  |     -0.216 | \[-0.65, 0.22\] |   0.478 |
| Verbaliser - Spatialiser |     -0.127 | \[-0.68, 0.43\] |   0.854 |

``` r
mf_acc_osivq  |> report_contrast(~ category | cluster, ratios = FALSE) |> knitr::kable()
```

| Contrast          | Cluster     | Difference | 95% CI          | p.value |
|:------------------|:------------|-----------:|:----------------|--------:|
| Spatial - Control | Visualiser  |     -0.329 | \[-1.07, 0.41\] |   0.551 |
| Visual - Control  | Visualiser  |     -0.618 | \[-1.35, 0.12\] |   0.121 |
| Visual - Spatial  | Visualiser  |     -0.288 | \[-1, 0.42\]    |   0.608 |
| Spatial - Control | Spatialiser |      0.011 | \[-0.89, 0.91\] |   1.000 |
| Visual - Control  | Spatialiser |     -0.340 | \[-1.23, 0.55\] |   0.643 |
| Visual - Spatial  | Spatialiser |     -0.351 | \[-1.23, 0.53\] |   0.618 |
| Spatial - Control | Verbaliser  |     -0.098 | \[-0.8, 0.61\]  |   0.944 |
| Visual - Control  | Verbaliser  |     -0.177 | \[-0.89, 0.54\] |   0.829 |
| Visual - Spatial  | Verbaliser  |     -0.079 | \[-0.78, 0.62\] |   0.962 |

``` r
mf_acc_osivq  |> report_contrast(~ category * cluster, interaction = TRUE) |>
  knitr::kable()
```

| category_revpairwise | cluster_revpairwise      | Odds ratio | 95% CI         | p.value |
|:---------------------|:-------------------------|-----------:|:---------------|--------:|
| Spatial / Control    | Spatialiser / Visualiser |      1.405 | \[0.7, 2.84\]  |   0.344 |
| Visual / Control     | Spatialiser / Visualiser |      1.320 | \[0.65, 2.68\] |   0.444 |
| Visual / Spatial     | Spatialiser / Visualiser |      0.939 | \[0.47, 1.87\] |   0.858 |
| Spatial / Control    | Verbaliser / Visualiser  |      1.260 | \[0.73, 2.18\] |   0.407 |
| Visual / Control     | Verbaliser / Visualiser  |      1.553 | \[0.89, 2.72\] |   0.123 |
| Visual / Spatial     | Verbaliser / Visualiser  |      1.232 | \[0.72, 2.1\]  |   0.442 |
| Spatial / Control    | Verbaliser / Spatialiser |      0.897 | \[0.45, 1.79\] |   0.758 |
| Visual / Control     | Verbaliser / Spatialiser |      1.177 | \[0.58, 2.37\] |   0.648 |
| Visual / Spatial     | Verbaliser / Spatialiser |      1.312 | \[0.66, 2.6\]  |   0.437 |

### Strategy groups

#### Bayesian

``` r
mb_acc_strat <-
  fit_brms_model(
    formula = build_formula("accuracy", "strategy_group"),
    data    = df_expe,
    family  = brms::bernoulli(),
    prior   = prior_acc,
    file    = "models/m_acc_strat.rds"
  )

# Singularity check
mb_acc_strat |> get_singularity()
# Model performance indices
mb_acc_strat  |>
  get_performance(metrics = c("WAIC", "R2", "RMSE")) |>
  knitr::kable(align = "c")
```

|  WAIC  |  R2   | R2 (marg.) | RMSE  |
|:------:|:-----:|:----------:|:-----:|
| 2721.6 | 0.102 |   0.006    | 0.379 |

``` r
# Posterior predictive check (best model performance indicator)
mb_acc_strat |> 
  performance::check_predictions() |> plot() + theme_pdf(base_size = 12)
```

![Posterior predictive
distributions.](analysing_accuracy_files/figure-html/bayesian-strategy-groups-1.png)

``` r
# Group contrasts
mb_acc_strat |> 
  marginaleffects::avg_comparisons(
    variables = list("strategy_group" = "pairwise"), 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(contrast) |> knitr::kable()
```

| contrast                                  | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:------------------------------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| No_visual_strategy - Visual_strategy_user |    0.006 | \[-0.347, 0.328\] | 0.518 |      0.128 |       0.723 |       0.15 |

``` r
# Category contrasts within groups
mb_acc_strat |> 
  marginaleffects::avg_comparisons(
    variables = list("category" = "pairwise"), by = "strategy_group", 
    type = "link",
    draw_ids = draws
  ) |>
  report_rope(strategy_group, contrast) |> knitr::kable()
```

| strategy_group       | contrast          | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:---------------------|:------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Visual_strategy_user | Spatial - Control |   -0.297 | \[-0.868, 0.281\] | 0.837 |      0.638 |       0.312 |      0.050 |
| Visual_strategy_user | Visual - Control  |   -0.349 | \[-0.943, 0.201\] | 0.895 |      0.730 |       0.241 |      0.029 |
| Visual_strategy_user | Visual - Spatial  |   -0.065 | \[-0.597, 0.486\] | 0.585 |      0.332 |       0.482 |      0.187 |
| No_visual_strategy   | Spatial - Control |   -0.014 | \[-0.602, 0.61\]  | 0.519 |      0.284 |       0.450 |      0.266 |
| No_visual_strategy   | Visual - Control  |   -0.227 | \[-0.811, 0.364\] | 0.764 |      0.554 |       0.354 |      0.092 |
| No_visual_strategy   | Visual - Spatial  |   -0.215 | \[-0.796, 0.385\] | 0.770 |      0.544 |       0.365 |      0.091 |

``` r
# Interaction contrasts
mb_acc_strat |> 
  marginaleffects::avg_comparisons(
    variables = list("category" = "pairwise"),
    by = "strategy_group",
    hypothesis = ~revpairwise, # for the interaction
    type = "link",
    draw_ids = draws
  ) |> 
  report_rope(hypothesis) |> knitr::kable()
```

| Category contrast | Grouping contrast                         | Estimate | 95% CI            |    PD | Below ROPE | Inside ROPE | Above ROPE |
|:------------------|:------------------------------------------|---------:|:------------------|------:|-----------:|------------:|-----------:|
| Spatial - Control | Visual_strategy_user - No_visual_strategy |   -0.282 | \[-0.77, 0.201\]  | 0.874 |      0.651 |       0.319 |      0.030 |
| Visual - Control  | Visual_strategy_user - No_visual_strategy |   -0.133 | \[-0.577, 0.34\]  | 0.713 |      0.416 |       0.492 |      0.092 |
| Visual - Spatial  | Visual_strategy_user - No_visual_strategy |    0.153 | \[-0.302, 0.608\] | 0.749 |      0.081 |       0.470 |      0.448 |

#### Frequentist

``` r
mf_acc_strat <-
  glmmTMB::glmmTMB(
    data    = df_expe,
    formula = build_formula("accuracy", "strategy_group"),
    family  = binomial(link = "logit"),
    prior   = set_ranef_prior(55)
  )

mf_acc_strat |> get_singularity()

mf_acc_strat |> get_performance() |> knitr::kable(align = "c")
```

|  AIC   |  BIC   | R2 (cond.) | R2 (marg.) |  ICC  | RMSE  |
|:------:|:------:|:----------:|:----------:|:-----:|:-----:|
| 2891.5 | 2968.7 |   0.191    |   0.007    | 0.185 | 0.380 |

``` r
# Posterior predictive check (best model performance indicator)
mf_acc_strat |> 
  performance::check_predictions() |> plot() + theme_pdf(base_size = 12)
```

![Posterior predictive
distributions.](analysing_accuracy_files/figure-html/frequentist-strategy-groups-1.png)

``` r
mf_acc_strat |> report_contrast(~ strategy_group, ratios = FALSE) |> knitr::kable()
```

| Contrast                                  | Difference | 95% CI          | p.value |
|:------------------------------------------|-----------:|:----------------|--------:|
| No_visual_strategy - Visual_strategy_user |      0.013 | \[-0.32, 0.34\] |   0.941 |

``` r
mf_acc_strat |> report_contrast(~ category | strategy_group, ratios = FALSE) |> knitr::kable()
```

| Contrast          | strategy_group | Difference | 95% CI          | p.value |
|:------------------|:---------------|-----------:|:----------------|--------:|
| Spatial - Control | Visualegy_user |     -0.317 | \[-1.01, 0.38\] |   0.533 |
| Visual - Control  | Visualegy_user |     -0.458 | \[-1.15, 0.24\] |   0.271 |
| Visual - Spatial  | Visualegy_user |     -0.141 | \[-0.81, 0.53\] |   0.876 |
| Spatial - Control | No_visualegy   |     -0.019 | \[-0.72, 0.68\] |   0.998 |
| Visual - Control  | No_visualegy   |     -0.317 | \[-1.02, 0.38\] |   0.537 |
| Visual - Spatial  | No_visualegy   |     -0.298 | \[-0.99, 0.39\] |   0.568 |

``` r
mf_acc_strat |> report_contrast(~ category * strategy_group, interaction = TRUE) |>
  knitr::kable()
```

| category_revpairwise | strategy_group_revpairwise    | Odds ratio | 95% CI         | p.value |
|:---------------------|:------------------------------|-----------:|:---------------|--------:|
| Spatial / Control    | No_visualegy / Visualegy_user |      1.347 | \[0.82, 2.21\] |   0.237 |
| Visual / Control     | No_visualegy / Visualegy_user |      1.151 | \[0.69, 1.91\] |   0.585 |
| Visual / Spatial     | No_visualegy / Visualegy_user |      0.854 | \[0.53, 1.38\] |   0.522 |

## Visualisation

The figures showing the distribution of accuracy data displayed in the
manuscript were created with the
[`plot_superb_jitter()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_superb_raincloud.md)
function from the package, which wraps functions from the *ggplot2* and
*superb* packages to create nice visualisations easily. A little
[`add_significance()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_significance.md)
helper function was also created to add significance stars to the plots.

``` r
library(patchwork)
library(superb)

pa1 <-
  plot_superb_jitter(
    df_expe, accuracy, group_2,
    title = "VVIQ 2 groups", y_title = "Mean accuracy",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    axis_rel = 1.2
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 2,
      y_star = 1.07,
      stars  = "*",
      x_line = .data$x_star - 0.16,
      x_line_end = .data$x_star + 0.16,
      y_line = 1.05
    )
  )

pa2 <-
  plot_superb_jitter(
    df_expe, accuracy, group_3,
    title = "VVIQ 3 groups", y_title = "Mean accuracy",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    axis_rel = 1.2
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3,
      y_star = 1.07,
      stars  = "*",
      x_line = .data$x_star - 0.16,
      x_line_end = .data$x_star + 0.16,
      y_line = 1.05
    )
  )

pa3 <-
  plot_superb_jitter(
    df_expe, accuracy, cluster,
    title = "OSIVQ clusters", y_title = "Mean accuracy",
    base_size = 12,
    plot.background = ggplot2::element_rect(fill = "white", colour = NA),
    axis_rel = 1.2
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3,
      y_star = 1.065,
      stars  = "°",
      x_line = .data$x_star - 0.16,
      x_line_end = .data$x_star + 0.16,
      y_line = 1.05
    )
  )

pa <- pa1 + pa2 + pa3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

plot(pa)
```

![A jitter plot showing the mean accuracy in each category for several
different grouping
variables.](analysing_accuracy_files/figure-html/plot-accuracy-1.png)

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
    #>  date     2025-12-10
    #>  pandoc   3.1.11 @ /opt/hostedtoolcache/pandoc/3.1.11/x64/ (via rmarkdown)
    #>  quarto   1.8.26 @ /usr/local/bin/quarto
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  ! package                 * version    date (UTC) lib source
    #>    abind                     1.4-8      2024-09-12 [1] RSPM
    #>    aphantasiaReasoningViie * 1.0        2025-12-10 [1] local
    #>    assertthat                0.2.1      2019-03-21 [1] RSPM
    #>    backports                 1.5.0      2024-05-23 [1] RSPM
    #>    bayesplot                 1.14.0     2025-08-31 [1] RSPM
    #>    bayestestR                0.17.0     2025-08-29 [1] RSPM
    #>  P boot                      1.3-32     2025-08-29 [?] CRAN (R 4.5.2)
    #>    bridgesampling            1.2-1      2025-11-19 [1] RSPM
    #>    brms                      2.23.0     2025-09-09 [1] RSPM
    #>    Brobdingnag               1.2-9      2022-10-19 [1] RSPM
    #>    bslib                     0.9.0      2025-01-30 [1] RSPM
    #>    cachem                    1.1.0      2024-05-16 [1] RSPM
    #>    checkmate                 2.3.3      2025-08-18 [1] RSPM
    #>  P class                     7.3-23     2025-01-01 [?] CRAN (R 4.5.2)
    #>    cli                       3.6.5      2025-04-23 [1] RSPM
    #>    clue                      0.3-66     2024-11-13 [1] RSPM
    #>  P cluster                   2.1.8.1    2025-03-12 [?] CRAN (R 4.5.2)
    #>    clusterCrit               1.3.0      2023-11-23 [1] RSPM
    #>    clValid                   0.7        2021-02-14 [1] RSPM
    #>    coda                      0.19-4.1   2024-01-31 [1] RSPM
    #>  P codetools                 0.2-20     2024-03-31 [?] CRAN (R 4.5.2)
    #>    collapse                  2.1.5      2025-11-19 [1] RSPM
    #>    combinat                  0.0-8      2012-10-29 [1] RSPM
    #>    crayon                    1.5.3      2024-06-20 [1] RSPM
    #>    curl                      7.0.0      2025-08-19 [1] RSPM
    #>    data.table                1.17.8     2025-07-10 [1] RSPM
    #>    datawizard                1.3.0      2025-10-11 [1] RSPM
    #>    desc                      1.4.3      2023-12-10 [1] RSPM
    #>  P devtools                * 2.4.6      2025-10-03 [?] RSPM
    #>    diceR                     3.1.0      2025-06-19 [1] RSPM
    #>    digest                    0.6.39     2025-11-19 [1] RSPM
    #>    distributional            0.5.0      2024-09-17 [1] RSPM
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
    #>    Formula                   1.2-5      2023-02-24 [1] RSPM
    #>    fs                        1.6.6      2025-04-12 [1] RSPM
    #>    generics                  0.1.4      2025-05-09 [1] RSPM
    #>    ggplot2                   4.0.1      2025-11-14 [1] RSPM
    #>    glmmTMB                   1.1.13     2025-10-09 [1] RSPM
    #>    glue                      1.8.0      2024-09-30 [1] RSPM
    #>    gridExtra                 2.3        2017-09-09 [1] RSPM
    #>    gtable                    0.3.6      2024-10-25 [1] RSPM
    #>    haven                     2.5.5      2025-05-30 [1] RSPM
    #>    highr                     0.11       2024-05-26 [1] RSPM
    #>    hms                       1.1.4      2025-10-17 [1] RSPM
    #>    htmltools                 0.5.9      2025-12-04 [1] RSPM
    #>    htmlwidgets               1.6.4      2023-12-06 [1] RSPM
    #>    httpuv                    1.6.16     2025-04-16 [1] RSPM
    #>    inline                    0.3.21     2025-01-09 [1] RSPM
    #>    insight                   1.4.4      2025-12-06 [1] RSPM
    #>    jquerylib                 0.1.4      2021-04-26 [1] RSPM
    #>    jsonlite                  2.0.0      2025-03-27 [1] RSPM
    #>    klaR                      1.7-3      2023-12-13 [1] RSPM
    #>    knitr                     1.50       2025-03-16 [1] RSPM
    #>    labeling                  0.4.3      2023-08-29 [1] RSPM
    #>    labelled                  2.16.0     2025-10-22 [1] RSPM
    #>    later                     1.4.4      2025-08-27 [1] RSPM
    #>  P lattice                   0.22-7     2025-04-02 [?] CRAN (R 4.5.2)
    #>    lifecycle                 1.0.4      2023-11-07 [1] RSPM
    #>    lme4                      1.1-38     2025-12-02 [1] RSPM
    #>    loo                       2.8.0      2024-07-03 [1] RSPM
    #>    lsr                       0.5.2      2021-12-01 [1] RSPM
    #>    magrittr                  2.0.4      2025-09-12 [1] RSPM
    #>    marginaleffects           0.31.0     2025-11-15 [1] RSPM
    #>  P MASS                      7.3-65     2025-02-28 [?] CRAN (R 4.5.2)
    #>  P Matrix                    1.7-4      2025-08-28 [?] CRAN (R 4.5.2)
    #>    matrixStats               1.5.0      2025-01-07 [1] RSPM
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
    #>    performance               0.15.3     2025-12-01 [1] RSPM
    #>    pillar                    1.11.1     2025-09-17 [1] RSPM
    #>    pkgbuild                  1.4.8      2025-05-26 [1] RSPM
    #>    pkgconfig                 2.0.3      2019-09-22 [1] RSPM
    #>    pkgdown                   2.2.0      2025-11-06 [1] any (@2.2.0)
    #>    pkgload                   1.4.1      2025-09-23 [1] RSPM
    #>    plyr                      1.8.9      2023-10-02 [1] RSPM
    #>    posterior                 1.6.1      2025-02-27 [1] RSPM
    #>    promises                  1.5.0      2025-11-01 [1] RSPM
    #>    proxy                     0.4-27     2022-06-09 [1] RSPM
    #>    purrr                     1.2.0      2025-11-04 [1] RSPM
    #>    questionr                 0.8.1      2025-06-10 [1] RSPM
    #>    QuickJSR                  1.8.1      2025-09-20 [1] RSPM
    #>    R6                        2.6.1      2025-02-15 [1] RSPM
    #>    ragg                      1.5.0      2025-09-02 [1] RSPM
    #>    rbibutils                 2.4        2025-11-07 [1] RSPM
    #>    RColorBrewer              1.1-3      2022-04-03 [1] RSPM
    #>    Rcpp                      1.1.0      2025-07-02 [1] RSPM
    #>    RcppParallel              5.1.11-1   2025-08-27 [1] RSPM
    #>    Rdpack                    2.6.4      2025-04-09 [1] RSPM
    #>    reformulas                0.4.2      2025-10-28 [1] RSPM
    #>  P remotes                   2.5.0      2024-03-17 [?] RSPM
    #>    renv                      1.1.4      2025-03-20 [1] RSPM (R 4.5.0)
    #>    reshape2                  1.4.5      2025-11-12 [1] RSPM
    #>    rlang                     1.1.6      2025-04-11 [1] RSPM
    #>    rmarkdown                 2.30       2025-09-28 [1] RSPM
    #>    rrapply                   1.2.8      2025-11-25 [1] RSPM
    #>    rstan                     2.32.7     2025-03-10 [1] RSPM
    #>    rstantools                2.5.0      2025-09-01 [1] RSPM
    #>    rstudioapi                0.17.1     2024-10-22 [1] RSPM
    #>    S7                        0.2.1      2025-11-14 [1] RSPM
    #>    sandwich                  3.1-1      2024-09-15 [1] RSPM
    #>    sass                      0.4.10     2025-04-11 [1] RSPM
    #>    scales                    1.4.0      2025-04-24 [1] RSPM
    #>    see                       0.12.0     2025-09-14 [1] RSPM
    #>    sessioninfo               1.2.3      2025-02-05 [1] RSPM
    #>    shiny                     1.12.0     2025-12-03 [1] RSPM
    #>    shinyBS                   0.61.1     2022-04-17 [1] RSPM
    #>    showtext                  0.9-7      2024-03-02 [1] RSPM
    #>    showtextdb                3.0        2020-06-04 [1] RSPM
    #>    StanHeaders               2.32.10    2024-07-15 [1] RSPM
    #>    stringi                   1.8.7      2025-03-27 [1] RSPM
    #>    stringr                   1.6.0      2025-11-04 [1] RSPM
    #>    superb                  * 1.0.1      2025-12-04 [1] RSPM
    #>    sysfonts                  0.8.9      2024-03-02 [1] RSPM
    #>    systemfonts               1.3.1      2025-10-01 [1] RSPM
    #>    tensorA                   0.36.2.1   2023-12-13 [1] RSPM
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
    #>    yaml                      2.3.11     2025-11-28 [1] RSPM
    #>    zoo                       1.8-14     2025-04-10 [1] RSPM
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

Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., & Lüdecke, D. (2019).
Indices of Effect Existence and Significance in the Bayesian Framework.
*Frontiers in Psychology*, *10*.
<https://doi.org/10.3389/fpsyg.2019.02767>

McGillycuddy, M., Warton, D. I., Popovic, G., & Bolker, B. M. (2025).
Parsimoniously fitting large multivariate random effects in glmmTMB.
*Journal of Statistical Software*, *112*(1), 1–19.
<https://doi.org/10.18637/jss.v112.i01>

------------------------------------------------------------------------

1.  This specific number was chosen arbitrarily to have an even number
    of iterations on each chain on the two computers that were used for
    the analyses, one with 20 cores and one with 24 cores. The only
    important aspect here is to have enough iterations to ensure
    convergence and a good effective sample size.

2.  The marginaleffects package was not used for frequentist analyses
    because it conflicted with glmmTMB models, whereas the emmeans
    package worked fine.
