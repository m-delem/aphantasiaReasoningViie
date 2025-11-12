# Get the contrasts of a model and format them for reporting

Get the contrasts of a model and format them for reporting

## Usage

``` r
report_contrast(model, formula, ...)
```

## Arguments

- model:

  A fitted model object.

- formula:

  A formula specifying the variables for which to get the contrasts. See
  [`?emmeans::emmeans`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  for details.

- ...:

  Additional arguments passed to
  [`get_contrast()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_contrast.md).

## Value

A data frame with the pairwise contrasts of the specified variables
formatted for reporting.

## Examples

``` r
df_expe <- get_clean_data()$df_expe

if (require("glmmTMB", quietly = TRUE)) {
 model <- glmmTMB::glmmTMB(
  formula = accuracy ~ group_2 * category + (1 | id),
  data = df_expe,
  family = binomial(link = "logit"),
  prior = set_ranef_prior(20)
 )

 report_contrast(model, ~ category | group_2)
}
#>            Contrast    group_2 Odds ratio           CI p.value
#> 1 Control / Spatial Aphantasia      1.000 [0.67, 1.48]   1.000
#> 2  Control / Visual Aphantasia      1.000 [0.67, 1.48]   1.000
#> 3  Spatial / Visual Aphantasia      1.000 [0.67, 1.48]   1.000
#> 4 Control / Spatial    Typical      1.219 [0.82, 1.81]   0.468
#> 5  Control / Visual    Typical      1.604 [1.09, 2.35]   0.011
#> 6  Spatial / Visual    Typical      1.315 [0.91, 1.91]   0.194
```
