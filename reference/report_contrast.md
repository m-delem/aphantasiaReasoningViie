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
#> 1 Spatial / Control Aphantasia      1.000 [0.67, 1.48]   1.000
#> 2  Visual / Control Aphantasia      1.000 [0.67, 1.48]   1.000
#> 3  Visual / Spatial Aphantasia      1.000 [0.67, 1.48]   1.000
#> 4 Spatial / Control    Typical      0.820 [0.55, 1.22]   0.468
#> 5  Visual / Control    Typical      0.624 [0.43, 0.91]   0.011
#> 6  Visual / Spatial    Typical      0.760  [0.52, 1.1]   0.194
```
