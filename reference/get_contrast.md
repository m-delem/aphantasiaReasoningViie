# Get the pairwise contrasts of variables in a model

Get the pairwise contrasts of variables in a model

## Usage

``` r
get_contrast(model, formula, at = NULL, ...)
```

## Arguments

- model:

  A fitted model object.

- formula:

  A formula specifying the variables for which to get the contrasts. See
  [`?emmeans::emmeans`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  for details.

- at:

  Optional. A list of values at which to evaluate the contrasts.

- ...:

  Additional arguments passed to
  [`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html).

## Value

An emm_grid object with the pairwise contrasts of the specified
variables.

## Examples

``` r
df_expe <- get_clean_data()$df_expe

if (require("glmmTMB", quietly = TRUE)) {
 model <- glmmTMB::glmmTMB(
  formula = accuracy ~ group_2 * category + (1 | id),
  data = df_expe,
  family = binomial(link = "logit"),
  prior = set_ranef_prior(65)
 )

 get_contrast(model, ~ category | group_2)
}
#> group_2 = Aphantasia:
#>  contrast          odds.ratio    SE  df null z.ratio p.value
#>  Control / Spatial       1.00 0.169 Inf    1   0.000  1.0000
#>  Control / Visual        1.00 0.169 Inf    1   0.000  1.0000
#>  Spatial / Visual        1.00 0.169 Inf    1   0.000  1.0000
#> 
#> group_2 = Typical:
#>  contrast          odds.ratio    SE  df null z.ratio p.value
#>  Control / Spatial       1.22 0.205 Inf    1   1.176  0.4676
#>  Control / Visual        1.60 0.262 Inf    1   2.890  0.0108
#>  Spatial / Visual        1.32 0.208 Inf    1   1.731  0.1935
#> 
#> P value adjustment: tukey method for comparing a family of 3 estimates 
#> Tests are performed on the log odds ratio scale 
```
