# Get the pairwise contrasts of variables in a model

Get the pairwise contrasts of variables in a model

## Usage

``` r
get_contrast(
  model,
  formula,
  type = "response",
  at = NULL,
  method = "revpairwise",
  ...
)
```

## Arguments

- model:

  A fitted model object.

- formula:

  A formula specifying the variables for which to get the contrasts. See
  [`?emmeans::emmeans`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  for details.

- type:

  Type of response to be returned. Default is "response".

- at:

  Optional. A list of values at which to evaluate the contrasts.

- method:

  Method for computing the contrasts. Default is "revpairwise".

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
#>  Spatial / Control      1.000 0.169 Inf    1   0.000  1.0000
#>  Visual / Control       1.000 0.169 Inf    1   0.000  1.0000
#>  Visual / Spatial       1.000 0.169 Inf    1   0.000  1.0000
#> 
#> group_2 = Typical:
#>  contrast          odds.ratio    SE  df null z.ratio p.value
#>  Spatial / Control      0.820 0.138 Inf    1  -1.176  0.4676
#>  Visual / Control       0.624 0.102 Inf    1  -2.890  0.0108
#>  Visual / Spatial       0.760 0.120 Inf    1  -1.731  0.1935
#> 
#> P value adjustment: tukey method for comparing a family of 3 estimates 
#> Tests are performed on the log odds ratio scale 
```
