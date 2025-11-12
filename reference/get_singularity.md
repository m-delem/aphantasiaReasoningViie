# Check if the model is singular and print a message

Check if the model is singular and print a message

## Usage

``` r
get_singularity(model)
```

## Arguments

- model:

  A fitted model object.

## Value

Nothing. Prints a message indicating whether the model is singular or
not.

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

 get_singularity(model)
}
#> The model is not singular, parameter estimates are trustworthy.
```
