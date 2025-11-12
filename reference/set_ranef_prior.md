# Create a weakly informative regularizing Gamma prior for the random effects

Create a weakly informative regularizing Gamma prior for the random
effects

## Usage

``` r
set_ranef_prior(gamma_mean = 100)
```

## Arguments

- gamma_mean:

  A numeric value indicating the mean of the Gamma prior. The higher the
  value, the less impact the prior has, but also the more chance of
  singularity.

## Value

A data frame with the prior and class for the random effects.

## Examples

``` r
set_ranef_prior(100)
#>             prior class
#> 1 gamma(100, 2.5) ranef
```
