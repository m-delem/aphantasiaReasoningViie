# Add treatment contrasts to a factor

Add treatment contrasts to a factor

## Usage

``` r
add_factor_contrasts(vect, n = levels(vect), ...)
```

## Arguments

- vect:

  A factor vector to which contrasts will be added.

- n:

  A vector of levels for the factor. Default is the levels of `vect`.

- ...:

  Additional arguments passed to `contr.treatment`.

## Value

A factor vector with treatment contrasts added.
