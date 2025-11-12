# Convert the `category` column to a factor with contrasts

Convert the `category` column to a factor with contrasts

## Usage

``` r
factor_categories(df, n = c("_control", "_spatial", "_visual"), base = 3, ...)
```

## Arguments

- df:

  A data frame containing a `category` column.

- n:

  A vector of levels for the factor. Default is
  `c("_control", "_spatial", "_visual")`.

- base:

  The base level for the contrasts. Default is 3 for the visual
  category, as our hypothesis called for planned comparisons of the
  visual category with the other two. This argument can be used to set a
  different base level if desired.

- ...:

  Additional arguments passed to
  [`add_factor_contrasts()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_factor_contrasts.md).

## Value

A data frame with the `category` column converted to a factor with
contrasts added.

## Examples

``` r
df <- factor_categories(experiment_data)
contrasts(df$category)
#>         _control _spatial
#> Control        1        0
#> Spatial        0        1
#> Visual         0        0
```
