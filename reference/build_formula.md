# Build a formula based on the common model for accuracy and RT modelling

Build a formula based on the common model for accuracy and RT modelling

## Usage

``` r
build_formula(vd, grouping)
```

## Arguments

- vd:

  Variable of interest, either "accuracy" or "rt_total".

- grouping:

  Grouping variable, e.g., "group_3", "cluster", etc.

## Value

A formula object for the model.

## Examples

``` r
build_formula("accuracy", "group")
#> accuracy ~ group * category + (category | id) + (1 | problem)
#> <environment: 0x562c734cb490>
```
