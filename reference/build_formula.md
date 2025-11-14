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

  Grouping variable, typically "Group" or "cluster".

## Value

A formula object for the model.

## Examples

``` r
build_formula("accuracy", "group")
#> accuracy ~ group * category + (category | id) + (group | problem)
#> <environment: 0x55cd5cb7a0e8>
```
