# Helper function to prepare the data frame for plotting

This function groups the data by participant ID, category, and any
grouping or clustering variables, then calculates the mean of the
dependent variable for each group. It also reverses the factor levels
for any clustering variables to ensure correct ordering in plots.

## Usage

``` r
prepare_df_for_plotting(df, dvar)
```

## Arguments

- df:

  A data frame containing the data to be plotted.

- dvar:

  The dependent variable to be averaged and plotted.

## Value

A data frame ready for plotting with the mean dependent variable for
each participant, category, and grouping/cluster combination.

## Examples

``` r
df <- get_clean_data()$df_expe
df_rt <- filter_trials_on_rt(df)

df_rt |> prepare_df_for_plotting(dvar = rt_total) |> head(12)
#> # A tibble: 12 × 7
#>    id                      category group group_2 group_3 strategy_group mean_dv
#>    <fct>                   <fct>    <fct> <fct>   <fct>   <fct>            <dbl>
#>  1 acdn247721443631359lzxb Control  Typi… Typical Typical No visual str…    21.6
#>  2 acdn247721443631359lzxb Spatial  Typi… Typical Typical No visual str…    17.9
#>  3 acdn247721443631359lzxb Visual   Typi… Typical Typical No visual str…    20.8
#>  4 ahos206230340719944kiwl Control  Apha… Aphant… Aphant… No visual str…    26.3
#>  5 ahos206230340719944kiwl Spatial  Apha… Aphant… Aphant… No visual str…    27.2
#>  6 ahos206230340719944kiwl Visual   Apha… Aphant… Aphant… No visual str…    28.5
#>  7 anoo201523848333505mpxv Control  Typi… Typical Typical Visual strate…    17.5
#>  8 anoo201523848333505mpxv Spatial  Typi… Typical Typical Visual strate…    15.2
#>  9 anoo201523848333505mpxv Visual   Typi… Typical Typical Visual strate…    26.2
#> 10 arje91119258110263fkhy  Control  Typi… Typical Typical Visual strate…    27.8
#> 11 arje91119258110263fkhy  Spatial  Typi… Typical Typical Visual strate…    20.9
#> 12 arje91119258110263fkhy  Visual   Typi… Typical Typical Visual strate…    23.2
```
