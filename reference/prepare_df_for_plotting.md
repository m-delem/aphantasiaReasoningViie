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
#>    id                    category group_4 group_2 group_3 strategy_group mean_dv
#>    <fct>                 <fct>    <fct>   <fct>   <fct>   <fct>            <dbl>
#>  1 acdn247721443631359l… Control  Typical Typical Typical No_visual_str…    21.6
#>  2 acdn247721443631359l… Spatial  Typical Typical Typical No_visual_str…    17.9
#>  3 acdn247721443631359l… Visual   Typical Typical Typical No_visual_str…    20.8
#>  4 ahos206230340719944k… Control  Aphant… Aphant… Aphant… No_visual_str…    26.3
#>  5 ahos206230340719944k… Spatial  Aphant… Aphant… Aphant… No_visual_str…    27.2
#>  6 ahos206230340719944k… Visual   Aphant… Aphant… Aphant… No_visual_str…    28.5
#>  7 anoo201523848333505m… Control  Typical Typical Typical Visual_strate…    17.5
#>  8 anoo201523848333505m… Spatial  Typical Typical Typical Visual_strate…    15.2
#>  9 anoo201523848333505m… Visual   Typical Typical Typical Visual_strate…    26.2
#> 10 arje91119258110263fk… Control  Typical Typical Typical Visual_strate…    27.8
#> 11 arje91119258110263fk… Spatial  Typical Typical Typical Visual_strate…    20.9
#> 12 arje91119258110263fk… Visual   Typical Typical Typical Visual_strate…    23.2
```
