# Get data with the Visual Imagery Impedance Effect manually calculated

Another approach to our main problem (evaluating the individual
differences in contrasts between categories for, e.g., response times)
could be to compute the category contrasts in the outcome variable
manually and then compare individuals or groups on these difference
scores. This is less powerful than using mixed-effects models with
interaction terms, but some might wonder what comes out of such an
approach. This function computes these difference scores between
response times in the visual category minus the other categories
(spatial, control, and non-visual, an average of control and spatial) to
create three Visual Imagery Impedance Effect (VIIE) scores per
participant. It returns a data frame with these VIIE scores along with
relevant individual difference (questionnaire) variables.

## Usage

``` r
get_viie_data(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [`get_clustered_data()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_clustered_data.md).

## Value

A data frame with one row per participant containing their questionnaire
scores and groups along with three VIIE scores: `viie_total` (mean
visual RT minus mean non-visual RT), `viie_spatial` (visual RT minus
spatial RT), and `viie_control` (visual RT minus control RT).

## Examples

``` r
colnames(get_viie_data())
#>  [1] "id"               "vviq_total_score" "osivq_object"     "osivq_spatial"   
#>  [5] "osivq_verbal"     "group_4"          "cluster"          "viie_total"      
#>  [9] "viie_spatial"     "viie_control"     "group_2"          "group_3"         
#> [13] "strategy_group"   "control_rt"       "spatial_rt"       "visual_rt"       
#> [17] "non_vis_rt"      
```
