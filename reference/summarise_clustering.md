# Get the size and questionnaire means of clusters

Get the size and questionnaire means of clusters

## Usage

``` r
summarise_clustering(df)
```

## Arguments

- df:

  A data frame with columns `group`, `cluster`, `vviq_total_score`,
  `osivq_object`, `osivq_spatial`, `osivq_verbal`, and `raven_score`.

## Value

A data frame summarizing the clusters, including the number of
participants in each cluster and the mean scores for VVIQ, OSIVQ object,
OSIVQ spatial, OSIVQ verbal, and Raven scores.

## Examples

``` r
df <- get_clean_data()$df_survey
clustering <- cluster_osivq(df)

df |>
 add_named_clusters(clustering) |>
 summarise_clustering()
#> # A tibble: 9 × 8
#>   group          cluster       n  vviq object spatial verbal raven
#>   <fct>          <fct>     <int> <dbl>  <dbl>   <dbl>  <dbl> <dbl>
#> 1 Hypophantasia  cluster_1     3  24     1.67    3.67   2.55  15.7
#> 2 Typical        cluster_1    12  50.9   2.39    2.84   2.46  15.3
#> 3 Aphantasia     cluster_1     4  16     1.41    3.32   1.97  17  
#> 4 Typical        cluster_2    38  57.3   3.71    2.87   3.09  15.8
#> 5 Hyperphantasia cluster_2     4  77.5   4.41    3.93   3.56  15  
#> 6 Aphantasia     cluster_2     1  16     4.93    3.27   4.44  16  
#> 7 Aphantasia     cluster_3    25  16.0   1.25    2.26   3.38  16.0
#> 8 Hypophantasia  cluster_3    14  23.7   1.36    2.52   3.5   15.3
#> 9 Typical        cluster_3     3  50.3   1.76    2.67   4.11  17.3
```
