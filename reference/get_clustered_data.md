# Wrapper function to get clean and clustered data immediately

This function retrieves clean data using
[`get_clean_data()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_clean_data.md)
and then performs clustering on the OSIVQ data using
[`cluster_osivq()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/cluster_osivq.md).
It then adds named clusters to the survey data using
[`add_named_clusters()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_named_clusters.md)
and merges the cluster information with the experiment data. Depending
on the `type` parameter, it returns either the cleaned experiment data
with cluster information, the cleaned survey data with cluster
information, the clustering results, or all of these as a list. This is
a convenience function to streamline the process of obtaining clustered
data for analysis, but its defaults are based on a first iteration of
this clustering procedure, which is described in full in the vignette
[`vignette("osivq_clusters")`](https://m-delem.github.io/aphantasiaReasoningViie/articles/osivq_clusters.md).

## Usage

``` r
get_clustered_data(
  type = "all",
  names = c("Spatialiser", "Visualiser", "Verbaliser"),
  levels = c("Visualiser", "Spatialiser", "Verbaliser"),
  contrasts = c("_visualiser", "_spatialiser", "_verbaliser"),
  base = 1
)
```

## Arguments

- type:

  The type of data to return: "experiment", "survey", "clustering", or
  "all".

- names:

  A character vector of names for the clusters passed to
  [`add_named_clusters()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_named_clusters.md).
  Default is `c("Spatialiser", "Visualiser", "Verbaliser")`.

- levels:

  A character vector of levels for the factor passed to
  [`add_named_clusters()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_named_clusters.md).
  Default is `c("Visualiser", "Spatialiser", "Verbaliser")`.

- contrasts:

  A character vector of contrasts for the factor levels passed to
  [`add_named_clusters()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_named_clusters.md).

- base:

  An integer indicating the base level for the contrasts. Default is 1,
  which corresponds to the first cluster in `names`.

## Value

A cleaned and clustered data frame or a list of cleaned and clustered
data frames, depending on the `type` parameter:

- If `type` is "experiment", returns the cleaned experiment data frame
  with cluster information.

- If `type` is "survey", returns the cleaned survey data frame with
  cluster information.

- If `type` is "clustering", returns the clustering results.

- If `type` is "all", returns a list containing the cleaned experiment
  data frame with cluster information, the cleaned survey data frame
  with cluster information, and the clustering results.

## Examples

``` r
colnames(get_clustered_data(type = "experiment"))
#>  [1] "id"                 "language"           "group_4"           
#>  [4] "cluster"            "group_2"            "group_3"           
#>  [7] "strategy_group"     "expe_phase"         "trial_number"      
#> [10] "problem"            "category"           "premise_1_rt"      
#> [13] "premise_2_rt"       "premise_3_rt"       "conclusion_rt"     
#> [16] "rt_total"           "response"           "correct_response"  
#> [19] "accuracy"           "acc_perc"           "visual_strat"      
#> [22] "verbal_strat"       "spatial_strat"      "semantic_strat"    
#> [25] "sensorimotor_strat" "asso_strat_1"       "other_strat"       
#> [28] "asso_strat_2"       "asso_strat_3"       "median_rt"         
```
