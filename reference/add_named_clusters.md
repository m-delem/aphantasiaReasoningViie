# Add a column with named cluster assignments to a data frame

This function is tailored for three clusters. After checking the results
of the clustering procedure with
[`summarise_clustering()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/summarise_clustering.md)
and a graphical examination, we can choose names for the three clusters
manually and input them into this function to get a meaningful variable
to analyse.

## Usage

``` r
add_named_clusters(
  df,
  clustering,
  method = "CSPA",
  names = c("cluster_1", "cluster_2", "cluster_3"),
  levels = names,
  contrasts = paste0("_", levels),
  base = 1,
  ...
)
```

## Arguments

- df:

  A data frame with the OSIVQ scores, typically obtained from
  [`get_clean_data()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_clean_data.md).

- clustering:

  A clustering object obtained from
  [`cluster_osivq()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/cluster_osivq.md).

- method:

  A character string specifying the consensus clustering method to use.
  Must be "kmodes", "majority", or "CSPA". Default is "CSPA".

- names:

  A character vector of names for the clusters. Default is
  `c("cluster_1", "cluster_2", "cluster_3")`.

- levels:

  A character vector of levels for the factor. Default is the same as
  `names`.

- contrasts:

  A character vector of contrasts for the factor levels.

- base:

  An integer indicating the base level for the contrasts. Default is 1,
  which corresponds to the first cluster in `names`.

- ...:

  Additional arguments passed to
  [`add_factor_contrasts()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_factor_contrasts.md).

## Value

A data frame with an additional column `cluster` that contains the named
cluster assignments.

## Examples

``` r
df <- get_clean_data()$df_survey
clustering <- cluster_osivq(df)

df |> add_named_clusters(clustering) |> dplyr::select(id, group, cluster)
#> # A tibble: 104 × 3
#>    id                      group          cluster  
#>    <fct>                   <fct>          <fct>    
#>  1 acdn247721443631359lzxb Typical        cluster_2
#>  2 ahos206230340719944kiwl Aphantasia     cluster_3
#>  3 anoo201523848333505mpxv Typical        cluster_2
#>  4 arje91119258110263fkhy  Typical        cluster_2
#>  5 auzb748856118756901vqiz Typical        cluster_2
#>  6 azcj317771758245897ccrd Hypophantasia  cluster_3
#>  7 bbed48536882735234umna  Typical        cluster_2
#>  8 bbwg95779316229529nvnw  Typical        cluster_2
#>  9 bwba621197531274841rnng Hyperphantasia cluster_2
#> 10 cbkt251247663521563wvjf Typical        cluster_2
#> # ℹ 94 more rows
```
