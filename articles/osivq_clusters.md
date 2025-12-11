# Creating OSIVQ cognitive style clusters

This short vignette details how the OSIVQ clusters were created and
named, and how they relate to the VVIQ-based groups.

``` r
library(aphantasiaReasoningViie)
#> Welcome to aphantasiaReasoningViie.
#> See https://osf.io/hfbcp/ for the associated study.
```

First, let’s get the cleaned, analysis-ready data (see
[`vignette("preparing_data")`](https://m-delem.github.io/aphantasiaReasoningViie/articles/preparing_data.md)
for details).

``` r
df_survey  <- get_clean_data("survey")
```

The OSIVQ clusters were created using a consensus of three clustering
algorithms (GMM, PAM and C-Means) applied to the three OSIVQ sub-scales
(Object, Spatial and Verbal). A function was created for this specific
task,
[`cluster_osivq()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/cluster_osivq.md),
which uses the
[`diceR::dice()`](https://alinetalhouk.github.io/diceR/reference/dice.html)
function from the *diceR* package under the hood. Following the
methodology proposed by Delem et al. (2025), we searched for three
“visualiser”, “spatialiser” and “verbaliser” clusters based on their
most “dominant” OSIVQ sub-scale score.

However, the clustering algorithm does not name the clusters, so we
needed to check their properties to assign meaningful names. The
function
[`add_named_clusters()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_named_clusters.md)
adds the cluster assignments to the data frame, by default with generic
names.
[`summarise_clustering()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/summarise_clustering.md)
then provides a summary of the clusters’ properties, which allowed us to
choose meaningful names for the three clusters produced.

``` r
# Clustering OSIVQ data
clustering <- cluster_osivq(df_survey)

# Checking cluster properties to define names for each cluster
df_survey |>
  add_named_clusters(clustering) |>
  summarise_clustering()
#> # A tibble: 9 × 8
#>   group_4        cluster       n  vviq object spatial verbal raven
#>   <fct>          <fct>     <int> <dbl>  <dbl>   <dbl>  <dbl> <dbl>
#> 1 Hypophantasia  cluster_1     3  24     1.67    3.67   2.55  15.7
#> 2 Typical        cluster_1    12  50.9   2.39    2.84   2.46  15.3
#> 3 Aphantasia     cluster_1     4  16     1.41    3.32   1.97  17  
#> 4 Typical        cluster_2    38  57.3   3.71    2.87   3.09  15.8
#> 5 Hyperphantasia cluster_2     4  77.5   4.41    3.93   3.56  15  
#> 6 Aphantasia     cluster_2     1  16     4.93    3.27   4.44  16  
#> 7 Aphantasia     cluster_3    25  16     1.25    2.26   3.38  16.0
#> 8 Hypophantasia  cluster_3    14  23.7   1.36    2.52   3.5   15.3
#> 9 Typical        cluster_3     3  50.3   1.76    2.67   4.11  17.3
```

We saw here that cluster 1 was the spatialiser one, cluster 2 was the
visualiser and cluster 3 the verbaliser. We can now use
[`add_named_clusters()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_named_clusters.md)
again with its optional arguments to add these labels, reorder them,
choose factor levels and add contrasts for planned comparisons between
the visualiser cluster and the other two.

``` r
# Adding named clusters to the survey data
df_survey <- add_named_clusters(
  df_survey, clustering,
  names  = c("Spatialiser", "Visualiser", "Verbaliser"),
  levels = c("Visualiser", "Spatialiser", "Verbaliser"),
  contrasts = c("_visualiser", "_spatialiser", "_verbaliser"),
  base = 1
)

# Let's check the cluster properties again
contrasts(df_survey$cluster)
#>             _spatialiser _verbaliser
#> Visualiser             0           0
#> Spatialiser            1           0
#> Verbaliser             0           1
summarise_clustering(df_survey)
#> # A tibble: 9 × 8
#>   group_4        cluster         n  vviq object spatial verbal raven
#>   <fct>          <fct>       <int> <dbl>  <dbl>   <dbl>  <dbl> <dbl>
#> 1 Typical        Visualiser     38  57.3   3.71    2.87   3.09  15.8
#> 2 Hyperphantasia Visualiser      4  77.5   4.41    3.93   3.56  15  
#> 3 Aphantasia     Visualiser      1  16     4.93    3.27   4.44  16  
#> 4 Hypophantasia  Spatialiser     3  24     1.67    3.67   2.55  15.7
#> 5 Typical        Spatialiser    12  50.9   2.39    2.84   2.46  15.3
#> 6 Aphantasia     Spatialiser     4  16     1.41    3.32   1.97  17  
#> 7 Aphantasia     Verbaliser     25  16     1.25    2.26   3.38  16.0
#> 8 Hypophantasia  Verbaliser     14  23.7   1.36    2.52   3.5   15.3
#> 9 Typical        Verbaliser      3  50.3   1.76    2.67   4.11  17.3
```

[`cluster_osivq()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/cluster_osivq.md)
uses a seed, so the resulting clusters will always be the same for a
given seed and data set. Thus, we were able to use this short clustering
pipeline in all subsequent analyses to create our “cluster” variable and
add it to the experimental data set for modelling. To avoid copy-pasting
this code pipeline in every analysis script, we created a wrapper,
[`get_clustered_data()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_clustered_data.md)
with default arguments based on the analysis above:

``` r
colnames(get_clustered_data("experiment"))
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

Clusters are there, ready for analysis!

------------------------------------------------------------------------

    #> ─ Session info ───────────────────────────────────────────────────────────────
    #>  setting  value
    #>  version  R version 4.5.2 (2025-10-31)
    #>  os       Ubuntu 24.04.3 LTS
    #>  system   x86_64, linux-gnu
    #>  ui       X11
    #>  language en
    #>  collate  C.UTF-8
    #>  ctype    C.UTF-8
    #>  tz       UTC
    #>  date     2025-12-11
    #>  pandoc   3.1.11 @ /opt/hostedtoolcache/pandoc/3.1.11/x64/ (via rmarkdown)
    #>  quarto   1.8.26 @ /usr/local/bin/quarto
    #> 
    #> ─ Packages ───────────────────────────────────────────────────────────────────
    #>  ! package                 * version date (UTC) lib source
    #>    abind                     1.4-8   2024-09-12 [1] RSPM
    #>    aphantasiaReasoningViie * 1.0     2025-12-11 [1] local
    #>    assertthat                0.2.1   2019-03-21 [1] RSPM
    #>    bslib                     0.9.0   2025-01-30 [1] RSPM
    #>    cachem                    1.1.0   2024-05-16 [1] RSPM
    #>  P class                     7.3-23  2025-01-01 [?] CRAN (R 4.5.2)
    #>    cli                       3.6.5   2025-04-23 [1] RSPM
    #>    clue                      0.3-66  2024-11-13 [1] RSPM
    #>  P cluster                   2.1.8.1 2025-03-12 [?] CRAN (R 4.5.2)
    #>    clusterCrit               1.3.0   2023-11-23 [1] RSPM
    #>    clValid                   0.7     2021-02-14 [1] RSPM
    #>    combinat                  0.0-8   2012-10-29 [1] RSPM
    #>    crayon                    1.5.3   2024-06-20 [1] RSPM
    #>    desc                      1.4.3   2023-12-10 [1] RSPM
    #>  P devtools                * 2.4.6   2025-10-03 [?] RSPM
    #>    diceR                     3.1.0   2025-06-19 [1] RSPM
    #>    digest                    0.6.39  2025-11-19 [1] RSPM
    #>    dplyr                     1.1.4   2023-11-17 [1] RSPM
    #>    e1071                     1.7-16  2024-09-16 [1] RSPM
    #>  P ellipsis                  0.3.2   2021-04-29 [?] RSPM
    #>    evaluate                  1.0.5   2025-08-27 [1] RSPM
    #>    farver                    2.1.2   2024-05-13 [1] RSPM
    #>    fastmap                   1.2.0   2024-05-15 [1] RSPM
    #>    forcats                   1.0.1   2025-09-25 [1] RSPM
    #>    fs                        1.6.6   2025-04-12 [1] RSPM
    #>    generics                  0.1.4   2025-05-09 [1] RSPM
    #>    ggplot2                   4.0.1   2025-11-14 [1] RSPM
    #>    glue                      1.8.0   2024-09-30 [1] RSPM
    #>    gtable                    0.3.6   2024-10-25 [1] RSPM
    #>    haven                     2.5.5   2025-05-30 [1] RSPM
    #>    highr                     0.11    2024-05-26 [1] RSPM
    #>    hms                       1.1.4   2025-10-17 [1] RSPM
    #>    htmltools                 0.5.9   2025-12-04 [1] RSPM
    #>    htmlwidgets               1.6.4   2023-12-06 [1] RSPM
    #>    httpuv                    1.6.16  2025-04-16 [1] RSPM
    #>    jquerylib                 0.1.4   2021-04-26 [1] RSPM
    #>    jsonlite                  2.0.0   2025-03-27 [1] RSPM
    #>    klaR                      1.7-3   2023-12-13 [1] RSPM
    #>    knitr                     1.50    2025-03-16 [1] RSPM
    #>    labelled                  2.16.0  2025-10-22 [1] RSPM
    #>    later                     1.4.4   2025-08-27 [1] RSPM
    #>    lifecycle                 1.0.4   2023-11-07 [1] RSPM
    #>    magrittr                  2.0.4   2025-09-12 [1] RSPM
    #>  P MASS                      7.3-65  2025-02-28 [?] CRAN (R 4.5.2)
    #>    mclust                    6.1.2   2025-10-31 [1] RSPM
    #>    memoise                   2.0.1   2021-11-26 [1] RSPM
    #>    mime                      0.13    2025-03-17 [1] RSPM
    #>    miniUI                    0.1.2   2025-04-17 [1] RSPM
    #>    otel                      0.2.0   2025-08-29 [1] RSPM
    #>    pillar                    1.11.1  2025-09-17 [1] RSPM
    #>    pkgbuild                  1.4.8   2025-05-26 [1] RSPM
    #>    pkgconfig                 2.0.3   2019-09-22 [1] RSPM
    #>    pkgdown                   2.2.0   2025-11-06 [1] any (@2.2.0)
    #>    pkgload                   1.4.1   2025-09-23 [1] RSPM
    #>    promises                  1.5.0   2025-11-01 [1] RSPM
    #>    proxy                     0.4-27  2022-06-09 [1] RSPM
    #>    purrr                     1.2.0   2025-11-04 [1] RSPM
    #>    questionr                 0.8.1   2025-06-10 [1] RSPM
    #>    R6                        2.6.1   2025-02-15 [1] RSPM
    #>    ragg                      1.5.0   2025-09-02 [1] RSPM
    #>    RColorBrewer              1.1-3   2022-04-03 [1] RSPM
    #>    Rcpp                      1.1.0   2025-07-02 [1] RSPM
    #>  P remotes                   2.5.0   2024-03-17 [?] RSPM
    #>    renv                      1.1.4   2025-03-20 [1] RSPM (R 4.5.0)
    #>    rlang                     1.1.6   2025-04-11 [1] RSPM
    #>    rmarkdown                 2.30    2025-09-28 [1] RSPM
    #>    rstudioapi                0.17.1  2024-10-22 [1] RSPM
    #>    S7                        0.2.1   2025-11-14 [1] RSPM
    #>    sass                      0.4.10  2025-04-11 [1] RSPM
    #>    scales                    1.4.0   2025-04-24 [1] RSPM
    #>    sessioninfo               1.2.3   2025-02-05 [1] RSPM
    #>    shiny                     1.12.1  2025-12-09 [1] RSPM
    #>    stringi                   1.8.7   2025-03-27 [1] RSPM
    #>    stringr                   1.6.0   2025-11-04 [1] RSPM
    #>    systemfonts               1.3.1   2025-10-01 [1] RSPM
    #>    textshaping               1.0.4   2025-10-10 [1] RSPM
    #>    tibble                    3.3.0   2025-06-08 [1] RSPM
    #>    tidyselect                1.2.1   2024-03-11 [1] RSPM
    #>  P usethis                 * 3.2.1   2025-09-06 [?] RSPM
    #>    utf8                      1.2.6   2025-06-08 [1] RSPM
    #>    vctrs                     0.6.5   2023-12-01 [1] RSPM
    #>    withr                     3.0.2   2024-10-28 [1] RSPM
    #>    xfun                      0.54    2025-10-30 [1] RSPM
    #>    xtable                    1.8-4   2019-04-21 [1] RSPM
    #>    yaml                      2.3.12  2025-12-10 [1] RSPM
    #> 
    #>  [1] /home/runner/.cache/R/renv/library/aphantasiaReasoningViie-b75da44b/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu
    #>  [2] /home/runner/.cache/R/renv/sandbox/linux-ubuntu-noble/R-4.5/x86_64-pc-linux-gnu/8f3cef43
    #> 
    #>  * ── Packages attached to the search path.
    #>  P ── Loaded and on-disk path mismatch.
    #> 
    #> ──────────────────────────────────────────────────────────────────────────────

## References

Delem, M., Turkben, S., Cavalli, E., Cousineau, D., & Plancher, G.
(2025). Unsupervised clustering reveals spatial and verbal cognitive
profiles in aphantasia and typical imagery. *Neuropsychologia*, *219*,
109279. <https://doi.org/10.1016/j.neuropsychologia.2025.109279>
