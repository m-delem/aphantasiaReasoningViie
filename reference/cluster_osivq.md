# Cluster the OSIVQ data using consensus between various algorithms

Cluster the OSIVQ data using consensus between various algorithms

## Usage

``` r
cluster_osivq(
  df,
  algorithms = c("gmm", "pam", "cmeans"),
  cons.funs = c("kmodes", "majority", "CSPA"),
  seed = 667,
  progress = FALSE,
  verbose = FALSE
)
```

## Arguments

- df:

  A dataframe with the OSIVQ scores, typically obtained from
  [`get_clean_data()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_clean_data.md).

- algorithms:

  A character vector of clustering algorithms to use. Default is
  `c("gmm", "pam", "cmeans", "diana")`. See `?diceR::dice()` for more
  details.

- cons.funs:

  A character vector of consensus functions to use. Default is
  `c("CSPA")`. See `?diceR::dice()` for more details.

- seed:

  An integer seed for reproducibility. Default is 667.

- progress:

  Logical value indicating whether to show a progress bar.

- verbose:

  Logical value indicating whether to print detailed messages during the
  clustering process.

## Value

A list with the clustering results from
[`diceR::dice()`](https://alinetalhouk.github.io/diceR/reference/dice.html).

## Examples

``` r
clustering <-
 get_clean_data()$df_survey |>
 cluster_osivq()

clustering$clusters
#>     kmodes majority CSPA
#> 1        2        2    2
#> 2        3        3    3
#> 3        2        2    2
#> 4        2        2    2
#> 5        2        2    2
#> 6        3        3    3
#> 7        2        2    2
#> 8        2        2    2
#> 9        2        2    2
#> 10       2        2    2
#> 11       2        2    2
#> 12       2        2    2
#> 13       3        3    3
#> 14       3        3    3
#> 15       2        2    2
#> 16       2        2    2
#> 17       3        3    3
#> 18       1        1    1
#> 19       3        3    1
#> 20       3        3    3
#> 21       3        3    3
#> 22       1        1    1
#> 23       2        2    2
#> 24       3        3    3
#> 25       3        3    3
#> 26       3        3    3
#> 27       2        2    2
#> 28       3        3    3
#> 29       3        3    3
#> 30       2        2    2
#> 31       2        2    2
#> 32       3        3    3
#> 33       3        3    3
#> 34       2        2    2
#> 35       2        2    2
#> 36       3        3    3
#> 37       3        3    3
#> 38       3        3    3
#> 39       3        3    3
#> 40       1        1    1
#> 41       1        1    1
#> 42       1        1    1
#> 43       3        3    3
#> 44       3        3    3
#> 45       2        2    2
#> 46       3        3    3
#> 47       2        2    2
#> 48       3        3    3
#> 49       3        3    3
#> 50       2        2    2
#> 51       3        3    3
#> 52       3        3    3
#> 53       2        2    2
#> 54       1        1    1
#> 55       2        2    2
#> 56       2        2    2
#> 57       2        2    2
#> 58       2        2    2
#> 59       2        2    2
#> 60       3        3    3
#> 61       1        1    1
#> 62       2        2    2
#> 63       3        3    3
#> 64       2        2    1
#> 65       1        1    1
#> 66       1        1    1
#> 67       3        3    3
#> 68       2        2    2
#> 69       1        1    1
#> 70       1        1    1
#> 71       3        3    3
#> 72       3        3    3
#> 73       2        2    2
#> 74       2        2    2
#> 75       1        1    1
#> 76       2        2    2
#> 77       2        2    2
#> 78       3        3    3
#> 79       1        1    1
#> 80       3        3    3
#> 81       3        3    3
#> 82       3        3    3
#> 83       2        2    2
#> 84       3        3    3
#> 85       3        3    3
#> 86       2        2    2
#> 87       2        2    2
#> 88       2        2    2
#> 89       3        3    3
#> 90       2        2    2
#> 91       2        2    1
#> 92       2        2    2
#> 93       3        3    3
#> 94       1        1    1
#> 95       2        2    2
#> 96       3        3    3
#> 97       3        3    3
#> 98       2        2    2
#> 99       1        1    1
#> 100      2        2    2
#> 101      2        2    2
#> 102      3        3    3
#> 103      1        1    1
#> 104      3        3    3
```
