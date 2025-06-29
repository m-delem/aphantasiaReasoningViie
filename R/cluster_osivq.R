#' Cluster the OSIVQ data using consensus between various algorithms
#'
#' @param df A dataframe with the OSIVQ scores, typically obtained from
#' `get_osivq_data()`.
#' @param algorithms A character vector of clustering algorithms to use. Default
#' is `c("gmm", "pam", "cmeans", "diana")`. See `?diceR::dice()` for more
#' details.
#' @param cons.funs A character vector of consensus functions to use. Default is
#' `c("CSPA")`. See `?diceR::dice()` for more details.
#' @param seed An integer seed for reproducibility. Default is 667.
#'
#' @returns A list with the clustering results from `diceR::dice()`.
#' @export
cluster_osivq <- function(
    df,
    algorithms = c("gmm", "pam", "cmeans", "hc", "diana"),
    cons.funs  = c("kmodes", "majority", "CSPA"),
    seed = 667,
    progress = FALSE,
    verbose = FALSE
) {
  rlang::check_installed("diceR")

  if (is.numeric(seed)) set.seed(seed)

  clustering <-
    diceR::dice(
      dplyr::select(df, osivq_object, osivq_spatial, osivq_verbal),
      nk     = 3,
      p.item = 1,
      algorithms = algorithms,
      cons.funs  = cons.funs,
      seed     = seed,
      progress = progress,
      verbose  = verbose
    )
  return(clustering)
}
