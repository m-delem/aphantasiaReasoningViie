#' Cluster the OSIVQ data using consensus between various algorithms
#'
#' @param df A dataframe with the OSIVQ scores, typically obtained from
#' [get_clean_data()].
#' @param algorithms A character vector of clustering algorithms to use. Default
#' is `c("gmm", "pam", "cmeans", "diana")`. See `?diceR::dice()` for more
#' details.
#' @param cons.funs A character vector of consensus functions to use. Default is
#' `c("CSPA")`. See `?diceR::dice()` for more details.
#' @param seed An integer seed for reproducibility. Default is 667.
#'
#' @returns A list with the clustering results from [diceR::dice()].
#' @export
cluster_osivq <- function(
    df,
    algorithms = c("gmm", "pam", "cmeans"),
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

#' Add a column with named cluster assignments to a data frame
#'
#' This function is tailored for three clusters. After checking the results of
#' the clustering procedure with [summarise_clustering()] and a graphical
#' examination, we can choose names for the three clusters manually and input
#' them into this function to get a meaningful variable to analyse.
#'
#' @param df A data frame with the OSIVQ scores, typically obtained from
#' [get_clean_data()].
#' @param clustering A clustering object obtained from [cluster_osivq()].
#' @param method A character string specifying the consensus clustering method
#' to use. Must be "kmodes", "majority", or "CSPA". Default is "CSPA".
#'
#' @returns
#' @export
add_named_clusters <- function(
    df,
    clustering,
    method = "CSPA",
    names  = c("cluster_1", "cluster_2", "cluster_3"),
    levels = names,
    contrasts = paste0("_", levels),
    base = 1,
    ...
) {
  df_with_cluster <-
    df |>
    dplyr::mutate(
      cluster = clustering$clusters[, method],
      cluster = dplyr::case_when(
        cluster == 1 ~ names[1],
        cluster == 2 ~ names[2],
        cluster == 3 ~ names[3],
        TRUE ~ NA
        ) |>
        factor(levels = levels) |>
        add_factor_contrasts(n = contrasts, base = base, ...)
    ) |>
    dplyr::relocate(cluster, .after = "group")
  return(df_with_cluster)
}

#' Get the size and questionnaire means of clusters
#'
#' @param df A data frame with columns `group`, `cluster`, `vviq_total_score`,
#' `osivq_object`, `osivq_spatial`, `osivq_verbal`, and `raven_score`.
#'
#' @returns A data frame summarizing the clusters, including the number of
#' participants in each cluster and the mean scores for VVIQ, OSIVQ object,
#' OSIVQ spatial, OSIVQ verbal, and Raven scores.
#' @export
summarise_clustering <- function(df) {
  df_summary <-
    df |>
    dplyr::reframe(
      .by = c("group", "cluster"),
      n = dplyr::n(),
      vviq    = mean(vviq_total_score, na.rm = TRUE) |> round(2),
      object  = mean(osivq_object,  na.rm = TRUE) |> round(2),
      spatial = mean(osivq_spatial, na.rm = TRUE) |> round(2),
      verbal  = mean(osivq_verbal,  na.rm = TRUE) |> round(2),
      raven   = mean(raven_score,   na.rm = TRUE) |> round(2),
    ) |>
    dplyr::arrange(cluster)

  return(df_summary)
}
