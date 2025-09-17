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
#' @param progress Logical value indicating whether to show a progress bar.
#' @param verbose Logical value indicating whether to print detailed messages
#' during the clustering process.
#'
#' @returns A list with the clustering results from [diceR::dice()].
#' @export
#'
#' @examples
#' clustering <-
#'  get_clean_data()$df_survey |>
#'  cluster_osivq()
#'
#' clustering$clusters
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
      dplyr::select(
        df,
        "osivq_object",
        "osivq_spatial",
        "osivq_verbal"
      ),
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
#' @param names A character vector of names for the clusters. Default is
#' `c("cluster_1", "cluster_2", "cluster_3")`.
#' @param levels A character vector of levels for the factor. Default is the
#' same as `names`.
#' @param contrasts A character vector of contrasts for the factor levels.
#' @param base An integer indicating the base level for the contrasts. Default
#' is 1, which corresponds to the first cluster in `names`.
#' @param ... Additional arguments passed to [add_factor_contrasts()].
#'
#' @returns A data frame with an additional column `cluster` that contains the
#' named cluster assignments.
#' @export
#'
#' @examples
#' df <- get_clean_data()$df_survey
#' clustering <- cluster_osivq(df)
#'
#' df |> add_named_clusters(clustering) |> dplyr::select(id, group, cluster)
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
        .data$cluster == 1 ~ names[1],
        .data$cluster == 2 ~ names[2],
        .data$cluster == 3 ~ names[3],
        TRUE ~ NA
        ) |>
        factor(levels = levels) |>
        add_factor_contrasts(n = contrasts, base = base, ...)
    ) |>
    dplyr::relocate("cluster", .after = "group")
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
#'
#' @examples
#' df <- get_clean_data()$df_survey
#' clustering <- cluster_osivq(df)
#'
#' df |>
#'  add_named_clusters(clustering) |>
#'  summarise_clustering()
summarise_clustering <- function(df) {
  df_summary <-
    df |>
    dplyr::reframe(
      .by = c("group", "cluster"),
      n = dplyr::n(),
      vviq    = mean(.data$vviq_total_score, na.rm = TRUE) |> round(2),
      object  = mean(.data$osivq_object,  na.rm = TRUE) |> round(2),
      spatial = mean(.data$osivq_spatial, na.rm = TRUE) |> round(2),
      verbal  = mean(.data$osivq_verbal,  na.rm = TRUE) |> round(2),
      raven   = mean(.data$raven_score,   na.rm = TRUE) |> round(2),
    ) |>
    dplyr::arrange(.data$cluster)

  return(df_summary)
}

#' Plot the OSIVQ scores of clusters in a ternary diagram
#'
#' @param df A dataframe containing `osivq_object`, `osivq_spatial`,
#'`osivq_verbal` and `cluster` columns.
#' @param dot_size Size of the dots in the plot.
#' @param plot_it Logical. If TRUE, the plot will be printed immediately.
#' @param colours A vector of colours to use for the clusters in the plot passed
#' to the [ggplot2::scale_discrete_manual()] function. Default is
#' [palette.colors()].
#' @param ... Additional arguments passed to the [theme_pdf()] function.
#'
#' @returns A ggplot object representing the OSIVQ scores in a ternary diagram.
#' @export
#'
#' @examples
#' df <- get_clean_data()$df_survey
#'
#' # Clustering the sample based on OSIVQ scores
#' clustering <- cluster_osivq(df)
#' df <- add_named_clusters(df, clustering)
#'
#' if (require("coda.plot", quietly = TRUE)) {
#'  plot_osivq_ternary(df, base_theme = ggplot2::theme_grey, base_size = 12)
#' }
#'
#' @keywords internal
plot_osivq_ternary <- function(
    df,
    dot_size = 1.5,
    plot_it  = FALSE,
    colours = palette.colors(),
    ...
) {
  rlang::check_installed("coda.plot")

  p <-
    df |>
    dplyr::select(
      Object  = "osivq_object",
      Spatial = "osivq_spatial",
      Verbal  = "osivq_verbal"
    ) |>
    coda.plot::ternary_diagram(
      group  = df$cluster,
      center = TRUE,
      scale  = TRUE
    ) +
    ggplot2::scale_discrete_manual(
      name = NULL,
      aesthetics = c("color", "fill"),
      values = colours
    ) +
    theme_pdf(...) +
    ggplot2::theme(legend.margin = ggplot2::margin(b = -15))

  p$layers[[2]]$geom$default_aes$size   <- dot_size
  p$layers[[2]]$geom$default_aes$alpha  <- 0.6
  p$layers[[2]]$geom$default_aes$stroke <- 0.2

  if (plot_it) plot(p)
  return(p)
}
