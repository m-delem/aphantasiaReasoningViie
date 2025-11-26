#' Wrapper function to get clean "analysis-ready" data
#'
#' @description
#' This function retrieves and cleans the data for the experiment and survey. It
#' uses several helper functions to filter and format the data, including
#' [filter_random_accuracy_ids()], [filter_manually_identified_ids()],
#' [filter_suspicious_rt_ids()], [factor_categories()], [factor_groups()],
#' [factor_chr_vars()], [factor_strategies()], and [compute_nieq_scores()].
#' The cleaned data is returned as a list containing two data frames:
#' `df_expe` and `df_survey`. The `df_expe` data frame contains the cleaned
#' experiment data, while the `df_survey` data frame contains the cleaned
#' survey data.
#'
#' @param type The type of data to return: "experiment", "survey", or "all".
#' @param n_groups The number of groups to factor in the data. Must be 2, 3 or
#' 4. 2 divides the sample into Aphants and Typical imagers using the 32 VVIQ
#' criterio, 3 divides the sample into Aphants (VVIQ = 16), Hypophants
#' (VVIQ < 32) and Typical imagers, and 4 also isolates Hyperphants with
#' VVIQ > 75.
#' @param exclude_no_vviq Logical, whether to exclude participants without VVIQ.
#' @param exclude_no_osivq Logical, whether to exclude participants without
#' OSIVQ.
#' @param exclude_no_raven Logical, whether to exclude participants without
#' Raven.
#' @param exclude_cheated Logical, whether to exclude participants who have
#' cheated (based on self-report).
#' @param exclude_distracted Logical, whether to exclude participants who have
#' been distracted (based on self-report).
#' @param exclude_treatment Logical, whether to exclude participants who have
#' a treatment for a neurological or psychiatric disorder.
#' @param exclude_adhd Logical, whether to exclude participants who have ADHD.
#' @param exclude_asd Logical, whether to exclude participants who have ASD.
#' @param exclude_dyslexia Logical, whether to exclude participants who have
#' dyslexia.
#' @param exclude_other Logical, whether to exclude participants who have
#' other neurological troubles.
#' @param sd_mult A numeric value indicating how many standard deviations to use
#' for identifying suspicious median RTs. The default is 2.25, which means that
#' median RTs that are more than 2.25 standard deviations inferior to the mean
#' are considered suspiciously fast and potential "spamming".
#' @param verbose A logical value indicating whether to print verbose messages
#' about the filtering process. Default is `FALSE`.
#'
#' @returns
#' A cleaned data frame or a list of cleaned data frames, depending on the
#' `type` parameter:
#' - If `type` is "experiment", returns the cleaned experiment data frame.
#' - If `type` is "survey", returns the cleaned survey data frame.
#' - If `type` is "all", returns a list containing both cleaned data frames.
#' @export
#'
#' @examples
#' clean_data <- get_clean_data(type = "all", verbose = TRUE)
#' colnames(get_clean_data(type = "experiment"))
#' colnames(get_clean_data(type = "survey"))
get_clean_data <- function(
    type = "all",
    n_groups = 2,
    exclude_no_vviq = TRUE,
    exclude_no_osivq = TRUE,
    exclude_no_raven = TRUE,
    exclude_cheated = TRUE,
    exclude_distracted = TRUE,
    exclude_treatment = FALSE,
    exclude_adhd = FALSE,
    exclude_asd = FALSE,
    exclude_dyslexia = FALSE,
    exclude_other = FALSE,
    sd_mult = 2.25,
    verbose = FALSE
) {
  df_expe <-
    dplyr::inner_join(
      filter_random_accuracy_ids(
        aphantasiaReasoningViie::experiment_data,
        verbose = verbose
      ),
      filter_manually_identified_ids(
        aphantasiaReasoningViie::survey_data,
        exclude_no_vviq = exclude_no_vviq,
        exclude_no_osivq = exclude_no_osivq,
        exclude_no_raven = exclude_no_raven,
        exclude_cheated = exclude_cheated,
        exclude_distracted = exclude_distracted,
        exclude_treatment = exclude_treatment,
        exclude_adhd = exclude_adhd,
        exclude_asd = exclude_asd,
        exclude_dyslexia = exclude_dyslexia,
        exclude_other = exclude_other,
        verbose = verbose
      ) |>
        dplyr::select("id", tidyselect::starts_with("strats_dlc_")),
      by = "id"
    ) |>
    filter_suspicious_rt_ids(sd_mult = sd_mult, verbose = verbose) |>
    factor_categories() |>
    factor_strategies() |>
    create_all_groups() |>
    factor_chr_vars()

  df_survey <-
    aphantasiaReasoningViie::survey_data |>
    dplyr::filter(.data$id %in% df_expe$id) |>
    factor_strategies() |>
    create_all_groups() |>
    factor_chr_vars() |>
    compute_nieq_scores()

  if (type == "experiment") {
    return(df_expe)
  } else if (type == "survey") {
    return(df_survey)
  } else if (type == "all") {
    # Return all data
    clean_data <- list(
      df_expe   = df_expe,
      df_survey = df_survey
    )
    return(clean_data)
  } else {
    stop(glue::glue_col(
      "Invalid value for 'data'. Must be '{cyan experiment}', ",
      "'{yellow survey}', or {green all}'."
      )
    )
  }
}

#' Wrapper function to get clean and clustered data immediately
#'
#' @description
#' This function retrieves clean data using [get_clean_data()] and then
#' performs clustering on the OSIVQ data using [cluster_osivq()]. It then
#' adds named clusters to the survey data using [add_named_clusters()] and
#' merges the cluster information with the experiment data. Depending on the
#' `type` parameter, it returns either the cleaned experiment data with cluster
#' information, the cleaned survey data with cluster information, the clustering
#' results, or all of these as a list. This is a convenience function to
#' streamline the process of obtaining clustered data for analysis, but its
#' defaults are based on a first iteration of this clustering procedure, which
#' is described in full in the vignette `vignette("osivq_clusters")`.
#'
#' @param type The type of data to return: "experiment", "survey", "clustering",
#' or "all".
#' @param names A character vector of names for the clusters passed to
#' [add_named_clusters()]. Default is
#' `c("Spatialiser", "Visualiser", "Verbaliser")`.
#' @param levels A character vector of levels for the factor passed to
#' [add_named_clusters()]. Default is
#' `c("Visualiser", "Spatialiser", "Verbaliser")`.
#' @param contrasts A character vector of contrasts for the factor levels
#' passed to [add_named_clusters()].
#' @param base An integer indicating the base level for the contrasts. Default
#' is 1, which corresponds to the first cluster in `names`.
#' @returns
#' A cleaned and clustered data frame or a list of cleaned and clustered data
#' frames, depending on the `type` parameter:
#' - If `type` is "experiment", returns the cleaned experiment data frame with
#'  cluster information.
#' - If `type` is "survey", returns the cleaned survey data frame with cluster
#'  information.
#' - If `type` is "clustering", returns the clustering results.
#' - If `type` is "all", returns a list containing the cleaned experiment data
#'  frame with cluster information, the cleaned survey data frame with cluster
#'  information, and the clustering results.
#' @export
#'
#' @examples
#' colnames(get_clustered_data(type = "experiment"))
get_clustered_data <- function(
    type = "all",
    names     = c("Spatialiser", "Visualiser", "Verbaliser"),
    levels    = c("Visualiser", "Spatialiser", "Verbaliser"),
    contrasts = c("_visualiser", "_spatialiser", "_verbaliser"),
    base = 1
) {
  df_survey  <- get_clean_data("survey")

  # Clustering OSIVQ data
  clustering <- cluster_osivq(df_survey)

  # Adding named clusters to the survey data
  df_survey <- add_named_clusters(
    df_survey, clustering,
    names  = names,
    levels = levels,
    contrasts = contrasts,
    base = base
  )

  # Merging with experiment data
  df_expe <-
    dplyr::left_join(
      get_clean_data("experiment"),
      df_survey |> dplyr::select(id, cluster),
      by = dplyr::join_by("id")
    ) |>
    dplyr::relocate(cluster, .after = "group_4")

  if (type == "experiment") {
    return(df_expe)
  } else if (type == "survey") {
    return(df_survey)
  } else if (type == "clustering") {
    return(clustering)
  } else if (type == "all") {
    # Return all data
    clustered_data <- list(
      df_expe   = df_expe,
      df_survey = df_survey,
      clustering = clustering
    )
    return(clustered_data)
  } else {
    stop(glue::glue_col(
      "Invalid value for 'data'. Must be '{cyan experiment}', ",
      "'{yellow survey}', '{magenta clustering}', or {green all}'."
      )
    )
  }
}

#' Get data with the Visual Imagery Impedance Effect manually calculated
#'
#' @description
#' Another approach to our main problem (evaluating the individual differences
#' in contrasts between categories for, e.g., response times) could be to
#' compute the category contrasts in the outcome variable manually and then
#' compare individuals or groups on these difference scores. This is less
#' powerful than using mixed-effects models with interaction terms, but some
#' might wonder what comes out of such an approach. This function computes
#' these difference scores between response times in the visual category minus
#' the other categories (spatial, control, and non-visual, an average of
#' control and spatial) to create three Visual Imagery Impedance Effect (VIIE)
#' scores per participant. It returns a data frame with these VIIE scores along
#' with relevant individual difference (questionnaire) variables.
#'
#' @param ... Additional arguments passed to [get_clustered_data()].
#'
#' @returns A data frame with one row per participant containing their
#' questionnaire scores and groups along with three VIIE scores:
#' `viie_total` (mean visual RT minus mean non-visual RT), `viie_spatial`
#' (visual RT minus spatial RT), and `viie_control` (visual RT minus control
#' RT).
#' @export
#'
#' @examples
#' colnames(get_viie_data())
get_viie_data <- function(...) {
  df_viie <-
    dplyr::left_join(
      get_clean_data("survey") |>
        dplyr::select(
          "id", "vviq_total_score",
          "osivq_object", "osivq_spatial", "osivq_verbal"
        ),
      get_clustered_data("experiment", ...),
      by = dplyr::join_by("id")
    ) |>
    filter_trials_on_rt(verbose = FALSE) |>
    dplyr::mutate(
      category_2 = ifelse(
        .data$category == "Visual",
        "Visual", "Non_visual")
    ) |>
    dplyr::mutate(
      mean_rt_1 = mean(.data$rt_total), .by = c("id", "category")) |>
    dplyr::mutate(
      mean_rt_2 = mean(.data$rt_total), .by = c("id", "category_2")) |>
    dplyr::select(
      "id":"osivq_verbal",
      "group_4":"strategy_group",
      "category_1" = "category", "category_2",
      "mean_rt_1", "mean_rt_2"
    ) |>
    dplyr::distinct() |>
    tidyr::nest(
      data_1 = c("category_1", "mean_rt_1"),
      data_2 = c("category_2", "mean_rt_2")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(dplyr::across(
      tidyselect::starts_with("data_"),
      ~ list(
        .x |>
          dplyr::distinct() |>
          dplyr::rename(
            category = 1,
            rt       = 2
          ) |>
          tidyr::pivot_wider(
            names_from = "category",
            values_from = "rt"
          )
    ))) |>
    tidyr::unnest("data_1") |>
    tidyr::hoist("data_2", "Non_visual") |>
    dplyr::select(
      "id":"strategy_group",
      "control_rt" = "Control",
      "spatial_rt" = "Spatial",
      "visual_rt"  = "Visual",
      "non_vis_rt" = "Non_visual"
    ) |>
    dplyr::mutate(
      viie_total   = .data$visual_rt - .data$non_vis_rt,
      viie_spatial = .data$visual_rt - .data$spatial_rt,
      viie_control = .data$visual_rt - .data$control_rt
    ) |>
    dplyr::relocate(tidyselect::starts_with("viie"), .after = "cluster")

  return(df_viie)
}
