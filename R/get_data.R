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
#' @returns A list containing two data frames:
#' - `df_expe`: The cleaned experiment data.
#' - `df_survey`: The cleaned survey data.
#' @export
#'
#' @examples
#' clean_data <- get_clean_data(verbose = TRUE)
#' head(clean_data$df_expe)
#' head(clean_data$df_survey)
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
    # factor_groups(n_groups = n_groups) |>
    create_all_groups() |>
    factor_chr_vars()

  df_survey <-
    aphantasiaReasoningViie::survey_data |>
    dplyr::filter(.data$id %in% df_expe$id) |>
    factor_strategies() |>
    # factor_groups(n_groups = n_groups) |>
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
    dplyr::relocate(cluster, .after = "group")

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
    dplyr::rename("group_4" = "group") |>
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
