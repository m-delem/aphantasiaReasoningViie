get_clean_data <- function(n_groups = 2, sd_mult = 2, verbose = FALSE) {
  df_expe <-
    dplyr::inner_join(
      filter_random_accuracy_ids(experiment_data, verbose = verbose),
      filter_manually_identified_ids(survey_data, verbose = verbose) |>
        dplyr::select(id),
      by = "id"
    ) |>
    filter_suspicious_rt_ids(sd_mult = sd_mult, verbose = verbose) |>
    factor_categories() |>
    factor_groups(n_groups = n_groups) |>
    factor_chr_vars()

  df_survey <-
    survey_data |>
    dplyr::filter(id %in% df_expe$id) |>
    factor_strategies() |>
    factor_groups(n_groups = n_groups) |>
    factor_chr_vars() |>
    compute_nieq_scores()

  clean_data <- list(
    df_expe = df_expe,
    df_survey = df_survey
  )
  return(clean_data)
}
