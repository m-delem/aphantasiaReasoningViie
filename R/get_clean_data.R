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
        dplyr::select(.data$id),
      by = "id"
    ) |>
    filter_suspicious_rt_ids(sd_mult = sd_mult, verbose = verbose) |>
    factor_categories() |>
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

  clean_data <- list(
    df_expe   = df_expe,
    df_survey = df_survey
  )
  return(clean_data)
}
