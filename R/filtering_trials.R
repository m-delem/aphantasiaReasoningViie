#' Filter outlier trials based on mean response time per participant
#'
#' @param df A data frame containing trial data with columns for id, Group,
#' trial_number, category, premise_1_rt, premise_2_rt, premise_3_rt,
#' conclusion_rt and accuracy.
#' @param mult A multiplier for the standard deviation to set the threshold for
#' outlier detection. Default is 2.
#' @param min_rt Minimum response time in seconds for trials to be considered
#' valid. Default is 0.6 seconds.
#' @param max_rt Maximum response time in seconds for trials to be considered
#' valid. Default is 30 seconds.
#' @param verbose Logical. Whether to print a summary of the filtering
#' process. Default is FALSE.
#' @param ... Additional arguments passed to the function. Unused.
#'
#' @returns A filtered data frame containing only valid trials, with outliers
#' removed based on the specified thresholds.
#' @export
#'
#' @examples
#' df_rt <-
#'   get_clean_data()$df_expe |>
#'   filter_trials_on_rt()
filter_trials_on_rt <- function(
    df,
    mult   = 2,
    min_rt = 0.6,
    max_rt = 30,
    verbose = FALSE,
    ...
) {
  df_filtered <-
    df |>
    dplyr::filter(
      .data$accuracy == 1 &
        .data$premise_1_rt > min_rt &
        .data$premise_2_rt > min_rt &
        .data$premise_3_rt > min_rt &
        .data$conclusion_rt > min_rt &
        .data$premise_1_rt < max_rt &
        .data$premise_2_rt < max_rt &
        .data$premise_3_rt < max_rt &
        .data$conclusion_rt < max_rt
    ) |>
    dplyr::mutate(
      .by = c("id", "category"),
      start_temp_cols = "temporary cols start here",
      id_mean_p1  = mean(.data$premise_1_rt),
      id_mean_p2  = mean(.data$premise_2_rt),
      id_mean_p3  = mean(.data$premise_3_rt),
      id_mean_c   = mean(.data$conclusion_rt),
      id_sd_p1    = stats::sd(.data$premise_1_rt),
      id_sd_p2    = stats::sd(.data$premise_2_rt),
      id_sd_p3    = stats::sd(.data$premise_3_rt),
      id_sd_c     = stats::sd(.data$conclusion_rt),
      id_thres_p1 = .data$id_mean_p1 + mult * .data$id_sd_p1,
      id_thres_p2 = .data$id_mean_p2 + mult * .data$id_sd_p2,
      id_thres_p3 = .data$id_mean_p3 + mult * .data$id_sd_p3,
      id_thres_c  = .data$id_mean_c  + mult * .data$id_sd_c,
      end_temp_cols = "temporary cols end here",
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      good_trial = ifelse(
        .data$premise_1_rt < .data$id_thres_p1 &
          .data$premise_2_rt < .data$id_thres_p2 &
          .data$premise_3_rt < .data$id_thres_p3 &
          .data$conclusion_rt < .data$id_thres_c,
        TRUE, FALSE
      ),
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$good_trial) |>
    dplyr::select(!c(
      "good_trial",
      "start_temp_cols":"end_temp_cols",
      tidyselect::contains("response"),
      tidyselect::contains("acc"),
      tidyselect::contains("median_rt")
    ))

  if (verbose) {
    n_before      <- nrow(df)
    n_acc_removed <- sum(df$accuracy == 0)
    n_acc_perc    <- round(n_acc_removed / n_before * 100, 2)
    n_filter_removed <- n_before - n_acc_removed - nrow(df_filtered)
    n_filter_perc <-
      round(n_filter_removed / (n_before - n_acc_removed) * 100, 2)
    message(glue::glue("

    Outlier trials filtration summary
    {n_before} trials before filtering
    {n_acc_removed} incorrect trials removed ({n_acc_perc}%)
    {n_filter_removed} trials filtered based on mean + {mult} * SD
    ({n_filter_perc}% of remaining trials)
    {nrow(df_filtered)} trials remaining after filtering
    "
    ))
  }
  return(df_filtered)
}

#' Get the count of trials per participant and category
#'
#' @param df A data frame containing trial data with columns for id, group, and
#' category.
#'
#' @returns A data frame summarizing the number of trials per participant and
#' category, including total trials and a flag for low trial counts.
#' @export
#'
#' @examples
#' df <- get_clean_data()$df_expe
#' get_trial_count(df) |> head()
#'
#' df_rt <- filter_trials_on_rt(df)
#' # Allows to identify and examine the participants that lost a lot of trials
#' get_trial_count(df_rt) |> head()
#'
#' @keywords internal
get_trial_count <- function(df) {
  df_trials <-
    df |>
    dplyr::group_by(.data$id, .data$group, .data$category) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across("group":"category", stringr::str_to_title)) |>
    tidyr::pivot_wider(
      names_from = .data$category,
      values_from = .data$n,
      values_fill = 0
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      n_trials_total = .data$Visual + .data$Control + .data$Spatial,
      low_trials = ifelse(
        .data$Visual < 3 | .data$Control < 3 | .data$Spatial < 3,
        TRUE, FALSE
      )
    ) |>
    dplyr::arrange(.data$n_trials_total) |>
    dplyr::mutate(id = factor(.data$id))

  return(df_trials)
}
