#' Filter participants with suspicious median RTs
#'
#' @param df A data frame containing participant responses and median RTs.
#' @param sd_mult A numeric value indicating how many standard deviations to use
#' for identifying suspicious median RTs. The default is 2, which means that
#' median RTs that are more than 2 standard deviations away from the mean
#' will be considered suspicious.
#'
#' @returns A filtered data frame with participants who have median RTs within
#' the specified range.
#' @export
filter_suspicious_rt_ids <- function(df, sd_mult = 2, verbose = TRUE) {

  filtered_df <-
    df |>
    dplyr::group_by(id) |>
    dplyr::mutate(
      dplyr::across(tidyselect::contains("rt"), ~ . / 1000),
      median_rt = median(rt_total)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      median_rt >= mean(median_rt) - sd_mult * sd(median_rt) &
        median_rt <= mean(median_rt) + sd_mult * sd(median_rt)
    )

  n_before <- length(unique(df$id))
  n_after  <- length(unique(filtered_df$id))
  perc     <- round((n_before - n_after) / n_before * 100, 2)

  if (verbose) {
    message(glue::glue(
      "

    Sample size before median RTs analysis: {n_before}
    Participants with median RTs outside {sd_mult} SDs: {n_before - n_after} ({perc}%)
    "
    ))
  }

  return(filtered_df)
}
