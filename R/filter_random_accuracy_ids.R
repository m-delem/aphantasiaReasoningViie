#' Filter participants with below random accuracy
#'
#' @param df A data frame containing participant responses and accuracy.
#' @param verbose Logical, whether to print the number of participants excluded
#' based on accuracy.
#'
#' @returns A filtered data frame with participants who have an accuracy above
#' 50% across all trials.
#' @export
filter_random_accuracy_ids <- function(df, verbose = TRUE) {
  n_before <- df$id |> unique() |> length()

  filtered_df <- df |>
    dplyr::mutate(acc_perc = mean(accuracy, na.rm = TRUE), .by = "id") |>
    dplyr::filter(acc_perc > 0.5)

  n_after <- filtered_df$id |> unique() |> length()
  perc <- round((n_before - n_after) / n_before * 100, 2)

  if (verbose) {
    message(glue::glue(
      "

    Sample size before accuracy analysis: {n_before}
    Participants below random accuracy (<= 50%): {n_before - n_after} ({perc}%)
    "
    ))
  }
  return(filtered_df)
}
