#' Filter manually identified participant based on various criteria
#'
#' @param df A data frame containing participant information.
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
#' @param verbose Logical, whether to print the number of participants excluded
#' based on the criteria.
#'
#' @returns A filtered data frame with participants who meet the specified
#' criteria.
#' @export
#'
#' @examples
#' df <- filter_manually_identified_ids(survey_data)
#'
#' # We might want to try being less strict and keeping people who did not
#' # complete the OSIVQ or Raven, e.g. if we're only interested in the VVIQ
#' df <-
#'  filter_manually_identified_ids(
#'   survey_data,
#'   exclude_no_osivq = FALSE,
#'   exclude_no_raven = FALSE
#'  )
#' # Alternatively we could study a sample without any psychiatric/neurological
#' # troubles, etc.
filter_manually_identified_ids <- function(
    df,
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
    verbose = TRUE
) {
  count_ids_with <- function(df_to_count, column, condition) {
    n_ids <-
      df_to_count |>
      dplyr::filter(.data[[column]] == condition) |>
      dplyr::select("id", {{ column }}) |>
      unique() |>
      nrow()
    return(n_ids)
  }

  n_no_vviq    <- df |> count_ids_with("vviq_is_complete", FALSE)
  n_no_osivq   <- df |> count_ids_with("osivq_is_complete", FALSE)
  n_no_raven   <- df |> count_ids_with("raven_is_complete", FALSE)
  n_cheated    <- df |> count_ids_with("has_cheated", TRUE)
  n_distracted <- df |> count_ids_with("has_been_distracted", TRUE)
  n_treatment  <- df |> count_ids_with("has_treatment", TRUE)
  n_adhd       <- df |> count_ids_with("has_adhd", TRUE)
  n_asd        <- df |> count_ids_with("has_asd", TRUE)
  n_dyslexia   <- df |> count_ids_with("has_dyslexia", TRUE)
  n_other      <- df |> count_ids_with("has_other_neuro_trouble", TRUE)

  filtered_df <- df

  if (exclude_no_vviq) {
    vviq <- "Excluded"
    filtered_df <-
      filtered_df |>
      dplyr::filter(.data$vviq_is_complete == TRUE)
  } else vviq <- "Included"

  if (exclude_no_osivq) {
    osivq <- "Excluded"
    filtered_df <-
      filtered_df |>
      dplyr::filter(.data$osivq_is_complete == TRUE)
  } else osivq <- "Included"

  if (exclude_no_raven) {
    raven <- "Excluded"
    filtered_df <-
      filtered_df |>
      dplyr::filter(.data$raven_is_complete == TRUE)
  } else raven <- "Included"

  if (exclude_cheated) {
    cheated <- "Excluded"
    filtered_df <-
      filtered_df |>
      dplyr::filter(.data$has_cheated == FALSE)
  } else cheated <- "Included"

  if (exclude_distracted) {
    distracted <- "Excluded"
    filtered_df <-
      filtered_df |>
      dplyr::filter(.data$has_been_distracted == FALSE)
  } else distracted <- "Included"

  if (exclude_treatment) {
    treatment <- "Excluded"
    filtered_df <-
      filtered_df |>
      dplyr::filter(.data$has_treatment == FALSE)
  } else treatment <- "Included"

  if (exclude_adhd) {
    adhd <- "Excluded"
    filtered_df <-
      filtered_df |>
      dplyr::filter(.data$has_adhd == FALSE)
  } else adhd <- "Included"

  if (exclude_asd) {
    asd <- "Excluded"
    filtered_df <-
      filtered_df |>
      dplyr::filter(.data$has_asd == FALSE)
  } else asd <- "Included"

  if (exclude_dyslexia) {
    dyslexia <- "Excluded"
    filtered_df <-
      filtered_df |>
      dplyr::filter(.data$has_dyslexia == FALSE)
  } else dyslexia <- "Included"

  if (exclude_other) {
    other <- "Excluded"
    filtered_df <-
      filtered_df |>
      dplyr::filter(.data$has_other_neuro_trouble == FALSE)
  } else other <- "Included"

  n_before <- length(unique(df$id))
  n_after  <- length(unique(filtered_df$id))
  perc     <- round((n_before - n_after) / n_before * 100, 2)

  if (verbose) {
    message(glue::glue(
      "

    Sample size before manual examination: {n_before}
    Manually identified participants:
    - N without VVIQ: {n_no_vviq} -> {vviq}
    - N without OSIVQ: {n_no_osivq} -> {osivq}
    - N without Raven: {n_no_raven} -> {raven}
    - N who cheated: {n_cheated} -> {cheated}
    - N who were distracted: {n_distracted} -> {distracted}
    - N who had treatment: {n_treatment} -> {treatment}
    - N with ADHD: {n_adhd} -> {adhd}
    - N with ASD: {n_asd} -> {asd}
    - N with dyslexia: {n_dyslexia} -> {dyslexia}
    - N with other neuro troubles: {n_other} -> {other}
    Participants to exclude: {n_before - n_after} ({perc}%)
    "
    ))
  }
  return(filtered_df)
}

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
  n_before <- length(unique(df$id))

  filtered_df <- df |>
    dplyr::mutate(acc_perc = mean(.data$accuracy, na.rm = TRUE), .by = "id") |>
    dplyr::filter(.data$acc_perc > 0.5)

  n_after <- length(unique(filtered_df$id))
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

#' Filter (or mark) participants with suspicious median RTs
#'
#' @description
#' These two functions identify participants with suspiciously fast RTs overall
#' using their median RTs and comparing them to the sample mean of median RTs.
#'
#' `filter_suspicious_rt_ids` removes these participants from the data frame,
#' while `mark_suspicious_rt_ids` adds a column to flag them without removing
#' their data (e.g., for manual examination after identifying them).
#'
#' @param df A data frame containing participants' total response times for each
#' trial in a `rt_total` column.
#' @param sd_mult A numeric value indicating how many standard deviations to use
#' for identifying suspicious median RTs. The default is 2, which means that
#' median RTs that are more than 2 standard deviations away from the mean
#' will be considered suspicious.
#' @param verbose Logical, whether to print the number of participants excluded
#' or marked based on median RTs.
#'
#' @returns A data frame with suspicious participants removed (or marked).
#' @export
#'
#' @examples
#' df <- mark_suspicious_rt_ids(experiment_data)
#' df |>
#'   dplyr::filter(is_suspicious == TRUE) |>
#'   dplyr::select(id, median_rt) |>
#'   dplyr::distinct()
#'
#' df <- filter_suspicious_rt_ids(experiment_data)
#' length(unique(df$id))
filter_suspicious_rt_ids <- function(df, sd_mult = 2, verbose = TRUE) {

  filtered_df <-
    df |>
    dplyr::group_by(.data$id) |>
    dplyr::mutate(
      dplyr::across(tidyselect::contains("rt"), ~ . / 1000),
      median_rt = median(.data$rt_total)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      .data$median_rt >=
        mean(.data$median_rt) - sd_mult * sd(.data$median_rt),
      # .data$median_rt <=
      # mean(.data$median_rt) + sd_mult * sd(.data$median_rt)
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

#' @rdname filter_suspicious_rt_ids
#' @export
mark_suspicious_rt_ids <- function(df, sd_mult = 2, verbose = TRUE) {
  marked_df <-
    df |>
    dplyr::group_by(.data$id) |>
    dplyr::mutate(
      dplyr::across(tidyselect::contains("rt"), ~ . / 1000),
      median_rt = median(.data$rt_total)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      is_suspicious = ifelse(
        .data$median_rt <
          mean(.data$median_rt) - sd_mult * sd(.data$median_rt),
        # .data$median_rt >
        # mean(.data$median_rt) + sd_mult * sd(.data$median_rt),
        TRUE, FALSE
      )
    ) |>
    dplyr::ungroup()

  if (verbose) {
    n_suspicious <-
      marked_df |>
      dplyr::filter(.data$is_suspicious == TRUE) |>
      dplyr::select("id") |>
      unique() |>
      nrow()
    n_total <- length(unique(df$id))
    perc <- round(n_suspicious / n_total * 100, 2)
    message(glue::glue(
      "

      Marked {n_suspicious} suspicious median RTs ({perc}%) out of {n_total} IDs.
      "
    ))
  }

  return(marked_df)
}
