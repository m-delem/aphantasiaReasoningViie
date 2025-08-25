#' Compute NIEQ scores by combining the frequency and proportion items of each
#' subscale
#'
#' @param df A data frame containing the NIEQ data with columns for frequency
#' and proportion of each subscale.
#'
#' @returns A data frame with new columns for NIEQ scores, calculated as the
#' mean of the frequency and proportion items for each subscale.
#' @export
#'
#' @examples
#' df <- compute_nieq_scores(survey_data)
#' df |>
#'   dplyr::filter(nieq_is_complete == TRUE) |>
#'   dplyr::select("id", tidyselect::contains("nieq_")) |>
#'   head()
compute_nieq_scores <- function(df) {
  df_scored <-
    df |>
    dplyr::mutate(
      nieq_voice    =
        (.data$nieq_freq_inner_voice + .data$nieq_prop_inner_voice) / 2,
      nieq_visual   =
        (.data$nieq_freq_mental_imagery + .data$nieq_prop_mental_imagery) / 2,
      nieq_emotions =
        (.data$nieq_freq_emotions + .data$nieq_prop_emotions) / 2,
      nieq_sensory  =
        (.data$nieq_freq_sensory_focus + .data$nieq_prop_sensory_focus) / 2,
      nieq_abstract =
        (.data$nieq_freq_unsymbolised + .data$nieq_prop_unsymbolised) / 2
    ) |>
    dplyr::relocate("nieq_voice":"nieq_abstract", .after = "nieq_is_complete")
  return(df_scored)
}

#' Helper function to quickly describe the cleaned survey data
#'
#' @param df A data frame containing the survey data columns related to the
#' questionnaires. Note that the functions computes means for the combined
#' NIEQ scores, so the data frame must have gone through
#' [compute_nieq_scores()].
#'
#' @returns A data frame summarising the survey data grouped by VVIQ group,
#' including sample size, gender distribution, mean scores for VVIQ, OSIVQ
#' sub-scales, Raven matrices and NIEQ sub-scales, along with the number of
#' participants who completed the NIEQ questionnaire (which was part of another
#' online experiment, so completely optional).
#' @export
#'
#' @examples
#' # The filtering function removes participants with bad or incomplete data,
#' # notably those who did not complete certain questionnaires.
#' survey_data |>
#'  filter_manually_identified_ids() |>
#'  compute_nieq_scores() |>
#'  describe_survey_data()
#'
#' @keywords internal
describe_survey_data <- function(df, grouping = NULL) {
  mean_sd <- function(x) {
    glue::glue(
      "{mean(x, na.rm = TRUE) |> round(2)} ({sd(x, na.rm = TRUE) |> round(2)})"
    ) |> as.character()
  }

  df_summary <-
    df |>
    # dplyr::group_by(.data$group) |>
    dplyr::reframe(
      # N = dplyr::n(),
      # Female = sum(.data$gender == "f"),
      # Other  = sum(!(.data$gender %in% c("m", "f"))),
      N = glue::glue(
        "{dplyr::n()} ({sum(.data$gender == 'f')} F, ",
        "{sum(!(.data$gender %in% c('m', 'f')))} O)"
      ),
      Age    = mean_sd(.data$age),
      VVIQ   = mean_sd(.data$vviq_total_score),
      `OSIVQ-Object`   = mean_sd(.data$osivq_object),
      `OSIVQ-Spatial`  = mean_sd(.data$osivq_spatial),
      `OSIVQ-Verbal`   = mean_sd(.data$osivq_verbal),
      `RSPM-18`    = mean_sd(.data$raven_score),
      nieq_completed = sum(.data$nieq_is_complete == TRUE),
      voice    = mean_sd(.data$nieq_voice),
      visual   = mean_sd(.data$nieq_visual),
      emotions = mean_sd(.data$nieq_emotions),
      sensory  = mean_sd(.data$nieq_sensory),
      abstract = mean_sd(.data$nieq_abstract),
      .by = grouping
    )
  return(df_summary)
}

#' Custom ggsave wrapper set with Nature's formatting guidelines
#'
#' @description
#' See:
#' https://www.nature.com/documents/NRJs-guide-to-preparing-final-artwork.pdf
#' It's pretty strict. A one column figure is 88 mm wide and a two column figure
#' is 180 mm wide. Depending on the length of the figure caption, there are
#' different maximum heights (see the PDF). Most figures types must be in vector
#' format to prevent quality loss when zooming in. Ever since I found these
#' guidelines, I use them for all figures, even if they are not for Nature...
#' Because it looks nice and I like it.
#'
#' @param plot     The plot to save.
#' @param path     The path to save the plot.
#' @param ncol     The number of columns for the plot. Either 1 (default) or 2.
#' @param height   The height of the plot in mm. Default is 90 mm.
#' @param width    Optional. The width of the plot in mm. If NULL (default), it
#'                 will be set to 88 mm for one-column figures and 180 mm for
#'                 two-column figures.
#' @param print_it Logical. Whether to print the plot in the console. Default is
#'                 FALSE.
#' @param verbose  Logical. Whether to print a message in the console. Default
#'                 is TRUE.
#' @param ...      Additional arguments passed to `ggsave()`.
#'
#' @returns Nothing. The function saves the plot to the specified path.
#' @export
save_plot <- function(
    plot,
    path,
    ncol     = 1,
    height   = 90,
    width    = NULL,
    print_it = FALSE,
    verbose  = TRUE,
    ...
) {
  if (!is.null(width)) {
    width <- width
    colour <- "blue"
    shape  <- "Custom width"
  } else if (ncol == 1) {
    # Standard width for single column figures
    width  <- 88
    colour <- "green"
    shape  <- "One-column"
  } else if (ncol == 2) {
    # Standard width for double column figures
    width  <- 180
    colour <- "cyan"
    shape  <- "Two-column"
  } else stop(glue::glue_col("ncol must be {cyan 1} or {green 2}."))

  ggplot2::ggsave(
    filename = here::here(path),
    plot     = plot,
    width    = width,
    height   = height,
    units    = "mm",
    dpi      = 600,
    ...
  )
  if (verbose) {
    print(glue::glue_col(
      "{magenta |-> {", colour, " {shape}} figure saved in {yellow {path}}.}"
    ))
  }
  if (print_it) print(plot) |> suppressMessages() |> suppressWarnings()
}
