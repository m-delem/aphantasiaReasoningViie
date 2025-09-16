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
      .by = tidyselect::any_of(grouping)
    )
  return(df_summary)
}

#' Custom ggsave wrapper set with Nature's formatting guidelines (width-locked)
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
#' @param plot     The ggplot object to save.
#' @param path     A character string with the path to save the plot.
#' @param ncol     The number of columns for the plot. Either 1 (default) or 2.
#' @param width    Optional. The width of the plot in mm. If NULL (default), it
#'                 will be set to 88 mm for one-column figures and 180 mm for
#'                 two-column figures.
#' @param height   The height of the plot in mm. Default is 90 mm.
#' @param show     Logical. Whether to return the plot visibly or not. Default
#' is FALSE, the plot is returned invisibly.
#' @param verbose  Logical. Whether to print a message in the console when the
#' saving is done. Default is TRUE.
#' @param units    The units for the width and height. Default is "mm".
#' @param dpi      The resolution of the plot. Default is 600.
#' @param ...      Additional arguments passed to `ggsave()`.
#'
#' @returns Nothing. The function saves the ggplot to the specified path.
#' @export
save_ggplot <- function(
    plot,
    path,
    ncol     = 1,
    width    = NULL,
    height   = 90,
    show     = FALSE,
    verbose  = TRUE,
    units    = "mm",
    dpi      = 600,
    ...
) {
  rlang::check_installed("here")

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
    units    = units,
    dpi      = dpi,
    ...
  )
  if (verbose) {
    message(glue::glue_col(
      "{magenta |-> {", colour, " {shape}} figure saved in {yellow {path}}.}"
    ))
  }
  if (show) return(plot)
  invisible(plot)
}

#' Add significance label and line to a plot
#'
#' @param df A dataframe containing one column per variable in the
#' desired aesthetics (x, y, colour, etc.) and the following columns:
#' - `x_star`: x position of the star label
#' - `y_star`: y position of the star label
#' - `stars`: the star label (e.g., "*", "**", "***")
#' - `x_line`: x position of the start of the line
#' - `x_line_end`: x position of the end of the line
#' - `y_line`: y position of the line
#' @param size_star Size of the star label. Default is 2.5.
#' @param lw Line width of the significance line. Default is 0.2.
#' @param color Color of the star label and line. Default is "black".
#' @param linetype Line type of the significance line. Default is "solid".
#'
#' @returns A list of ggplot2 layers that can be added to a ggplot object.
#' @export
#'
#' @examples
#' group_effect <-
#'  tibble::tibble(
#'     x_star     = 1.5,
#'     y_star     = 1.08,
#'     stars      = "**",
#'     x_line     = x_star - 0.5,
#'     x_line_end = x_star + 0.5,
#'     y_line     = 1.05
#'   )
#'
#' ggplot2::ggplot() +
#'   ggplot2::scale_x_discrete(limits = factor(c(1, 2))) +
#'   ggplot2::scale_y_continuous(limits = c(0, 1.1)) +
#'   ggplot2::labs(x = NULL, y = NULL) +
#'   add_significance(group_effect, size_star = 4)
add_significance <- function(
    df,
    size_star = 2.5,
    lw = 0.2,
    color = "black",
    linetype = "solid"
){
  list(
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(
        x     = .data$x_star,
        y     = .data$y_star,
        label = .data$stars
      ),
      size        = size_star,
      color       = color,
      inherit.aes = FALSE
    ),
    ggplot2::geom_segment(
      data = df,
      ggplot2::aes(
        x    = .data$x_line,
        xend = .data$x_line_end,
        y    = .data$y_line,
        yend = .data$y_line,
      ),
      color       = color,
      linewidth   = lw,
      linetype    = linetype,
      inherit.aes = FALSE
    )
  )
}
