#' Plot the distribution of the median RT across participants
#'
#' @param df A data frame containing participant responses with an `id` column
#' and a `rt_total` for each trial.
#' @param sd_mult A numeric value indicating how many standard deviations to use
#' for identifying suspicious median RTs. The default is 2, which means that
#' median RTs that are more than 2 standard deviations away from the mean
#' will be considered suspicious.
#' @param ... Additional arguments passed to the [theme_pdf()] function for
#' customizing the plot theme.
#'
#' @returns A ggplot2 object showing the distribution of median RTs across
#' participants, with suspicious median RTs highlighted.
#' @export
#'
#' @examples
#' df <- get_clean_data(sd_mult = 10)$df_expe
#' plot_median_rt_distribution(df, base_size = 12)
plot_median_rt_distribution <- function(df, sd_mult = 2, ...) {
  p <-
    df |>
    dplyr::group_by(.data$id) |>
    dplyr::mutate(median_rt = median(.data$rt_total)) |>
    dplyr::ungroup() |>
    dplyr::select("id", "median_rt") |>
    dplyr::distinct() |>
    ggplot2::ggplot(ggplot2::aes(x = .data$median_rt)) +
    ggplot2::geom_histogram(
      ggplot2::aes(
        color = (
          .data$median_rt <
            mean(.data$median_rt) - sd_mult * sd(.data$median_rt)
            # .data$median_rt >
            # mean(.data$median_rt) + sd_mult * sd(.data$median_rt)
        ),
        fill = (
          .data$median_rt <
            mean(.data$median_rt) - sd_mult * sd(.data$median_rt)
            # .data$median_rt >
            # mean(.data$median_rt) + sd_mult * sd(.data$median_rt)
        ),
      ),
      bins = 100,
      alpha = 0.3,
      linewidth = 0.2
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(
        xintercept = mean(.data$median_rt) - sd_mult * sd(.data$median_rt)
      ),
      color = "red",
      linetype = "dashed",
      linewidth = 0.3
    ) +
    # ggplot2::geom_vline(
    #   ggplot2::aes(
    #     xintercept = mean(.data$median_rt) + sd_mult * sd(.data$median_rt)
    #   ),
    #   color = "red",
    #   linetype = "dashed",
    #   linewidth = 0.3
    # ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 20)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.1))) +
    ggplot2::scale_discrete_manual(
      name = NULL,
      aesthetics = c("color", "fill"),
      breaks = c("TRUE", "FALSE"),
      values = c(
        "TRUE"  = palette.colors()[2],
        "FALSE" = palette.colors()[4]
      ),
      labels = c("Suspicious median RT", "Typical median RT")
    ) +
    ggplot2::labs(
      x = "Median RT (s)",
      y = "Count",
      title = "Distribution of median RT across participants"
    ) +
    theme_pdf(...)

  return(p)
}

#' Plot model predictions from the non-linear GAMM results
#'
#' @param df A data frame containing the model predictions with columns:
#' - `term`: the trial term as an integer from 1 to 4
#' - `Predicted`: the predicted response time
#' - `CI_low`: the lower bound of the confidence interval
#' - `CI_high`: the upper bound of the confidence interval
#' - `category`: the problem category (e.g., "Visual", "Control", "Spatial")
#' - `group`: the participant group
#' Results from model fits are saved natively in the package in the
#' `nl_predictions` object.
#' @param var A string indicating the variable to plot on the x-axis. Added for
#' backwards compatibility with an older version of the package where trial
#' phases were named "term", but have since been renamed "phase". Default is "term".
#' @param title An optional title for the plot. Default is NULL.
#' @param ... Additional arguments passed to the [theme_pdf()] function for
#' customising the plot theme.
#'
#' @returns A ggplot2 object showing the model predictions with confidence
#' intervals, faceted by participant group and coloured by problem category.
#' @export
#'
#' @examples
#' plot_nl(nl_predictions$vviq_2, base_size = 12)
plot_nl <- function(df, var = "term", title = NULL, ...) {
  p <-
    df |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = factor(.data[[var]]),
        y = .data$Predicted,
        color = .data$category,
        group = .data$category
      )) +
    ggplot2::geom_smooth(
      position = ggplot2::position_dodge(width = 0.3),
      se = FALSE,
      linewidth = 0.2
    ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = .data$CI_low, ymax = .data$CI_high),
      size = 0.1,
      linewidth = 0.2,
      position = ggplot2::position_dodge(width = 0.3)
    ) +
    ggplot2::facet_wrap(~ group) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Average response times per phase (s)",
      color = "Category"
    ) +
    ggplot2::scale_x_discrete(
      labels = c("Premise 1", "Premise 2", "Premise 3", "Conclusion")
    ) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(15)) +
    ggplot2::scale_discrete_manual(
      name = "Problem category: ",
      aesthetics = c("color", "fill"),
      values = c(
        Visual = palette.colors()[3],
        Control = palette.colors()[4],
        Spatial = palette.colors()[2]
      )
    ) +
    theme_pdf(
      base_theme = ggplot2::theme_minimal,
      # Custom theme arguments
      panel.grid.major.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "grey80", fill = NA),
      ...
    )
  return(p)
}
