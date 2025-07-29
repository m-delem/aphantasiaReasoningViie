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
#' plot_median_rt_distribution(df)
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
