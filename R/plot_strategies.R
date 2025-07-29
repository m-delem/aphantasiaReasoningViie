#' Plot proportions of strategy use for groups as barplots
#'
#' @param df_long A data frame containing the strategies data in a long format
#' with columns for grouping, strategy, and score.
#' @param grouping A variable to group the data by, typically group or cluster.
#' Can be unquoted.
#' @param ... Additional arguments passed to the [theme_pdf()] function for
#' customizing the plot theme.
#'
#' @returns A ggplot2 object showing the proportions of strategy use for each
#' group as a barplot, with strategies as facets.
#' @export
#'
#' @examples
#' df_long <-
#'   get_clean_data()$df_survey |>
#'   pivot_strategies_longer()
#'
#' plot_strategies_barplot(df_long, grouping = group)
plot_strategies_barplot <- function(df_long, grouping = "group", ...) {
  df_plot <-
    df_long |>
    dplyr::mutate(
      {{ grouping }} := forcats::fct_relevel(
        {{ grouping }},
        rev(levels(dplyr::pull(df_long, {{ grouping }})))
      ),
      score = as.numeric(.data$score)
    ) |>
    dplyr::select({{ grouping }}, "strategy", "score") |>
    dplyr::group_by({{ grouping }}, .data$strategy) |>
    dplyr::mutate(n = dplyr::n()) |>
    dplyr::group_by({{ grouping }}, .data$strategy, .data$score) |>
    dplyr::mutate(perc = round(dplyr::n() / .data$n, 3)) |>
    dplyr::distinct() |>
    dplyr::ungroup() |>
    dplyr::mutate(
      perc_label = paste0(round(.data$perc * 100, 0), "%"),
      label_black = ifelse(.data$score >= 4, .data$perc_label, ""),
      label_white = ifelse(.data$score <= 3, .data$perc_label, ""),
    )

  scale_labels <- c(
    "Not used",
    "Used secondary to\nanother strategy",
    "Used equally with\nanother strategy",
    "Dominantly used",
    "Only used"
  )

  p <-
    df_plot |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = {{ grouping }},
        y = .data$perc,
        fill = factor(.data$score),
        color = factor(.data$score),
      )
    ) +
    ggplot2::geom_bar(stat = "identity", position = "fill") +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$label_black),
      color = "black",
      position = ggplot2::position_fill(vjust = 0.5),
      size = 1.5,
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$label_white),
      color = "white",
      position = ggplot2::position_fill(vjust = 0.5),
      size = 1.5,
    ) +
    ggplot2::scale_colour_viridis_d(labels = scale_labels) +
    ggplot2::scale_fill_viridis_d(labels = scale_labels) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_reverse(expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(
      ~ .data$strategy,
      ncol = 1,
      strip.position = "top",
      labeller = ggplot2::label_wrap_gen(width = 10)
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_pdf(
      base_theme = ggplot2::theme_minimal,
      legend.text.position = "top",
      legend.justification = "center",
      legend.key.height = grid::unit(2, "mm"),
      legend.margin = ggplot2::margin(3, 3, 3, 3),
      legend.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y  = ggplot2::element_text(margin = ggplot2::margin(r = 4)),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      ...
    )

  return(p)
}

#' Plot mean strategy scores for groups
#'
#' @param df_long A data frame containing the strategies data in a long format
#' with columns for grouping, strategy, and score.
#' @param grouping A variable to group the data by, typically group or cluster.
#' Can be unquoted.
#' @param x_labels Optional labels for the x-axis, if different from the
#' grouping variable.
#' @param ... Additional arguments passed to the [theme_pdf()] function for
#' customizing the plot theme.
#'
#' @returns A ggplot2 object showing the mean strategy scores for each group,
#' with strategies as colors and filled points, and error bars representing
#' standard error of the mean.
#' @export
#'
#' @examples
#' df_long <-
#'   get_clean_data()$df_survey |>
#'   pivot_strategies_longer()
#'
#' plot_strategies_scores(df_long, grouping = group)
plot_strategies_scores <- function(
    df_long,
    grouping = "group",
    x_labels = NULL,
    ...
) {
  df_plot <-
    df_long |>
    dplyr::group_by({{ grouping }}, .data$strategy) |>
    dplyr::mutate(
      score = as.numeric(.data$score),
      mean = mean(.data$score) |> round(2),
      sd   = sd(.data$score) |> round(2),
      se   = .data$sd / 1.5 |> round(2)
    ) |>
    dplyr::ungroup()

  df_mean <-
    df_plot |>
    dplyr::select("group":"strategy", "mean":"se") |>
    dplyr::distinct()

  scale_labels <- c(
    "Not used",
    "Used secondary to\nanother strategy",
    "Used equally with\nanother strategy",
    "Dominantly used",
    "Only used"
  )

  p <-
    df_plot |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = {{ grouping }},
        y = .data$score,
        color = .data$strategy,
        fill  = .data$strategy,
      )
    ) +
    ggplot2::geom_point(
      size  = 1,
      alpha = 0.5,
      position = ggplot2::position_jitterdodge(
        jitter.width  = 0.1,
        jitter.height = 0.1,
        dodge.width   = 0.5
      ),
      na.rm = TRUE
    ) +
    # Connecting the points from the same strategy with a smooth curve
    ggplot2::geom_smooth(
      data = df_mean,
      ggplot2::aes(
        y = .data$mean,
        group = .data$strategy
      ),
      method = "loess",
      span = 1,
      se = FALSE,
      position = ggplot2::position_dodge(width = 0.5),
      linewidth = 0.4,
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    # Adding the points and SD representations for each strategy
    ggplot2::geom_pointrange(
      data = df_mean,
      ggplot2::aes(
        y = .data$mean,
        ymin = .data$mean - .data$se,
        ymax = .data$mean + .data$se,
      ),
      position = ggplot2::position_dodge(width = 0.5),
      size = 0.4,
      linewidth = 0.2,
      na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(1:5),
      labels = scale_labels,
      limits = c(0.8, 5.2),
      expand = ggplot2::expansion(c(0.01, 0))
    ) +
    ggplot2::scale_discrete_manual(
      name = "Strategy: ",
      aesthetics = c("colour", "fill"),
      values = c(
        Visual   = "#1b6096",
        Verbal   = "#793183",
        Semantic = "#db6100",
        Spatial  = "#318f2c",
        Sensorimotor = "#d4629e"
      ),
      limits = c("Visual", "Spatial", "Verbal", "Semantic", "Sensorimotor")
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_pdf(axis_relative_size = 1, ...)

  if (!is.null(x_labels)) {
    p <- p + ggplot2::scale_x_discrete(labels = x_labels)
  }

  return(p)
}
