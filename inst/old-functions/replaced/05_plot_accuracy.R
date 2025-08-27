#' Plot accuracy by group and category
#'
#' @param df A data frame containing the accuracy data.
#' @param title         (Optional) A string for the plot title.
#' @param subtitle      (Optional) A string for the plot subtitle.
#' @param print_it      Logical. Whether to print the plot.
#' @param dot_alpha     Numeric. The alpha level for the dots. Default is 0.22.
#' @param jitter_width  Numeric. The width of the jitter for the points. Default
#'                      is 0.13.
#' @param jitter_height Numeric. The height of the jitter for the points. 
#'                      Default is 0.02.
#' @param ... Other arguments passed to `ggplot2::theme()`.
#'
#' @return A ggplot object.
plot_accuracy <- function(
    df, 
    x_var,
    colour_var,
    group_var,
    title    = NULL,
    subtitle = NULL,
    print_it = FALSE, 
    dot_alpha = 0.22,
    jitter_width = 0.13,
    jitter_height = 0.02,
    ...
) {
  # Helper function to get the centrality statistic
  get_centrality_wide <- function(df, centrality_stat, ...) {
    df_central <- 
      df |> 
      dplyr::group_by(...) |>
      dplyr::reframe(
        sd = sd(mean_acc),
        se = sd / sqrt(length(mean_acc)),
        mean_acc = centrality_stat(mean_acc),
        lower = mean_acc - sd,
        upper = mean_acc + sd,
      )
    return(df_central)
  }
  
  p <- 
    df |>     
    dplyr::select(id, {{ group_var }}, category, mean_acc) |> 
    dplyr::distinct() |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x     = {{ x_var }},
        y     = mean_acc,
        color = {{ colour_var }},
        fill  = {{ colour_var }}
      )
    ) + 
    ggplot2::geom_hline(
      yintercept = 0.5,
      linewidth  = 0.3,
      linetype   = "dashed",
      color      = "grey50"
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      linewidth  = 0.3,
      linetype   = "dashed",
      color      = "grey50"
    ) +
    ggplot2::geom_point(
      size     = 1.6,
      alpha    = dot_alpha,
      stroke   = 0.5,
      position = ggplot2::position_jitterdodge(
        jitter.width  = jitter_width,
        jitter.height = jitter_height,
        dodge.width    = 0.65
      ),
      na.rm     = TRUE
    ) +
    # Group means +/- SE
    see::geom_pointrange2(
      data      = get_centrality_wide(df, mean, category, {{ group_var }}),
      mapping   = ggplot2::aes(ymin = lower, ymax = upper),
      position  = ggplot2::position_dodge(width = 0.7),
      linewidth = 0.3,
      size      = 0.65,
      na.rm     = TRUE
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, 0.05), 
      expand = c(0, 0.01),
      # limits = c(0, 1)
    ) +
    ggplot2::labs(
      y        = "Mean accuracy",
      title    = title,
      subtitle = subtitle,
    ) +
    scale_discrete_phantasia() +
    theme_phantasia(
      legend.position = "bottom",
      legend.title    = ggplot2::element_blank(),
      axis.title.x    = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(
        face = "bold",
        size   = ggplot2::rel(1),
        margin = ggplot2::margin(t = 3.5, b = 3.5)
      ),
      panel.grid.major.x = ggplot2::element_blank()
    ) +
    ggplot2::theme(...)
  
  if (print_it) print(p)
  return(p)
}