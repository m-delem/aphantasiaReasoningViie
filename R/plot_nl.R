plot_nl <- function(df, title = NULL, ...) {
  p <-
    df |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = factor(.data$term),
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
