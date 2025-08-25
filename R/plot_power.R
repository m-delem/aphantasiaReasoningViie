plot_power <- function(power_results, threshold = 0.05) {
  p <-
    power_results |>
    dplyr::mutate(beta_vis = as.factor(beta_vis)) |>
    dplyr::group_by(n_subj_per_group, beta_vis) |>
    dplyr::reframe(power = sum(p_aph_vis <= threshold) / dplyr::n()) |>
    ggplot2::ggplot(ggplot2::aes(
      x = n_subj_per_group,
      y = power,
      color = beta_vis,
      group = beta_vis
    )) +
    ggplot2::geom_line(
      linewidth = 0.3,
      alpha = 0.3,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      size = 0.75,
      alpha = 0.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_smooth(
      linewidth = 0.35,
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      span = 0.4
    ) +
    ggplot2::geom_hline(
      linewidth = 0.35,
      yintercept = 0.8,
      linetype = "dashed",
      color = "grey"
    ) +
    ggplot2::geom_hline(
      linewidth = 0.35,
      yintercept = 0.9,
      linetype = "dashed",
      color = "red"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(0.025),
      breaks = scales::breaks_pretty(20)
    ) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(10)) +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::labs(
      x = "Number of subjects per group",
      y = "Power",
      color = "Effect size (in seconds) "
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1)) +
    theme_pdf(
      base_theme = ggplot2::theme_minimal,
      # Custom
      panel.grid.minor      = ggplot2::element_blank(),
      legend.text.position  = "top",
      legend.text           = ggplot2::element_text(
        size = 5,
        margin = ggplot2::margin(b = -1)
      ),
      legend.key.spacing.x  = grid::unit(1, "mm")
    )

  return(p)
}
