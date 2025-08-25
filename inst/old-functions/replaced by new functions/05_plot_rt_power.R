plot_rt_power <- function(
    simulation_results, 
    plot_size = "pdf", # or "pdf"
    threshold = 0.05,
    print_it  = FALSE
  ) {
  if (plot_size == "standard") {
    lw_smol  <- 0.5
    lw_mid   <- 1
    dot_size <- 1.1
    txt_big  <- 12
    txt_mid  <- 10
    txt_smol <- 9
  } else if (plot_size == "pdf") {
    lw_smol  <- 0.3
    lw_mid   <- 0.35
    dot_size <- 0.15
    txt_big  <- 7
    txt_mid  <- 6
    txt_smol <- 5
  } else stop("plot_size must be 'standard' or 'pdf'")
  
  p <-
    simulation_results |> 
    dplyr::mutate(beta_vis = as.factor(beta_vis)) |>
    dplyr::group_by(n_subj_per_group, beta_vis) |>
    dplyr::reframe(power = sum(p_aph_vis < threshold) / dplyr::n()) |> 
    ggplot2::ggplot(ggplot2::aes(
      x = n_subj_per_group,
      y = power,
      color = beta_vis, 
      group = beta_vis
    )
    ) +
    ggplot2::geom_line(
      linewidth = lw_smol,
      alpha = 0.3,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      size = dot_size, 
      alpha = 0.2,
      show.legend = FALSE
      ) +
    ggplot2::geom_smooth(
      linewidth = lw_mid,
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      span = 0.75
    ) +
    ggplot2::geom_hline(
      linewidth = lw_mid,
      yintercept = 0.8, 
      linetype = "dashed", 
      color = "grey"
    ) +
    ggplot2::geom_hline(
      linewidth = lw_mid,
      yintercept = 0.9, 
      linetype = "dashed", 
      color = "red"
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(20)) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(10)) +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::labs(
      x = "Number of subjects per group",
      y = "Power",
      color = latex2exp::TeX("Interaction\neffect size ($\\beta_{vis}$)")
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text                  = ggplot2::element_text(size = txt_big),
      panel.grid.major      = ggplot2::element_line(linewidth = lw_smol),
      panel.grid.minor      = ggplot2::element_blank(),
      
      legend.position       = "top",
      legend.title.position = "top",
      legend.title          = ggplot2::element_text(
        margin = ggplot2::margin(t = -5, b = 5)
      ),
      legend.text.position  = "top",
      legend.text           = ggplot2::element_text(
        size = txt_smol, 
        margin = ggplot2::margin(b = -5)
      ),
      legend.key.spacing.x  = grid::unit(0, "mm"),
      legend.margin         = ggplot2::margin(10, 0, 0, 0),
      legend.box.margin     = ggplot2::margin(b = -10),
      
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 7)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 4)),
      axis.text    = ggplot2::element_text(size = txt_smol),
    )
  
  if (print_it) print(p)
  return(p)
}

