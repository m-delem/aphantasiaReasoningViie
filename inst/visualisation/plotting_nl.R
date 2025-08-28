# Run "cluster_and_create_data first

library(patchwork)

m_nl_vviq_2 <-
  mgcv::bam(
    formula = rt ~
      group_2_category +
      s(term, by = group_2_category, bs = "tp", k = 4),
    family  = Gamma(link = "identity"),
    data    = df_rt_long,
    method  = "fREML"
  )
m_nl_vviq_3 <-
  mgcv::bam(
    formula = rt ~
      group_3_category +
      s(term, by = group_3_category, bs = "tp", k = 4),
    family  = Gamma(link = "identity"),
    data    = df_rt_long,
    method  = "fREML"
  )
m_nl_osivq <-
  mgcv::bam(
    formula = rt ~
      cluster_category +
      s(term, by = cluster_category, bs = "tp", k = 4),
    family  = Gamma(link = "identity"),
    data    = df_rt_long,
    method  = "fREML"
  )

df_nl_vviq_2 <-
  modelbased::estimate_relation(
    m_nl_vviq_2,
    by = c("group_2_category", "term")
  ) |>
  as.data.frame() |>
  dplyr::select(group_2_category, term, Predicted, CI_low, CI_high) |>
  tidyr::separate_wider_delim(
    group_2_category,
    delim = ".",
    names = c("group", "category")
  ) |>
  dplyr::mutate(CI_low = CI_low - 0.4, CI_high = CI_high + 0.4)
df_nl_vviq_3 <-
  modelbased::estimate_relation(
    m_nl_vviq_3,
    by = c("group_3_category", "term")
  ) |>
  as.data.frame() |>
  dplyr::select(group_3_category, term, Predicted, CI_low, CI_high) |>
  tidyr::separate_wider_delim(
    group_3_category,
    delim = ".",
    names = c("group", "category")
  ) |>
  dplyr::mutate(CI_low = CI_low - 0.4, CI_high = CI_high + 0.4)
df_nl_osivq <-
  modelbased::estimate_relation(
    m_nl_osivq,
    by = c("cluster_category", "term")
  ) |>
  as.data.frame() |>
  dplyr::select(cluster_category, term, Predicted, CI_low, CI_high) |>
  tidyr::separate_wider_delim(
    cluster_category,
    delim = ".",
    names = c("group", "category")
  ) |>
  dplyr::mutate(CI_low = CI_low - 0.4, CI_high = CI_high + 0.4)

pnl1 <-
  plot_nl(df_nl_vviq_2, title = "VVIQ 2 groups") +

  # 2nd premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Aphantasia")),
      x_star = 1.93,
      y_star = 5.97,
      stars  = "°",
      x_line = .data$x_star - 0.06,
      x_line_end = .data$x_star + 0.06,
      y_line = 5.9
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Typical")),
      x_star = 1.93,
      y_star = 6.24,
      stars  = "**",
      x_line = .data$x_star - 0.06,
      x_line_end = .data$x_star + 0.06,
      y_line = 6.17
    )
  ) +

  # 3rd premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Aphantasia")),
      x_star = 3.06,
      y_star = 7.87,
      stars  = "***",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 7.8
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Aphantasia")),
      x_star = 3.01,
      y_star = 8.27,
      stars  = "°",
      x_line = .data$x_star - 0.1,
      x_line_end = .data$x_star + 0.1,
      y_line = 8.2
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Typical")),
      x_star = 3.06,
      y_star = 8.47,
      stars  = "***",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 8.4
    )
  )  +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Typical")),
      x_star = 2.93,
      y_star = 7.77,
      stars  = "*",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 7.7
    )
  )

pnl2 <-
  plot_nl(
    df_nl_vviq_3,
    title = "VVIQ 3 groups",
    plot.margin = ggplot2::margin(t = 10)
  ) +

  # 2nd premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Aphantasia")),
      x_star = 1.93,
      y_star = 6.63,
      stars  = "*",
      x_line = .data$x_star - 0.06,
      x_line_end = .data$x_star + 0.06,
      y_line = 6.57
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Typical")),
      x_star = 1.93,
      y_star = 6.24,
      stars  = "**",
      x_line = .data$x_star - 0.06,
      x_line_end = .data$x_star + 0.06,
      y_line = 6.17
    )
  )  +

  # 3rd premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Aphantasia")),
      x_star = 3.06,
      y_star = 8.32,
      stars  = "*",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 8.25
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Hypophantasia")),
      x_star = 3.06,
      y_star = 7.62,
      stars  = "*",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 7.55
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Typical")),
      x_star = 3.06,
      y_star = 8.47,
      stars  = "***",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 8.4
    )
  )  +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Typical")),
      x_star = 2.93,
      y_star = 7.77,
      stars  = "*",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 7.7
    )
  )

pnl3 <-
  plot_nl(
    df_nl_osivq,
    title = "OSIVQ clusters",
    plot.margin = ggplot2::margin(t = 10)
  ) +

  # 1st premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Visualiser")),
      x_star = 1,
      y_star = 8.47,
      stars  = "°",
      x_line = .data$x_star - 0.1,
      x_line_end = .data$x_star + 0.1,
      y_line = 8.4
    )
  ) +

  # 2nd premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Visualiser")),
      x_star = 1.93,
      y_star = 6.37,
      stars  = "**",
      x_line = .data$x_star - 0.06,
      x_line_end = .data$x_star + 0.06,
      y_line = 6.3
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Verbaliser")),
      x_star = 1.93,
      y_star = 6.17,
      stars  = "°",
      x_line = .data$x_star - 0.06,
      x_line_end = .data$x_star + 0.06,
      y_line = 6.1
    )
  ) +

  # 3rd premise
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Visualiser")),
      x_star = 3.06,
      y_star = 8.62,
      stars  = "***",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 8.55
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Visualiser")),
      x_star = 2.93,
      y_star = 7.77,
      stars  = "*",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 7.7
    )
  ) +
  add_significance(
    size_star = 3,
    tibble::tibble(
      group  = factor(c("Verbaliser")),
      x_star = 3.06,
      y_star = 8.04,
      stars  = "*",
      x_line = .data$x_star - 0.05,
      x_line_end = .data$x_star + 0.05,
      y_line = 7.97
    )
  )

pnl <- pnl1 / pnl2 / pnl3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

save_plot(
  pnl,
  "inst/figures/nl_plots.pdf",
  ncol = 2,
  height = 180,
  print_it = TRUE,
  verbose = TRUE
)
