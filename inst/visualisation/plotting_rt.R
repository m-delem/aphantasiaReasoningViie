# Run "cluster_and_create_data first

library(patchwork)
library(superb)

# RT ------------------------
pr1 <-
  plot_superb_raincloud(
    df_rt, rt_total, group_2,
    title = "VVIQ 2 groups", y_title = "Mean total RT (s)"
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 2,
      y_star = 30,
      stars  = "**",
      x_line = .data$x_star - 0.14,
      x_line_end = .data$x_star + 0.14,
      y_line = 29.5
    )
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 2.07,
      y_star = 28,
      stars  = "*",
      x_line = .data$x_star - 0.07,
      x_line_end = .data$x_star + 0.07,
      y_line = 27.5
    )
  )
pr2 <-
  plot_superb_raincloud(
    df_rt, rt_total, group_3,
    title = "VVIQ 3 groups", y_title = "Mean total RT (s)"
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3,
      y_star = 30,
      stars  = "**",
      x_line = .data$x_star - 0.14,
      x_line_end = .data$x_star + 0.14,
      y_line = 29.5
    )
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3.07,
      y_star = 28,
      stars  = "*",
      x_line = .data$x_star - 0.07,
      x_line_end = .data$x_star + 0.07,
      y_line = 27.5
    )
  )
pr3 <-
  plot_superb_raincloud(
    df_rt, rt_total, cluster,
    title = "OSIVQ clusters", y_title = "Mean total RT (s)"
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3,
      y_star = 30,
      stars  = "***",
      x_line = .data$x_star - 0.14,
      x_line_end = .data$x_star + 0.14,
      y_line = 29.5
    )
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3.07,
      y_star = 28,
      stars  = "*",
      x_line = .data$x_star - 0.07,
      x_line_end = .data$x_star + 0.07,
      y_line = 27.5
    )
  )

pr <- pr1 + pr2 + pr3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

save_plot(
  pr,
  "inst/figures/rt_plots.pdf",
  ncol = 2,
  height = 90,
  print_it = TRUE,
  verbose = TRUE
)
