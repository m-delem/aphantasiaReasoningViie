# Run "cluster_and_create_data first

library(patchwork)
library(superb)

# Accuracy ---------------
pa1 <-
  plot_superb_jitter(
    df_expe, accuracy, group_2,
    title = "VVIQ 2 groups", y_title = "Mean accuracy"
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 2,
      y_star = 1.07,
      stars  = "*",
      x_line = .data$x_star - 0.16,
      x_line_end = .data$x_star + 0.16,
      y_line = 1.05
    )
  )
pa2 <-
  plot_superb_jitter(
    df_expe, accuracy, group_3,
    title = "VVIQ 3 groups", y_title = "Mean accuracy"
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3,
      y_star = 1.07,
      stars  = "*",
      x_line = .data$x_star - 0.16,
      x_line_end = .data$x_star + 0.16,
      y_line = 1.05
    )
  )
pa3 <-
  plot_superb_jitter(
    df_expe, accuracy, cluster,
    title = "OSIVQ clusters", y_title = "Mean accuracy"
  ) +
  add_significance(
    size_star = 4,
    tibble::tibble(
      x_star = 3,
      y_star = 1.065,
      stars  = "°",
      x_line = .data$x_star - 0.16,
      x_line_end = .data$x_star + 0.16,
      y_line = 1.05
    )
  )

pa <- pa1 + pa2 + pa3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

save_plot(
  pa,
  "inst/figures/accuracy_plots.pdf",
  ncol = 2,
  height = 75,
  print_it = TRUE,
  verbose = TRUE
)
