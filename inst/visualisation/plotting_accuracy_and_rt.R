# Run "cluster_and_create_data first

library(patchwork)

# Accuracy ---------------
pa1 <- plot_superb_jitter(
  df_expe, accuracy, group_2,
  title = "VVIQ 2 groups", y_title = "Mean accuracy"
  )
pa2 <- plot_superb_jitter(
  df_expe, accuracy, group_3,
  title = "VVIQ 3 groups", y_title = "Mean accuracy"
)
pa3 <- plot_superb_jitter(
  df_expe, accuracy, cluster,
  title = "OSIVQ clusters", y_title = "Mean accuracy"
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

# RT ------------------------
pr1 <- plot_superb_raincloud(
  df_rt, rt_total, group_2,
  title = "VVIQ 2 groups", y_title = "Mean total RT (s)"
)
pr2 <- plot_superb_raincloud(
  df_rt, rt_total, group_3,
  title = "VVIQ 3 groups", y_title = "Mean total RT (s)"
)
pr3 <- plot_superb_raincloud(
  df_rt, rt_total, cluster,
  title = "OSIVQ clusters", y_title = "Mean total RT (s)"
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
