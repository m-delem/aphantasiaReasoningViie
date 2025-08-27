# Run "cluster_and_create_data first

pa <- plot_superb_categories(
  df_expe, accuracy, group_2,
  title = "Accuracy per problem type and group", y_title = "Mean accuracy"
)

save_plot(
  pa,
  "inst/figures/accuracy_poster_plot.pdf",
  ncol = 1,
  height = 80,
  print_it = TRUE,
  verbose = TRUE
)

pr <- plot_superb_categories(
  df_rt, rt_total, group_2,
  title = "Response time per group and problem type",
  y_title = "Mean total RT (s)"
)

save_plot(
  pr,
  "inst/figures/rt_poster_plot.pdf",
  ncol = 1,
  height = 80,
  print_it = TRUE,
  verbose = TRUE
)

ps <-
  plot_strategies_scores(
    df_strats_long,
    grouping = group_2,
    title = "VVIQ 2 groups",
    axis_relative_size = 1,
    axis_relative_x = 1.2,
    legend_relative = 1.2,
  ) +
  ggplot2::theme(
    panel.border = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_line(color = "grey70"),
    panel.grid.major.y = ggplot2::element_line(color = "grey70"),
  )
