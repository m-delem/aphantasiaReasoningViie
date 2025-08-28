# Run "cluster_and_create_data first

library(patchwork)

# Plotting proportions -----------------------
# pb1 <- plot_strategies_barplot(df_strats_long, group_2, title = "VVIQ 2 groups")
# pb2 <- plot_strategies_barplot(df_strats_long, group_3, title = "VVIQ 3 groups")
# pb3 <- plot_strategies_barplot(df_strats_long, cluster, title = "OSIVQ clusters")
#
# pb <-
#   pb1 + pb2 + pb3 +
#   patchwork::plot_layout(guides = "collect") &
#   ggplot2::theme(legend.position = "bottom")
#
# save_plot(
#   pb,
#   "inst/figures/strategy_proportions.pdf",
#   ncol = 2,
#   height = 140,
#   print_it = TRUE,
#   verbose = TRUE
# )

# Plotting scores -----------------------------
# n_aph_2 <- nrow(dplyr::filter(df_survey, group_2 == "Aphantasia"))
# n_typ_2 <- nrow(dplyr::filter(df_survey, group_2 == "Typical"))
# n_aph_3 <- nrow(dplyr::filter(df_survey, group_3 == "Aphantasia"))
# n_hypo  <- nrow(dplyr::filter(df_survey, group_3 == "Hypophantasia"))
# n_typ_3 <- nrow(dplyr::filter(df_survey, group_3 == "Typical"))
# n_visualisers  <- nrow(dplyr::filter(df_survey, cluster == "Visualiser"))
# n_spatialisers <- nrow(dplyr::filter(df_survey, cluster == "Spatialiser"))
# n_verbalisers  <- nrow(dplyr::filter(df_survey, cluster == "Verbaliser"))

size <- 3

ps1 <-
  plot_strategies_scores(
    df_strats_long,
    grouping = group_2,
    title = "VVIQ 2 groups",
    # x_labels = c(
    #   glue::glue("Aphantasia (N = {n_aph_2})"),
    #   glue::glue("Typical (N = {n_typ_2})")
    # )
  ) +
  add_significance(
    size_star = size,
    color = "#1b6096",
    tibble::tibble(
      x_star = 1.28,
      y_star = 4.67,
      stars  = "***",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = 4.6
    )
  )

ps2 <-
  plot_strategies_scores(
    df_strats_long,
    grouping = group_3,
    title = "VVIQ 3 groups",
    # x_labels = c(
    #   glue::glue("Aphantasia (N = {n_aph_3})"),
    #   glue::glue("Hypophantasia (N = {n_hypo})"),
    #   glue::glue("Typical (N = {n_typ_3})")
    # )
  ) +
  add_significance(
    size_star = size,
    color = "#1b6096",
    tibble::tibble(
      x_star = 1.28,
      y_star = 4.67,
      stars  = "**",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = 4.6
    )
  ) +
  add_significance(
    size_star = size,
    color = "#1b6096",
    tibble::tibble(
      x_star = 2.28,
      y_star = 4.79,
      stars  = "**",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = 4.72
    )
  ) +
  add_significance(
    size_star = size,
    color = "#1b6096",
    tibble::tibble(
      x_star = 1.78,
      y_star = 5.07,
      stars  = "***",
      x_line = .data$x_star - 1,
      x_line_end = .data$x_star + 1,
      y_line = 5
    )
  ) +
  add_significance(
    size_star = size,
    color = "#318f2c",
    linetype = "longdash",
    lw = 0.3,
    tibble::tibble(
      x_star = 2.38,
      y_star = 4.54,
      stars  = "*",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = 4.47
    )
  )

ps3 <-
  plot_strategies_scores(
    df_strats_long,
    grouping = cluster,
    title = "OSIVQ clusters",
    # x_labels = c(
    #   glue::glue("Visualiser (N = {n_visualisers})"),
    #   glue::glue("Spatialiser (N = {n_spatialisers})"),
    #   glue::glue("Verbaliser (N = {n_verbalisers})")
    # )
  ) +
  add_significance(
    size_star = size,
    color = "#1b6096",
    tibble::tibble(
      x_star = 2.28,
      y_star = 4.79,
      stars  = "*",
      x_line = .data$x_star - 0.5,
      x_line_end = .data$x_star + 0.5,
      y_line = 4.72
    )
  ) +
  add_significance(
    size_star = size,
    color = "#1b6096",
    tibble::tibble(
      x_star = 1.78,
      y_star = 5.07,
      stars  = "***",
      x_line = .data$x_star - 1,
      x_line_end = .data$x_star + 1,
      y_line = 5
    )
  )

ps <-
  ps1 + ps2 + ps3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

save_plot(
  ps,
  "inst/figures/strategy_scores.pdf",
  ncol = 2,
  height = 75,
  print_it = TRUE,
  verbose = TRUE
)
