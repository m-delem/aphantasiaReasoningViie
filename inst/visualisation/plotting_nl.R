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
  )
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
  )
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
  )

pnl1 <- plot_nl(df_nl_vviq_2, title = "VVIQ 2 groups")
pnl2 <- plot_nl(
  df_nl_vviq_3,
  title = "VVIQ 3 groups",
  plot.margin = ggplot2::margin(t = 10)
)
pnl3 <- plot_nl(
  df_nl_osivq,
  title = "OSIVQ clusters",
  plot.margin = ggplot2::margin(t = 10)
)

pnl <- pnl1 / pnl2 / pnl3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

save_plot(
  pnl,
  "inst/figures/nl_plots.pdf",
  ncol = 2,
  height = 150,
  print_it = TRUE,
  verbose = TRUE
)
