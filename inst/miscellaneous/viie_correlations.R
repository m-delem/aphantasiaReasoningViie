devtools::load_all()
pacman::p_load(dplyr, ggplot2, patchwork)

df_viie <- get_viie_data()

p_mat <-
  correlation::correlation(
    df_viie |> select(2:5, contains("_rt"), viie_total:viie_control),
    method = "spearman",
    p_adjust = "fdr",
  ) |>
  summary() |>
  correlation::visualisation_recipe(
    show_data  = "tiles",
    tile       = list(colour = "black", linewidth = 0.05),
    text       = list(size = 6, size.unit = "pt"),
    scale_fill = list(
      high = viridis::viridis(100)[55],
      low  = viridis::viridis(100)[5],
      name = "r"
    )
  ) |>
  plot() +
  scale_x_discrete(position = "top") +
  labs(title = NULL) +
  theme_pdf(
    base_theme = theme_minimal,
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_text(color = "black", size = 7),
    axis.text.x.top = element_text(angle = 45, hjust = 0),
    plot.margin = margin(5, 10, 5, 5)
  )

p_cor_osv_s <-
  ggpubr::ggscatter(
    data = df_viie,
    x = "osivq_spatial",
    y = "viie_total",
    color = "cluster",
    size  = 0.5,
    mean.point = TRUE,
    mean.point.size = 1.5,
    cor.method = "spearman",
    cor.coef = TRUE,
    cor.coef.size = 3,
    cor.coeff.args = list(family = "Montserrat"),
    add = "reg.line",
    add.params = list(
      color = "black",
      fill = "lightgray",
      size = 0.3
    ),
    conf.int = TRUE
  ) +
  scale_discrete_manual(
    name = NULL,
    aesthetics = "color",
    values = c(
      "Visualiser"  = palette.colors()[4],
      "Verbaliser"  = palette.colors()[1],
      "Spatialiser" = palette.colors()[2]
    )
  ) +
  labs(
    x = "OSIVQ spatial score",
    y = "VIIE (mean visual RT - mean non-visual RT)"
  ) +
  theme_pdf(
    base_theme = theme_minimal,
    legend.position = "bottom",
    legend.text = element_text(size = 6),
    # axis.title.y = element_blank()
  )

p_cor_vviq <-
  ggpubr::ggscatter(
    data = df_viie,
    x = "vviq_total_score",
    y = "viie_total",
    color = "group_4",
    size  = 0.5,
    mean.point = TRUE,
    mean.point.size = 1.5,
    cor.method = "spearman",
    cor.coef = TRUE,
    cor.coef.size = 3,
    cor.coeff.args = list(family = "Montserrat"),
    add = "reg.line",
    add.params = list(
      color = "black",
      fill = "lightgray",
      size = 0.3
    ),
    conf.int = TRUE
  ) +
  scale_discrete_manual(
    name = NULL,
    aesthetics = "color",
    values = as.character(palette.colors())
  ) +
  labs(
    x = "VVIQ score",
    y = "VIIE (mean visual RT - mean non-visual RT)"
  ) +
  theme_pdf(
    base_theme = theme_minimal,
    legend.position = "bottom",
    legend.text = element_text(size = 6)
  )

save_ggplot(
  ggpubr::ggarrange(
    p_mat,
    (p_cor_osv_s | p_cor_vviq),
    ncol = 1,
    heights = c(1.5, 1)
  ),
  ncol = 2,
  height = 200,
  path = "inst/miscellaneous/viie_correlations.pdf",
  show = TRUE
)
