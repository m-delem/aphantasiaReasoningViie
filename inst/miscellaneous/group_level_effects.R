devtools::load_all()
pacman::p_load(
  brms, dplyr, emmeans, ggplot2, ggpubr, marginaleffects,
  modelbased, parameters, performance, tidyr
  )

df_viie <- get_viie_data()

# var_x <- "group_2"
# var_x <- "group_3"
var_x <- "group_4"
# var_x <- "cluster"
# var_x <- "strategy_group"
var_y <- "viie_total"
# var_y <- "viie_spatial"
# var_y <- "viie_control"

# Continuous modelling -------------------------------
# p_cor <-
  ggpubr::ggscatter(
    data = df_viie,
    x = "vviq_total_score",
    y = var_y,
    color = var_x,
    size  = 0.5,
    mean.point      = TRUE,
    mean.point.size = 1.5,
    star.plot       = TRUE,
    star.plot.lwd   = .01,
    cor.coef = TRUE,
    cor.method = "spearman",
    cor.coef.coord = c(35, 15)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.2,
    color = "gray40") +
  ggplot2::geom_smooth(
    method    = "lm",
    formula   = y ~ x,
    color     = "black",
    linewidth = .3,
    alpha     = .2,
    fullrange = TRUE
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(10)
    # limits = c(-20, 10)
  ) +
  scale_discrete_manual(
    name = NULL,
    aesthetics = c("color", "fill"),
    values     = as.character(palette.colors())
  ) +
  labs(
    x = "VVIQ Total Score",
    # y = "VIIE Control (mean visual RT - mean control RT)",
    # y = "VIIE Spatial (mean visual RT - mean spatial RT)",
    y = "VIIE Average (mean visual RT - mean non-visual RT)"
  ) +
  theme_pdf()

save_ggplot(
  p,
  ncol = 2,
  height = 110,
  # path = "inst/miscellaneous/viie_control_vs_vviq.pdf",
  # path = "inst/miscellaneous/viie_spatial_vs_vviq.pdf",
  path = "inst/miscellaneous/viie_average_vs_vviq.pdf",
  show = TRUE
)

# Group comparisons ---------------------------------
p_group <-
  df_viie |>
  ggplot(
    aes(
      x = .data[[var_x]],
      y = .data[[var_y]],
      color = .data[[var_x]],
      fill  = .data[[var_x]]
    )
  ) +
  stat_summary(
    fun.data = "mean_cl_boot",
    size = 0.3,
    color = "black"
  ) +
  geom_point(
    size = 0.2,
    alpha = 0.3,
    position = ggplot2::position_jitter(
      width = 0.125,
      height = 0.25
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  see::geom_violinhalf(
    alpha = 0.3,
    position = ggplot2::position_nudge(x = 0.2)
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(10)
    # limits = c(-20, 10)
  ) +
  scale_discrete_manual(
    name = NULL,
    aesthetics = c("color", "fill"),
    values     = as.character(palette.colors())
  ) +
  labs(
    x = NULL,
    # y = "VIIE Control (mean visual RT - mean control RT)",
    # y = "VIIE Spatial (mean visual RT - mean spatial RT)",
    y = "VIIE Average (mean visual RT - mean non-visual RT)"
  ) +
  theme_pdf()

contrasts(df_viie$group_3) <-
  contr.treatment(n = levels(df_viie$group_3), base = 3)

m <-
  fit_brms_model(
    viie_total ~ group_3,
    data = df_viie,
    iterations = 40000,
    prior   = c(prior(normal(0, 10), class = "b")),
    file_refit = "always",
    file = here::here("inst/models/m_viie_total.rds"),
  )
