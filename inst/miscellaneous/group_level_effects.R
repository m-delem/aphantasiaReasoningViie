devtools::load_all()
pacman::p_load(dplyr, emmeans, ggplot2, marginaleffects, modelbased, parameters, performance, tidyr)

df_expe   <- get_clustered_data("experiment")
df_survey <- get_clustered_data("survey")

df_rt <-
  filter_trials_on_rt(df_expe, verbose = TRUE) |>
  select(id, group:strategy_group, problem, category, rt_total)

m_rt <-
  glmmTMB::glmmTMB(
    data    = df_rt,
    formula = rt_total ~ category + (category | id),
    family  = Gamma(link = "identity")
  )

df_viie_model <-
  estimate_grouplevel(m_rt, type = "total") |>
  reshape_grouplevel() |>
  # select(Level:Coefficient) |>
  # tidyr::pivot_wider(
  #   names_from  = Parameter,
  #   values_from = Coefficient
  # ) |>
  # mutate(id = as.factor(.data$Level), .keep = "unused") |>
  left_join(
    df_survey |>
      select(id, group_2, group_3, cluster, strategy_group, vviq_total_score),
    by = join_by("id")
  )

df_viie_manual <-
  df_rt |>
  group_by(id, category, group_3, group_2, cluster, strategy_group) |>
  # reframe(rt = mean(rt_total)) |>
  reframe(rt = mean(rt_total)) |>
  pivot_wider(
    names_from = category,
    values_from = rt
  ) |>
  mutate(
    category_spatial = Spatial - Visual,
    category_control = Control - Visual
  )

# Correlation plot between model-estimated and manually-computed effects
ggplot(
  data = data.frame(
    x = df_viie_manual$category_control,
    y = df_viie_model$category_control
  ),
  aes(x = x, y = y)
  ) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  theme_pdf()

var_x <- "group_3"
var_y <- "category_control"
var_y <- "category_spatial"

df_viie_manual |>
# df_viie_model |>
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
    size = 0.5,
    color = "black"
  ) +
  geom_point(
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
    aesthetics = c("color", "fill"),
    values     = as.character(palette.colors())
  ) +
  theme_pdf()

