devtools::load_all()
pacman::p_load(dplyr, emmeans, ggplot2, modelbased, performance, tidyr)

df_expe   <- get_clustered_data()$df_expe
df_survey <- get_clustered_data()$df_survey

df_rt <-
  filter_trials_on_rt(df_expe, verbose = TRUE) |>
  select(id, group:strategy_group, problem, category, rt_total)

m_rt <-
  glmmTMB::glmmTMB(
    data    = df_rt,
    formula = rt_total ~ category + (category | id) + (category | cluster),
    family  = Gamma(link = "identity")
    # prior   = set_ranef_prior(70)
  )

df_viie_model <-
  estimate_grouplevel(m_rt, type = "total") |>
  reshape_grouplevel(group = "id") |>
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
  reframe(rt = mean(rt_total)) |>
  pivot_wider(
    names_from = category,
    values_from = rt
  ) |>
  mutate(
    category_spatial = Spatial - Visual,
    category_control = Control - Visual
  )

df_viie_model |>
  ggplot(
    aes(
      x = group_3,
      y = Coefficient,
      color = group_3,
      fill = group_3
    )
  ) +
  geom_point(
    alpha = 0.3,
    position = ggplot2::position_jitter(
      width = 0.125,
      height = 0.25
    )
  ) +
  # geom_smooth(
  #   method = "lm",
  #   color = "black",
  #   fill  = "gray80",
  #   alpha = 0.5
  # ) +
  geom_violin(
    alpha = 0.2
    # position = ggplot2::position_nudge(x = 0.2)
  ) +
  stat_summary(
    fun.data = "mean_cl_boot",
    size = 0.5,
    color = "black"
  ) +
  # see::geom_violinhalf(
  #   alpha = 0.5,
  #   position = ggplot2::position_nudge(x = 0.2)
  # ) +
  # geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(
    vars(Parameter),
    scales = "free"
  ) +
  scale_discrete_manual(
    aesthetics = c("color", "fill"),
    values     = as.character(palette.colors())
  ) +
  theme_pdf()

df_viie_manual |>
  ggplot(
    aes(
      x = group_3,
      y = category_spatial
    )
  ) +
  stat_summary(
    fun.data = "mean_cl_boot",
    size = 0.5,
    color = "black"
  ) +
  see::geom_violinhalf(
    alpha = 0.5,
    position = ggplot2::position_nudge(x = 0.2)
  )

ggplot(
  data = data.frame(
    y = df_viie_model$category_control,
    x = df_viie_manual$category_control
  ),
  aes(
    x = x,
    y = y
  )
  ) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  theme_pdf()
