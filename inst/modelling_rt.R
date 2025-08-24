devtools::load_all()

# Run "cluster_and_create_data first

m_rt_vviq_2 <-
  glmmTMB::glmmTMB(
    data    = df_rt,
    formula = build_formula("rt_total", "group_2"),
    family  = Gamma(link = "identity"),
    prior   = set_ranef_prior(70)
  )
m_rt_vviq_3 <-
  glmmTMB::glmmTMB(
    data    = df_rt,
    formula = build_formula("rt_total", "group_3"),
    family  = Gamma(link = "identity"),
    prior   = set_ranef_prior(70)
  )
m_rt_osivq <-
  glmmTMB::glmmTMB(
    data    = df_rt,
    formula = build_formula("rt_total", "cluster"),
    family  = Gamma(link = "identity"),
    prior   = set_ranef_prior(70)
  )

cat("\014")
m_rt_vviq_2 |> get_singularity()
m_rt_vviq_2 |> get_performance() |> knitr::kable(align = "c")
m_rt_vviq_2 |> report_contrast(~ group_2) |> knitr::kable()
m_rt_vviq_2 |> report_contrast(~ category | group_2) |> knitr::kable()
m_rt_vviq_2 |> report_contrast(~ group_2 * category, interaction = TRUE) |>
  knitr::kable()

cat("\014")
m_rt_vviq_3 |> get_singularity()
m_rt_vviq_3 |> get_performance() |> knitr::kable(align = "c")
m_rt_vviq_3 |> report_contrast(~ group_3) |> knitr::kable()
m_rt_vviq_3 |> report_contrast(~ category | group_3) |> knitr::kable()
m_rt_vviq_3 |> report_contrast(~ group_3 * category, interaction = TRUE) |>
  knitr::kable()

cat("\014")
m_rt_osivq |> get_singularity()
m_rt_osivq |> get_performance() |> knitr::kable(align = "c")
m_rt_osivq |> report_contrast(~ cluster) |> knitr::kable()
m_rt_osivq |> report_contrast(~ category | cluster) |> knitr::kable()
m_rt_osivq |> report_contrast(~ cluster * category, interaction = TRUE) |>
  knitr::kable()
