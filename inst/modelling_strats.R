devtools::load_all()

# Run "cluster_and_create_data first

df_strats_long <- pivot_strategies_longer(df_survey)

m_strats_vviq_2 <- fit_clm(score ~ group_2 * strategy, df_strats_long)
m_strats_vviq_3 <- fit_clm(score ~ group_3 * strategy, df_strats_long)
m_strats_osivq  <- fit_clm(score ~ cluster * strategy, df_strats_long)

cat("\014")
m_strats_vviq_2 |> get_singularity()
m_strats_vviq_2 |> get_performance() |> knitr::kable(align = "c")
m_strats_vviq_2 |> report_contrast(~ group_2 | strategy) |> knitr::kable()
m_strats_vviq_2 |> report_contrast(~ strategy | group_2) |> knitr::kable()
m_strats_vviq_2 |> report_contrast(~ group_2 * strategy, interaction = TRUE) |>
  knitr::kable()

cat("\014")
m_strats_vviq_3 |> get_singularity()
m_strats_vviq_3 |> get_performance() |> knitr::kable(align = "c")
m_strats_vviq_3 |> report_contrast(~ group_3 | strategy) |> knitr::kable()
m_strats_vviq_3 |> report_contrast(~ strategy | group_3) |> knitr::kable()
m_strats_vviq_3 |> report_contrast(~ group_3 * strategy, interaction = TRUE) |>
  knitr::kable()

cat("\014")
m_strats_osivq |> get_singularity()
m_strats_osivq |> get_performance() |> knitr::kable(align = "c")
m_strats_osivq |> report_contrast(~ cluster | strategy) |> knitr::kable()
m_strats_osivq |> report_contrast(~ strategy | cluster) |> knitr::kable()
m_strats_osivq |> report_contrast(~ cluster * strategy, interaction = TRUE) |>
  knitr::kable()
