devtools::load_all()

df_survey  <- get_clean_data(verbose = FALSE)$df_survey
df_strats_long <- pivot_strategies_longer(df_survey)

m_strats_vviq <- fit_clm(score ~ group * strategy, df_strats_long)

cat("\014")
m_strats_vviq |> get_singularity()
m_strats_vviq |> get_performance() |> knitr::kable(align = "c")
m_strats_vviq |> report_contrast(~ group | strategy) |> knitr::kable()
m_strats_vviq |> report_contrast(~ strategy | group) |> knitr::kable()
m_strats_vviq |> report_contrast(~ group * strategy, interaction = TRUE) |>
  knitr::kable()
