# Run "cluster_and_create_data first

m_acc_vviq_2 <-
  glmmTMB::glmmTMB(
    data    = df_expe,
    formula = build_formula("accuracy", "group_2"),
    family  = binomial(link = "logit"),
    prior   = set_ranef_prior(65)
  )
m_acc_vviq_3 <-
  glmmTMB::glmmTMB(
    data    = df_expe,
    formula = build_formula("accuracy", "group_3"),
    family  = binomial(link = "logit"),
    prior   = set_ranef_prior(30)
  )
m_acc_osivq <-
  glmmTMB::glmmTMB(
    data    = df_expe,
    formula = build_formula("accuracy", "cluster"),
    family  = binomial(link = "logit"),
    prior   = set_ranef_prior(20)
  )

cat("\014")
# Singularity
m_acc_vviq_2 |> get_singularity()
# Performance
m_acc_vviq_2 |> get_performance() |> knitr::kable(align = "c")
# Group contrast
m_acc_vviq_2 |> report_contrast(~ group_2) |> knitr::kable()
# Category contrast within groups
m_acc_vviq_2 |> report_contrast(~ category | group_2) |> knitr::kable()
# Interaction contrast
m_acc_vviq_2 |> report_contrast(~ category * group_2, interaction = TRUE) |>
  knitr::kable()

cat("\014")
m_acc_vviq_3 |> get_singularity()
m_acc_vviq_3 |> get_performance() |> knitr::kable(align = "c")
m_acc_vviq_3 |> report_contrast(~ group_3) |> knitr::kable()
m_acc_vviq_3 |> report_contrast(~ category | group_3) |> knitr::kable()
m_acc_vviq_3 |> report_contrast(~ category * group_3, interaction = TRUE) |>
  knitr::kable()

cat("\014")
m_acc_osivq  |> get_singularity()
m_acc_osivq  |> get_performance() |> knitr::kable(align = "c")
m_acc_osivq  |> report_contrast(~ cluster) |> knitr::kable()
m_acc_osivq  |> report_contrast(~ category | cluster) |> knitr::kable()
m_acc_osivq  |> report_contrast(~ category * cluster, interaction = TRUE) |>
  knitr::kable()
