devtools::load_all()

df_expe    <- get_clean_data(n_groups = 3)$df_expe
df_rt      <- filter_trials_on_rt(df_expe)
df_rt_long <- pivot_terms_longer(df_rt)

# Accuracy ----------------------------
m_acc_vviq <-
  glmmTMB::glmmTMB(
    data    = df_expe,
    formula = build_formula("accuracy", "group"),
    family  = binomial(link = "logit"),
    prior   = set_ranef_prior(70)
  )

cat("\014")
m_acc_vviq |> get_singularity()
m_acc_vviq |> get_performance() |> knitr::kable(align = "c")
m_acc_vviq |> report_contrast(~ group) |> knitr::kable()
m_acc_vviq |> report_contrast(~ category | group) |> knitr::kable()
m_acc_vviq |> report_contrast(~ category * group, interaction = TRUE) |>
  knitr::kable()

# RT ---------------------------------
m_rt_vviq <-
  glmmTMB::glmmTMB(
    data    = df_rt,
    formula = build_formula("rt_total", "group"),
    family  = Gamma(link = "identity"),
    prior   = set_ranef_prior(70)
  )

cat("\014")
m_rt_vviq |> get_singularity()
m_rt_vviq |> get_performance() |> knitr::kable(align = "c")
m_rt_vviq |> report_contrast(~ group) |> knitr::kable()
m_rt_vviq |> report_contrast(~ category | group) |> knitr::kable()
m_rt_vviq |> report_contrast(~ group * category, interaction = TRUE) |>
  knitr::kable()

m_nl_vviq <-
  mgcv::bam(
    formula = rt ~
      group_category +
      s(term, by = group_category,    bs = "tp", k = 4) +
      s(term, problem, by = group,    bs = "fs", m = 1, k = 4) +
      s(term, id,      by = category, bs = "fs", m = 1, k = 4),
    family  = Gamma(link = "identity"),
    data    = df_rt_long,
    method  = "fREML"
  )

cat("\014")
m_nl_vviq |> get_singularity()
m_nl_vviq |>
  get_contrast(
    ~ group_category | term,
    at = list(term = c(1, 2, 3, 4)),
    interaction = FALSE,
    adjust = "none"
  ) |>
  as.data.frame() |>
  tidyr::separate_wider_delim(
    contrast, " - ", names = c("group_cat_1", "group_cat_2")
  ) |>
  tidyr::separate_wider_delim(
    group_cat_1, ".", names = c("group_1", "category_1")
  ) |>
  tidyr::separate_wider_delim(
    group_cat_2, ".", names = c("group_2", "category_2")
  ) |>
  dplyr::filter(group_1 == group_2 & p.value < 0.05) |>
  dplyr::select(!c(group_2, SE, df, t.ratio)) |>
  dplyr::mutate(across(c(estimate, p.value), ~round(., 3))) |>
  tidyr::unite(`Category contrast`, category_1, category_2, sep = " - ") |>
  dplyr::rename(group = group_1, `RT difference` = estimate) |>
  dplyr::arrange(term, group) |>
  dplyr::mutate(
    term = dplyr::case_match(
      term,
      "1" ~ "Premise 1",
      "2" ~ "Premise 2",
      "3" ~ "Premise 3",
      "4" ~ "Conclusion"
    ),
  ) |>
  knitr::kable(digits = 3)

# Strategies ---------------------------------------
df_survey  <- get_clean_data(n_groups = 2)$df_survey
df_strats_long <- pivot_strategies_longer(df_survey)

m_strats_vviq <- fit_clm(score ~ group * strategy, df_strats_long)

cat("\014")
m_strats_vviq |> get_singularity()
m_strats_vviq |> get_performance() |> knitr::kable(align = "c")
m_strats_vviq |> report_contrast(~ group | strategy) |> knitr::kable()
m_strats_vviq |> report_contrast(~ strategy | group) |> knitr::kable()
m_strats_vviq |> report_contrast(~ group * strategy, interaction = TRUE) |>
  knitr::kable()
