devtools::load_all()

# Creating OSIVQ clusters ------------------------
df_survey  <- get_clean_data()$df_survey
# Clustering OSIVQ data
clustering <- cluster_osivq(df_survey)
# Checking cluster properties to define names for each cluster
df_survey |>
  add_named_clusters(clustering) |>
  summarise_clustering()
# Adding named clusters to the survey data
df_survey <- add_named_clusters(
  df_survey, clustering,
  names  = c("Verbaliser", "Visualiser", "Spatialiser"),
  levels = c("Visualiser", "Spatialiser", "Verbaliser"),
  contrasts = c("_visualiser", "_spatialiser", "_verbaliser"),
  base = 1
)
# Checks
contrasts(df_survey$cluster)
summarise_clustering(df_survey)

# Modelling the VIIE with OSIVQ clusters -------
df_expe <-
  dplyr::left_join(
    get_clean_data(n_groups = 3)$df_expe,
    df_survey |> dplyr::select(id, cluster),
    by = dplyr::join_by("id")
  ) |>
  dplyr::relocate(cluster, .after = "group")

df_rt      <- filter_trials_on_rt(df_expe)
df_rt_long <-
  pivot_terms_longer(df_rt) |>
  dplyr::mutate(cluster_category = interaction(cluster, category))

m_acc_osivq <-
  glmmTMB::glmmTMB(
    data    = df_expe,
    formula = build_formula("accuracy", "cluster"),
    family  = binomial(link = "logit"),
    prior   = set_ranef_prior()
  )

cat("\014")
m_acc_osivq |> get_singularity()
m_acc_osivq |> get_performance() |> knitr::kable(align = "c")
m_acc_osivq |> report_contrast(~ cluster) |> knitr::kable()
m_acc_osivq |> report_contrast(~ category | cluster) |> knitr::kable()
m_acc_osivq |> report_contrast(~ category * cluster, interaction = TRUE) |>
  knitr::kable()

m_rt_osivq <-
  glmmTMB::glmmTMB(
    data    = df_rt,
    formula = build_formula("rt_total", "cluster"),
    family  = Gamma(link = "identity"),
    prior   = set_ranef_prior()
  )

cat("\014")
m_rt_osivq |> get_singularity()
m_rt_osivq |> get_performance() |> knitr::kable(align = "c")
m_rt_osivq |> report_contrast(~ cluster) |> knitr::kable()
m_rt_osivq |> report_contrast(~ category | cluster) |> knitr::kable()
m_rt_osivq |> report_contrast(~ cluster * category, interaction = TRUE) |>
  knitr::kable()

m_nl_osivq <-
  mgcv::bam(
    formula = rt ~
      cluster_category +
      s(term, by = cluster_category,    bs = "tp", k = 4) +
      s(term, problem, by = cluster,    bs = "fs", m = 1, k = 4) +
      s(term, id,      by = category, bs = "fs", m = 1, k = 4),
    family  = Gamma(link = "identity"),
    data    = df_rt_long,
    method  = "fREML"
  )

cat("\014")
m_nl_osivq |> get_singularity()
m_nlm_nl_osivq_vviq |>
  get_contrast(
    ~ cluster_category | term,
    at = list(term = c(1, 2, 3, 4)),
    interaction = TRUE
  ) |>
  as.data.frame() |>
  tidyr::separate_wider_delim(
    cluster_category_pairwise, " - ", names = c("cluster_cat_1", "cluster_cat_2")
  ) |>
  tidyr::separate_wider_delim(
    cluster_cat_1, ".", names = c("cluster_1", "category_1")
  ) |>
  tidyr::separate_wider_delim(
    cluster_cat_2, ".", names = c("cluster_2", "category_2")
  ) |>
  dplyr::filter(cluster_1 == cluster_2 & p.value < 0.05) |>
  dplyr::select(!c(cluster_2, SE, df, t.ratio)) |>
  dplyr::mutate(across(c(estimate, p.value), ~round(., 3))) |>
  tidyr::unite(`Category contrast`, category_1, category_2, sep = " - ") |>
  dplyr::rename(cluster = cluster_1, `RT difference` = estimate) |>
  dplyr::arrange(term, cluster) |>
  dplyr::mutate(
    term = dplyr::case_match(
      term,
      1 ~ "Premise 1",
      2 ~ "Premise 2",
      3 ~ "Premise 3",
      4 ~ "Conclusion"
    ),
  ) |>
  knitr::kable(digits = 3)

# Strategies ------------------------------------
df_strats_long <- pivot_strategies_longer(df_survey)

m_strats_osivq <- fit_clm(score ~ cluster * strategy, df_strats_long)

cat("\014")
m_strats_osivq |> get_singularity()
m_strats_osivq |> get_performance() |> knitr::kable(align = "c")
m_strats_osivq |> report_contrast(~ cluster | strategy) |> knitr::kable()
m_strats_osivq |> report_contrast(~ strategy | cluster) |> knitr::kable()
m_strats_osivq |> report_contrast(~ cluster * strategy, interaction = TRUE) |>
  knitr::kable()
