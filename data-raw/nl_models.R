# This is the code that was used to produce the sub-products of the non-linear
# modelling that are saved as package data (nl_contrasts and nl_relations) to
# compensate for the fact that the model fits themselves were to heavy to be
# saved here.
devtools::load_all()

# Preparing the data
df_survey  <- get_clean_data()$df_survey
# Clustering OSIVQ data
clustering <- cluster_osivq(df_survey)
# Adding named clusters to the survey data
df_survey <- add_named_clusters(
  df_survey, clustering,
  names  = c("Spatialiser", "Visualiser", "Verbaliser"),
  levels = c("Visualiser", "Spatialiser", "Verbaliser"),
  contrasts = c("_visualiser", "_spatialiser", "_verbaliser"),
  base = 1
)
# Merging with experiment data
df_expe <-
  dplyr::left_join(
    get_clean_data()$df_expe,
    df_survey |> dplyr::select(id, cluster),
    by = dplyr::join_by("id")
  ) |>
  dplyr::relocate(cluster, .after = "group")

df_rt_long <-
  df_expe |>
  filter_trials_on_rt() |>
  pivot_terms_longer() |>
  dplyr::mutate(
    group_2_category = interaction(group_2, category),
    group_3_category = interaction(group_3, category),
    cluster_category = interaction(cluster, category)
  )

# Fitting the models
m_nl_vviq_2 <-
  mgcv::bam(
    formula = rt ~
      group_2_category +
      s(term, by = group_2_category,  bs = "tp", k = 4) +
      s(term, problem, by = group_2,  bs = "fs", m = 1, k = 4) +
      s(term, id,      by = category, bs = "fs", m = 1, k = 4),
    family  = Gamma(link = "identity"),
    data    = df_rt_long,
    method  = "fREML"
  )
m_nl_vviq_3 <-
  mgcv::bam(
    formula = rt ~
      group_3_category +
      s(term, by = group_3_category,  bs = "tp", k = 4) +
      s(term, problem, by = group_3,  bs = "fs", m = 1, k = 4) +
      s(term, id,      by = category, bs = "fs", m = 1, k = 4),
    family  = Gamma(link = "identity"),
    data    = df_rt_long,
    method  = "fREML"
  )
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

# Getting the contrasts of interest
contrasts_2 <-
  m_nl_vviq_2 |>
  get_contrast(
    ~ group_2_category | term,
    at = list(term = c(1, 2, 3, 4)),
    interaction = FALSE,
    adjust = "none"
  )
contrasts_3 <-
  m_nl_vviq_3 |>
  get_contrast(
    ~ group_3_category | term,
    at = list(term = c(1, 2, 3, 4)),
    interaction = FALSE,
    adjust = "none"
  )
contrasts_osivq <-
  m_nl_osivq |>
  get_contrast(
    ~ cluster_category | term,
    at = list(term = c(1, 2, 3, 4)),
    interaction = TRUE
  )

# Getting model predictions (for plotting)
preds_2 <-
  modelbased::estimate_relation(
    m_nl_vviq_2,
    by = c("group_2_category", "term")
  ) |>
  as.data.frame() |>
  dplyr::select(group_2_category, term, Predicted, CI_low, CI_high) |>
  tidyr::separate_wider_delim(
    group_2_category,
    delim = ".",
    names = c("group", "category")
  )

preds_3 <-
  modelbased::estimate_relation(
    m_nl_vviq_3,
    by = c("group_3_category", "term")
  ) |>
  as.data.frame() |>
  dplyr::select(group_3_category, term, Predicted, CI_low, CI_high) |>
  tidyr::separate_wider_delim(
    group_3_category,
    delim = ".",
    names = c("group", "category")
  )

preds_osivq <-
  modelbased::estimate_relation(
    m_nl_osivq,
    by = c("cluster_category", "term")
  ) |>
  as.data.frame() |>
  dplyr::select(cluster_category, term, Predicted, CI_low, CI_high) |>
  tidyr::separate_wider_delim(
    cluster_category,
    delim = ".",
    names = c("group", "category")
  )

# Saving contrasts and predictions as package data
nl_contrasts <- list(
  vviq_2 = contrasts_2,
  vviq_3 = contrasts_3,
  osivq  = contrasts_osivq
)
nl_predictions <- list(
  vviq_2 = preds_2,
  vviq_3 = preds_3,
  osivq  = preds_osivq
)

usethis::use_data(nl_contrasts, overwrite = TRUE)
usethis::use_data(nl_predictions, overwrite = TRUE)
