devtools::load_all()

df_survey  <- get_clustered_data("survey")

# Bayesian models ----------------------------------------------

# Raven ----------
m_raven_vviq_2 <-
  brms::brm(
    raven_score ~ group_2,
    data = df_survey,
    file = "inst/extdata/models/brms_raven_vviq_2.rds",
    seed = 667
  )
marginaleffects::avg_comparisons(
  m_raven_vviq_2,
  variables = list("group_2" = "pairwise")
  ) |> report_rope(contrast)

m_raven_vviq_3 <-
  brms::brm(
    raven_score ~ group_3,
    data = df_survey,
    file = "inst/extdata/models/brms_raven_vviq_3.rds",
    seed = 667
  )
marginaleffects::avg_comparisons(
  m_raven_vviq_3,
  variables = list("group_3" = "pairwise")
  ) |> report_rope(contrast)

m_raven_osivq  <-
  brms::brm(
    raven_score ~ cluster,
    data = df_survey,
    file = "inst/extdata/models/brms_raven_osivq.rds",
    seed = 667
  )
marginaleffects::avg_comparisons(
  m_raven_osivq,
  variables = list("cluster" = "pairwise")
  ) |> report_rope(contrast)

# Age ----------
m_age_vviq_2 <-
  brms::brm(
    age ~ group_2,
    data = df_survey,
    file = "inst/extdata/models/brms_age_vviq_2.rds",
    seed = 667
  )
marginaleffects::avg_comparisons(
  m_age_vviq_2,
  variables = list("group_2" = "pairwise")
  ) |> report_rope(contrast)

m_age_vviq_3 <-
  brms::brm(
    age ~ group_3,
    data = df_survey,
    file = "inst/extdata/models/brms_age_vviq_3.rds",
    seed = 667
  )
marginaleffects::avg_comparisons(
  m_age_vviq_3,
  variables = list("group_3" = "pairwise")
) |> report_rope(contrast)

m_age_osivq <-
  brms::brm(
    age ~ cluster,
    data = df_survey,
    file = "inst/extdata/models/brms_age_osivq.rds",
    seed = 667
  )
marginaleffects::avg_comparisons(
  m_age_osivq,
  variables = list("cluster" = "pairwise")
) |> report_rope(contrast)

# Frequentist equivalents ------------------------------------

# Raven ---------
m_raven_vviq_2 <- lm(raven_score ~ group_2, data = df_survey)
m_raven_vviq_3 <- lm(raven_score ~ group_3, data = df_survey)
m_raven_osivq  <- lm(raven_score ~ cluster, data = df_survey)

# Omnibus
anova(m_raven_vviq_2)
anova(m_raven_vviq_3)
anova(m_raven_osivq)

# Contrasts
get_contrast(m_raven_vviq_2, ~group_2) |> knitr::kable(digits = 2)
get_contrast(m_raven_vviq_3, ~group_3) |> knitr::kable(digits = 2)
get_contrast(m_raven_osivq,  ~cluster) |> knitr::kable(digits = 2)

# Age ---------
m_age_vviq_2 <- lm(age ~ group_2, data = df_survey)
m_age_vviq_3 <- lm(age ~ group_3, data = df_survey)
m_age_osivq  <- lm(age ~ cluster, data = df_survey)

# Omnibus
anova(m_age_vviq_2)
anova(m_age_vviq_3)
anova(m_age_osivq)

# Contrasts
get_contrast(m_age_vviq_2, ~group_2) |> knitr::kable(digits = 2)
get_contrast(m_age_vviq_3, ~group_3) |> knitr::kable(digits = 2)
get_contrast(m_age_osivq,  ~cluster) |> knitr::kable(digits = 2)
