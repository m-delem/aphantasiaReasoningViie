# Run "cluster_and_create_data first

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
