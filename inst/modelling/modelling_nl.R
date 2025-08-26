# Run "cluster_and_create_data first

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

cat("\014")
m_nl_vviq_2 |> get_singularity()
contrasts_2 <-
  m_nl_vviq_2 |>
  get_contrast(
    ~ group_2_category | term,
    at = list(term = c(1, 2, 3, 4)),
    interaction = FALSE,
    adjust = "none"
  )
contrasts_2 |>
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
  dplyr::filter(group_1 == group_2 & p.value < 1) |>
  dplyr::select(!c(
    tidyselect::contains("group_2"),
    tidyselect::contains("cluster_2"),
    tidyselect::contains("SE"),
    tidyselect::contains("df")
  )) |>
  dplyr::mutate(across(c(estimate:p.value), ~round(., 3))) |>
  tidyr::unite(`Category contrast`, category_1, category_2, sep = " - ") |>
  dplyr::rename(group = group_1, `RT difference` = estimate) |>
  dplyr::arrange(term, group) |>
  dplyr::mutate(
    term = term |>
      as.character() |>
      dplyr::case_match(
        "1" ~ "Premise 1",
        "2" ~ "Premise 2",
        "3" ~ "Premise 3",
        "4" ~ "Conclusion"
      ),
  ) |>
  knitr::kable(digits = 3)

cat("\014")
m_nl_vviq_3 |> get_singularity()
contrasts_3 <-
  m_nl_vviq_3 |>
  get_contrast(
    ~ group_3_category | term,
    at = list(term = c(1, 2, 3, 4)),
    interaction = FALSE,
    adjust = "none"
  )
contrasts_3 |>
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
  dplyr::filter(group_1 == group_2 & p.value < 1) |>
  dplyr::select(!c(
    tidyselect::contains("group_2"),
    tidyselect::contains("cluster_2"),
    tidyselect::contains("SE"),
    tidyselect::contains("df")
  )) |>
  dplyr::mutate(across(c(estimate:p.value), ~round(., 3))) |>
  tidyr::unite(`Category contrast`, category_1, category_2, sep = " - ") |>
  dplyr::rename(group = group_1, `RT difference` = estimate) |>
  dplyr::arrange(term, group) |>
  dplyr::mutate(
    term = term |>
      as.character() |>
      dplyr::case_match(
        "1" ~ "Premise 1",
        "2" ~ "Premise 2",
        "3" ~ "Premise 3",
        "4" ~ "Conclusion"
      ),
  ) |>
  knitr::kable(digits = 3)

cat("\014")
m_nl_osivq |> get_singularity()
contrasts_osivq <-
  m_nl_osivq |>
  get_contrast(
    ~ cluster_category | term,
    at = list(term = c(1, 2, 3, 4)),
    interaction = TRUE
  )
contrasts_osivq |>
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
  dplyr::filter(cluster_1 == cluster_2 & p.value < 1) |>
  dplyr::select(!c(
    tidyselect::contains("group_2"),
    tidyselect::contains("cluster_2"),
    tidyselect::contains("SE"),
    tidyselect::contains("df")
  )) |>
  dplyr::mutate(across(c(estimate:p.value), ~round(., 3))) |>
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
