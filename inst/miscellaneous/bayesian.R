if(!requireNamespace("cmdstanr", quietly = TRUE)) {
  install.packages(
    "cmdstanr",
    repos = c('https://stan-dev.r-universe.dev', getOption("repos"))
  )
}
devtools::load_all()
pacman::p_load(brms, dplyr, marginaleffects, modelbased, performance, tidyr)

df_rt   <-
  get_clustered_data("experiment") |>
  filter_trials_on_rt() |>
  rename("group_4" = "group")

m_rt <-
  fit_brms_model(
    rt_total ~ group_3 * category + (category | id),
    data    = df_rt,
    family = exgaussian(),
    # family  = shifted_lognormal(),
    # family  = Gamma(link = "log"),
    prior   = c(prior(normal(0, 10), class = "b")),
    iterations = 40000,
    file_refit = "on_change",
    file = here::here("inst/models/m_rt_exgauss.rds"),
    # file = here::here("inst/models/m_rt_gamma.rds"),
  )

# contr <-
  avg_comparisons(
    m_rt,
    variables = list("category" = "revpairwise"),
    # variables = "category",
    by = "group_3",
    hypothesis = ~revpairwise, # for the interaction
    equivalence = bayestestR::rope_range(m_rt),
    ndraws = 1000
  )

contr |>
  as_tibble() |>
  separate_wider_delim(
    hypothesis, delim = ") - (",
    names = c("level_1", "level_2")
  ) |>
  mutate(
    across(
      c("level_1", "level_2"),
      ~ stringr::str_remove_all(., stringr::fixed("(")) |>
        stringr::str_remove_all(stringr::fixed(")"))
    )
  ) |>
  separate_wider_delim(
    c("level_1", "level_2"), delim = " - ",
    names = c("cat_1", "cat_2"), names_sep = "_"
  ) |>
  separate_wider_delim(
    contains("cat_2"), delim = " ",
    names = c("cat", "group"), names_sep = "_"
  ) |>
  filter(
    .data$level_1_cat_1 == .data$level_2_cat_1 &
    .data$level_1_cat_2_cat == .data$level_2_cat_2_cat
  ) |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  unite(
    "Category contrast",
    "level_1_cat_1":"level_1_cat_2_cat",
    sep = " - "
  ) |>
  unite(
    "Group contrast",
    c("level_1_cat_2_group","level_2_cat_2_group"),
    sep = " - "
  ) |>
  unite(
    "95% CI",
    c("conf.low","conf.high"),
    sep = ", "
  ) |>
  mutate(`95% CI` = paste0("[", `95% CI`, "]")) |>
  rename(
    "Pr(ROPE)" = "p.rope.conditional",
    "Pr(ROPE|CI)" = "p.rope.unconditional",
  )|>
  select(!c("level_2_cat_1", "level_2_cat_2_cat")) |>
  arrange(`Group contrast`)
