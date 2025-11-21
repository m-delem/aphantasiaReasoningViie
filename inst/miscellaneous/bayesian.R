if(!requireNamespace("cmdstanr", quietly = TRUE)) {
  install.packages(
    "cmdstanr",
    repos = c('https://stan-dev.r-universe.dev', getOption("repos"))
  )
}
devtools::load_all()
pacman::p_load(brms, dplyr, marginaleffects, modelbased, performance)

df_expe <- get_clustered_data("experiment")
df_rt   <-
  filter_trials_on_rt(df_expe, verbose = TRUE) |>
  rename("group_4" = "group")

model <-
  fit_brms_model(
    rt_total ~ group_3 * category + (category | id) + (group_3 | problem),
    data    = df_rt,
    family  = shifted_lognormal(),
    prior   = c(prior(normal(0, 10), class = "b")),
    iterations = 40000,
    file_refit = "on_change",
    file = here::here("inst/models/m_rt_shiftlog.rds")
  )

avg_comparisons(
  model,
  variables = list("category" = "pairwise"),
  by = "group_3",
  # hypothesis = ~pairwise, # for the interaction
  equivalence = c(-1, 1),
  # cross = TRUE, # bad
  ndraws = 1000
)
