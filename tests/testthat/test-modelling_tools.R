test_that("Modelling helper functions work as expected", {
  df_expe <- get_clean_data("experiment")
  df_strats <- get_clean_data("survey") |> pivot_strategies_longer()

  model <-
    glmmTMB::glmmTMB(
      accuracy ~ group_4 + (1 | id),
      data = df_expe,
      family = binomial
    )
  model_singular <-
    glmmTMB::glmmTMB(
      accuracy ~ category * group_4 + (category | id) + (problem | group_4),
      data = df_expe,
      family = binomial
    )|> suppressWarnings()

  expect_equal(class(build_formula("accuracy", "group")), "formula")
  expect_equal(class(set_ranef_prior(100)), "data.frame")
  expect_equal(
    class(get_singularity(model) |> suppressMessages()),
    "NULL"
  )
  expect_equal(
    class(get_singularity(model_singular) |> suppressMessages()),
    "NULL"
  )
  expect_equal(class(get_performance(model)), "data.frame")
  expect_equal(class(get_params(model)), "data.frame")
  expect_contains(class(get_contrast(model, ~group_4)), "emmGrid")
  expect_equal(
    class(get_contrast(model, ~group_4) |> extract_emm_confint()),
    "data.frame"
  )
  expect_equal(class(report_contrast(model, ~group_4)), "data.frame")
  expect_equal(class(fit_clm(score ~ group_4 * strategy, df_strats)), "clm")
})
