test_that("Modelling helper functions work as expected", {
  df_expe <- get_clean_data()$df_expe
  df_strats <- get_clean_data()$df_survey |> pivot_strategies_longer()

  model <-
    glmmTMB::glmmTMB(
      accuracy ~ group + (1 | id),
      data = df_expe,
      family = binomial
    )
  model_singular <-
    glmmTMB::glmmTMB(
      accuracy ~ category * group + (category | id) + (problem | group),
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
  expect_contains(class(get_contrast(model, ~group)), "emmGrid")
  expect_equal(
    class(get_contrast(model, ~group) |> extract_emm_confint()),
    "data.frame"
  )
  expect_equal(class(report_contrast(model, ~group)), "data.frame")
  expect_equal(class(fit_clm(score ~ group * strategy, df_strats)), "clm")
})
