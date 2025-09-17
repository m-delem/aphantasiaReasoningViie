test_that("Factoring functions work as expected", {
  df_chr   <- factor_chr_vars(survey_data)
  df_cat   <- factor_categories(experiment_data)
  df_strat <- factor_strategies(survey_data, ordered = TRUE)
  df_g3    <- factor_groups(survey_data, n_groups = 3, contrast_base = 1)
  df_g4    <- factor_groups(survey_data, n_groups = 4, contrast_base = 1)
  df_all   <- create_all_groups(df_strat)

  expect_contains(class(df_chr), c("tbl", "data.frame"))
  expect_contains(class(df_cat), c("tbl", "data.frame"))
  expect_contains(class(df_strat), c("tbl", "data.frame"))
  expect_contains(class(df_g3), c("tbl", "data.frame"))
  expect_contains(class(df_g4), c("tbl", "data.frame"))
  expect_contains(class(df_all), c("tbl", "data.frame"))
  expect_error(
    factor_groups(survey_data, n_groups = 5),
    "n_groups must be 2, 3 or 4."
  )
  expect_error(
    factor_groups(survey_data, n_groups = 2, contrast_base = 3),
    "contrast_base must be a valid index of the levels."
  )
})
