test_that("Pivoting functions work as expected", {
  df_terms <-
    get_clean_data()$df_expe |>
    filter_trials_on_rt() |>
    pivot_terms_longer()

  df_strats <-
    get_clean_data()$df_survey |>
    pivot_strategies_longer()

  expect_true(all(c(
    "term"      %in% colnames(df_terms),
    "term_name" %in% colnames(df_terms),
    "rt"        %in% colnames(df_terms)
  )))
  expect_true(all(c(
    "strategy" %in% colnames(df_strats),
    "score"    %in% colnames(df_strats)
  )))
})
