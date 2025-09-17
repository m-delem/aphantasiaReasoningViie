test_that("Trial filtering functions work as expected", {
  df_rt <-
    get_clean_data()$df_expe |>
    filter_trials_on_rt(verbose = TRUE) |>
    suppressMessages()
  df_trials <- get_trial_count(df_rt)

  expect_contains(class(df_rt), c("tbl", "data.frame"))
  expect_contains(class(df_trials), c("tbl", "data.frame"))
})
