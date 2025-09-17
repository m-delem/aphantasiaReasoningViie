test_that("Participant filtering functions work as expected", {
  df1 <-
    filter_manually_identified_ids(survey_data, verbose = TRUE) |>
    suppressMessages()

  df2 <-
    filter_manually_identified_ids(
      survey_data,
      exclude_no_vviq = FALSE,
      exclude_no_osivq = FALSE,
      exclude_no_raven = FALSE,
      exclude_cheated = FALSE,
      exclude_distracted = FALSE,
      exclude_treatment = TRUE,
      exclude_adhd = TRUE,
      exclude_asd = TRUE,
      exclude_dyslexia = TRUE,
      exclude_other = TRUE,
      verbose = FALSE
    )

  df3 <-
    filter_random_accuracy_ids(experiment_data, verbose = TRUE) |>
    suppressMessages()

  df4 <-
    mark_suspicious_rt_ids(experiment_data) |>
    suppressMessages()
  df5 <-
    filter_suspicious_rt_ids(experiment_data) |>
    suppressMessages()

  expect_contains(class(df1), c("tbl", "data.frame"))
  expect_contains(class(df2), c("tbl", "data.frame"))
  expect_contains(class(df3), c("tbl", "data.frame"))
  expect_contains(class(df4), c("tbl", "data.frame"))
  expect_contains(class(df5), c("tbl", "data.frame"))
})
