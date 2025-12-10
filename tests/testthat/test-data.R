test_that("All IDs only have a single experiment (27 trials)", {
  unique_n_trials <-
    experiment_data |>
    dplyr::group_by(id) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::pull(n) |>
    unique()
  expect_equal(length(unique_n_trials), 1)
  expect_equal(unique_n_trials, 27)
})

test_that("Data preparation functions work properly", {
  data_list <- get_clean_data("all")
  data_clustered <- get_clustered_data("all")
  df_survey <- get_clustered_data("survey")
  df_expe   <- get_clustered_data("experiment")
  clustering <- get_clustered_data("clustering")

  expect_true(all(c("df_expe", "df_survey") %in% names(data_list)))
  expect_equal(data_clustered$df_survey, df_survey)
  expect_equal(data_clustered$df_expe, df_expe)
  expect_equal(class(clustering), "list")

  expect_contains(class(get_viie_data()), c("tbl", "data.frame"))

  expect_error(get_clean_data(type = "invalid_type"))
  expect_error(get_clustered_data(type = "invalid_type"))
})
