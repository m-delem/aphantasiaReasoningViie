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
