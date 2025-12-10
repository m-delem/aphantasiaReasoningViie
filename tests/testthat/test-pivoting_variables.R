test_that("Pivoting functions work as expected", {
  df_phases <-
    get_clean_data("experiment") |>
    filter_trials_on_rt() |>
    pivot_phases_longer()

  df_strats <-
    get_clean_data("survey") |>
    pivot_strategies_longer()

  expect_true(all(c(
    "phase"      %in% colnames(df_phases),
    "phase_name" %in% colnames(df_phases),
    "rt"        %in% colnames(df_phases)
  )))
  expect_true(all(c(
    "strategy" %in% colnames(df_strats),
    "score"    %in% colnames(df_strats)
  )))
})
