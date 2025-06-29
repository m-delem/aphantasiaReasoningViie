test_that("factoring functions work properly", {
  # WIP: convert my manual tests of those contrasts into automated tests
  # clean_expe_data <-
  #   dplyr::inner_join(
  #     filter_random_accuracy_ids(experiment_data),
  #     filter_manually_identified_ids(survey_data) |>
  #       dplyr::select(id),
  #     by = "id"
  #   ) |>
  #   filter_suspicious_rt_ids(sd_mult = 2)
  # clean_expe_data |>
  #   factor_groups(n_groups = 4) |>
  #   dplyr::pull(group) |> contrasts()

  expect_equal(2 * 2, 4)
})
