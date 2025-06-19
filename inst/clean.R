devtools::load_all()

clean_expe_data <-
  dplyr::inner_join(
    filter_random_accuracy_ids(experiment_data),
    filter_manually_identified_ids(survey_data) |>
      dplyr::select(id),
    by = "id"
  ) |>
  filter_suspicious_rt_ids(sd_mult = 2)

clean_survey_data <- survey_data |> dplyr::filter(id %in% clean_expe_data$id)

