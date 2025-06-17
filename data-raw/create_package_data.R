# Create the clean datasets from the raw OSF data (see import_osf_data.R)
devtools::load_all()

data_paths <- here::here("inst/extdata") |> fs::dir_ls(glob = "*.rda")
load(data_paths[1]) # common_survey_data_full
load(data_paths[2]) # expe_reasoning_viie_data

survey_data <-
  dplyr::inner_join(
    common_survey_data_full |>
      dplyr::select(
        id:country, language_native, language_usual, job:treatment,
        vviq_is_complete, vviq_total_score:vviq_q16,
        osivq_is_complete,
        osivq_object  = object_mean,
        osivq_spatial = spatial_mean,
        osivq_verbal  = verbal_mean,
        osivq_q01s:osivq_q45o,
        raven_is_complete, raven_score,
        nieq_is_complete, nieq_freq_inner_voice:nieq_prop_unsymbolised,
        tidyselect::contains("false_info")
    ),
    expe_reasoning_viie_data |>
      dplyr::select(
        id, language,
        met_issues:what_external_support,
        tidyselect::contains("strats")
      ) |>
      dplyr::distinct(),
    by = "id"
  ) |>
  dplyr::relocate(language, .after = "id")

experiment_data <-
  expe_reasoning_viie_data |>
  dplyr::select(
    id, language, Group,
    expe_phase, trial_number, problem = trial_code,
    category:correct_response, accuracy
  ) |>
  dplyr::filter(expe_phase != "training")

usethis::use_data(survey_data, overwrite = TRUE)
usethis::use_data(experiment_data, overwrite = TRUE)
