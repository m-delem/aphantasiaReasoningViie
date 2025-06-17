devtools::load_all()

data_paths <- here::here("inst/extdata") |> fs::dir_ls(glob = "*.rda")
load(data_paths[1]) # common_survey_data_full
load(data_paths[2]) # expe_reasoning_viie_data
