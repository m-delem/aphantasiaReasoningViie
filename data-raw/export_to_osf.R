devtools::load_all()
library(osfr)

# Writing an Excel file for the team
openxlsx::write.xlsx(
  x = list(
    survey_data = survey_data,
    experiment_data = experiment_data
  ),
  file       = here::here("inst/extdata/raw_data.xlsx"),
  asTable    = TRUE,
  colNames   = TRUE,
  colWidths  = "auto",
  borders    = "all",
  tableStyle = "TableStyleMedium16"
)

# Uploading the data to the OSF project dedicated to this study
# ("The Impact of Mental Images on Reasoning: A Study on Aphantasia")
project_files <-
  osf_retrieve_node("hfbcp") |>
  osf_ls_files() |>
  dplyr::filter(name == "Data") |>
  osf_ls_files()

project_files |>
  dplyr::filter(name == "Excel format") |>
  osf_upload(
    here::here(fs::dir_ls(regexp = "raw_data", recurse = TRUE)),
    conflicts = "overwrite"
  )
project_files |>
  dplyr::filter(name == "R format") |>
  osf_upload(here::here(fs::dir_ls("data/")), conflicts = "overwrite")
