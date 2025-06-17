devtools::load_all()

# Writing an Excel file
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
osf_retrieve_node("hfbcp") |>
  osf_ls_files() |>
  dplyr::filter(name == "Data") |>
  osf_ls_files() |>
  dplyr::filter(name == "Raw data") |>
  osf_upload(
    here::here(fs::dir_ls("inst/extdata")),
    conflicts = "overwrite"
  )
