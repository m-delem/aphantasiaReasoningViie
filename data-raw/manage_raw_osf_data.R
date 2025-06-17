library(osfr)

# Creating an OSF Personal Access Token (PAT) is necessary for downloads from a
# private project.
# See: https://docs.ropensci.org/osfr/articles/auth
# osf_auth("ThIsIsNoTaReAlPATbUtYoUgEtIt")

# Retrieving the processed data from the "Aphantasia Online Studies Data" OSF
# project
online_data_files <- osf_retrieve_node("hzqdj") |> osf_ls_files()

# Extracting RDA files
online_data_files |>
  dplyr::filter(stringr::str_detect(name, "R")) |>
  osf_ls_files() |>
  dplyr::filter(stringr::str_detect(name, "full|viie")) |>
  osf_download(path = here::here("inst/extdata"), conflicts = "overwrite")
# Extracting the raw Excel file
online_data_files |>
  dplyr::filter(stringr::str_detect(name, "Excel")) |>
  osf_ls_files() |>
  dplyr::filter(stringr::str_detect(name, "full")) |>
  osf_download(path = here::here("inst/extdata"), conflicts = "overwrite")

# Selecting the sheets from the Excel files that are relevant to this study
excel_file <- fs::dir_ls(glob = "*.xlsx", recurse = TRUE)
excel_data <- list(
  common_survey_data_full =
    readxl::read_xlsx(excel_file, sheet = "common_survey_data_full"),
  reasoning_data =
    readxl::read_xlsx(excel_file, sheet = "reasoning_data")
)
# Overwriting the Excel file
openxlsx::write.xlsx(
  x          = excel_data,
  file       = here::here(excel_file),
  asTable    = TRUE,
  colNames   = TRUE,
  colWidths  = "auto",
  borders    = "all",
  tableStyle = "TableStyleMedium16"
)

# Uploading this data to the OSF project dedicated to this study
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
