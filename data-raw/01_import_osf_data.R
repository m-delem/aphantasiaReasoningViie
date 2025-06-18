library(osfr)

# Creating an OSF Personal Access Token (PAT) is necessary for downloads from a
# private project.
# See: https://docs.ropensci.org/osfr/articles/auth
# osf_auth("ThIsIsNoTaReAlPATbUtYoUgEtIt")

# Retrieving the RDA data files from the "Aphantasia Online Studies Data" OSF
# project
osf_retrieve_node("hzqdj") |>
  osf_ls_files() |>
  dplyr::filter(stringr::str_detect(name, "R")) |>
  osf_ls_files() |>
  dplyr::filter(stringr::str_detect(name, "full|viie")) |>
  osf_download(path = here::here("inst/extdata"), conflicts = "overwrite")
