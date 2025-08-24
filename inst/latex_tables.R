describe_survey_data(df_survey, "cluster") |>
  dplyr::select(cluster:`RSPM-18`) |>
  dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) |>
  tidyr::pivot_longer(-cluster, names_to = " ") |>
  tidyr::pivot_wider(names_from = cluster, values_from = value) |>
  Hmisc::latex(
    booktabs = TRUE,
    rowname = NULL,
    file = "",
    title = "",
    insert.bottom = "\\label{tbl:osivq-clusters}"
  )

describe_survey_data(df_survey, "group_3") |>
  dplyr::select(group_3:`RSPM-18`) |>
  dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) |>
  tidyr::pivot_longer(-group_3, names_to = " ") |>
  tidyr::pivot_wider(names_from = group_3, values_from = value) |>
  dplyr::relocate("Typical", .after = dplyr::last_col()) |>
  Hmisc::latex(
    booktabs = TRUE,
    rowname = NULL,
    file = "",
    title = "",
    insert.bottom = "\\label{tbl:vviq-groups}"
  )
