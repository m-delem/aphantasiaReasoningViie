devtools::load_all()

df_survey  <- get_clean_data(verbose = TRUE)$df_survey

clustering <- cluster_osivq(
  df_survey,
  algorithms = c("gmm", "pam", "cmeans"),
  progress = FALSE,
  verbose = FALSE
)

clustering$clusters |>
  as.data.frame() |>
  dplyr::group_by(kmodes) |>
  dplyr::count()

df_survey |>
  dplyr::mutate(cluster = clustering$clusters[, "kmodes"]) |>
  dplyr::reframe(
    .by = c("group", "cluster"),
    n = dplyr::n(),
    vviq = mean(vviq_total_score, na.rm = TRUE) |> round(2),
    object  = mean(osivq_object, na.rm = TRUE) |> round(2),
    spatial = mean(osivq_spatial, na.rm = TRUE) |> round(2),
    verbal  = mean(osivq_verbal, na.rm = TRUE) |> round(2),
    raven = mean(raven_score, na.rm = TRUE) |> round(2),
  ) |>
  dplyr::arrange(cluster)
