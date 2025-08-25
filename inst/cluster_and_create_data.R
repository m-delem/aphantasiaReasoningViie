devtools::load_all()

df_survey  <- get_clean_data(verbose = TRUE)$df_survey

# Clustering OSIVQ data
clustering <- cluster_osivq(df_survey)

# Checking cluster properties to define names for each cluster
df_survey |>
  add_named_clusters(clustering) |>
  summarise_clustering()

# Adding named clusters to the survey data
df_survey <- add_named_clusters(
  df_survey, clustering,
  names  = c("Spatialiser", "Visualiser", "Verbaliser"),
  levels = c("Visualiser", "Spatialiser", "Verbaliser"),
  contrasts = c("_visualiser", "_spatialiser", "_verbaliser"),
  base = 1
)

# Checks
contrasts(df_survey$cluster)
summarise_clustering(df_survey)

df_expe <-
  dplyr::left_join(
    get_clean_data()$df_expe,
    df_survey |> dplyr::select(id, cluster),
    by = dplyr::join_by("id")
  ) |>
  dplyr::relocate(cluster, .after = "group")

df_rt      <- filter_trials_on_rt(df_expe, verbose = TRUE)
df_rt_long <-
  pivot_terms_longer(df_rt) |>
  dplyr::mutate(
    group_2_category = interaction(group_2, category),
    group_3_category = interaction(group_3, category),
    cluster_category = interaction(cluster, category)
  )

df_strats_long <- pivot_strategies_longer(df_survey)
