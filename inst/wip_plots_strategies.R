devtools::load_all()

df_survey  <- get_clean_data(n_groups = 3)$df_survey
# Clustering OSIVQ data
clustering <- cluster_osivq(df_survey)
# Checking cluster properties to define names for each cluster
df_survey |>
  add_named_clusters(clustering) |>
  summarise_clustering()
# Adding named clusters to the survey data
df_survey <- add_named_clusters(
  df_survey, clustering,
  names  = c("Verbaliser", "Visualiser", "Spatialiser"),
  levels = c("Visualiser", "Spatialiser", "Verbaliser"),
  contrasts = c("_visualiser", "_spatialiser", "_verbaliser"),
  base = 1
)
# Pivoting the strategies data longer
df_strats_long <- pivot_strategies_longer(df_survey)

# Plotting proportions -----------------------
plot_strategies_barplot(df_strats_long, group)
plot_strategies_barplot(df_strats_long, cluster)

# Plotting scores -----------------------------
n_aph  <- nrow(dplyr::filter(df_survey, group == "Aphantasia"))
n_hypo <- nrow(dplyr::filter(df_survey, group == "Hypophantasia"))
n_typ  <- nrow(dplyr::filter(df_survey, group == "Typical"))
n_visualisers  <- nrow(dplyr::filter(df_survey, cluster == "Visualiser"))
n_spatialisers <- nrow(dplyr::filter(df_survey, cluster == "Spatialiser"))
n_verbalisers  <- nrow(dplyr::filter(df_survey, cluster == "Verbaliser"))

plot_strategies_scores(
  df_strats_long,
  grouping = group,
  x_labels = c(
    glue::glue("Aphantasia (N = {n_aph})"),
    glue::glue("Hypophantasia (N = {n_hypo})"),
    glue::glue("Typical (N = {n_typ})")
  )
)
plot_strategies_scores(
  df_strats_long,
  grouping = cluster,
  x_labels = c(
    glue::glue("Visualiser (N = {n_visualisers})"),
    glue::glue("Spatialiser (N = {n_spatialisers})"),
    glue::glue("Verbaliser (N = {n_verbalisers})")
  )
)

