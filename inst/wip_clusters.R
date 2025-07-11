devtools::load_all()

df_survey  <- get_clean_data(n_groups = 2)$df_survey

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

# Checks
contrasts(df_survey$cluster)
summarise_clustering(df_survey)

# Plotting
plot_osivq_ternary(
  df_survey,
  colours = palette.colors()[c(3, 2, 4)],
  base_theme = ggplot2::theme_grey
)
