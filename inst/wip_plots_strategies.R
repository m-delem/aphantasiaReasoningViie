devtools::load_all()

df_survey_2  <- get_clean_data(n_groups = 2)$df_survey
df_survey_3  <- get_clean_data(n_groups = 3)$df_survey
# Clustering OSIVQ data
clustering <- cluster_osivq(df_survey_2)
# Checking cluster properties to define names for each cluster
df_survey_2 |>
  add_named_clusters(clustering) |>
  summarise_clustering()
# Adding named clusters to the survey data
df_survey_2 <- add_named_clusters(
  df_survey_2, clustering,
  names  = c("Spatialiser", "Visualiser", "Verbaliser"),
  levels = c("Visualiser", "Spatialiser", "Verbaliser"),
  contrasts = c("_visualiser", "_spatialiser", "_verbaliser"),
  base = 1
)
# Pivoting the strategies data longer
df_strats_long_2 <- pivot_strategies_longer(df_survey_2)
df_strats_long_3 <- pivot_strategies_longer(df_survey_3)

# Plotting proportions -----------------------
plot_strategies_barplot(df_strats_long_2, group)
plot_strategies_barplot(df_strats_long_3, group)
plot_strategies_barplot(df_strats_long_2, cluster)

# Plotting scores -----------------------------
n_aph_2  <- nrow(dplyr::filter(df_survey_2, group == "Aphantasia"))
n_typ_2  <- nrow(dplyr::filter(df_survey_2, group == "Typical"))
n_aph_3  <- nrow(dplyr::filter(df_survey_3, group == "Aphantasia"))
n_hypo <- nrow(dplyr::filter(df_survey_3, group == "Hypophantasia"))
n_typ_3  <- nrow(dplyr::filter(df_survey_3, group == "Typical"))
n_visualisers  <- nrow(dplyr::filter(df_survey_2, cluster == "Visualiser"))
n_spatialisers <- nrow(dplyr::filter(df_survey_2, cluster == "Spatialiser"))
n_verbalisers  <- nrow(dplyr::filter(df_survey_2, cluster == "Verbaliser"))

p1 <-
  plot_strategies_scores(
    df_strats_long_2,
    grouping = group,
    x_labels = c(
      glue::glue("Aphantasia (N = {n_aph_2})"),
      glue::glue("Typical (N = {n_typ_2})")
    )
  )
p2 <-
  plot_strategies_scores(
    df_strats_long_3,
    grouping = group,
    x_labels = c(
      glue::glue("Aphantasia (N = {n_aph_3})"),
      glue::glue("Hypophantasia (N = {n_hypo})"),
      glue::glue("Typical (N = {n_typ_3})")
    )
  )
p3 <-
  plot_strategies_scores(
    df_strats_long_2,
    grouping = cluster,
    x_labels = c(
      glue::glue("Visualiser (N = {n_visualisers})"),
      glue::glue("Spatialiser (N = {n_spatialisers})"),
      glue::glue("Verbaliser (N = {n_verbalisers})")
    )
  )

library(patchwork)

p1 + p2 + p3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "top")
