devtools::load_all()
library(dplyr)

df_survey <- get_clean_data(n_groups = 4)$df_survey

c <- mclust::Mclust(df_survey |> select(osivq_object:osivq_verbal), G = 3)

df <- df_survey |> mutate(cluster = c$classification)
df |>
  group_by(group, cluster) |>
  reframe(
    n = n(),
    mean_osivq_object = mean(osivq_object, na.rm = TRUE),
    mean_osivq_spatial = mean(osivq_spatial, na.rm = TRUE),
    mean_osivq_verbal = mean(osivq_verbal, na.rm = TRUE)
  ) |>
  arrange(cluster)
