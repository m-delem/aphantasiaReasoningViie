pivot_terms_longer <- function(df) {
  df_pivoted <-
    df |>
    tidyr::pivot_longer(
      cols = "premise_1_rt":"conclusion_rt",
      names_to = "term_name",
      values_to = "rt"
    ) |>
    dplyr::mutate(
      group_category   = interaction(.data$group, .data$category),
      # cluster_category = interaction(cluster, category),
      term = dplyr::case_match(
        .data$term_name,
        "premise_1_rt"  ~ 1,
        "premise_2_rt"  ~ 2,
        "premise_3_rt"  ~ 3,
        "conclusion_rt" ~ 4
      ),
      term_name = factor(
        .data$term_name,
        levels = c(
          "premise_1_rt",
          "premise_2_rt",
          "premise_3_rt",
          "conclusion_rt"
        ),
        labels = c("Premise 1", "Premise 2", "Premise 3", "Conclusion")
      ),
      .after = "category"
    )

  return(df_pivoted)
}

pivot_strategies_longer <- function(df) {
  df_pivoted <-
    df |>
    factor_strategies(ordered = FALSE) |>
    dplyr::select(
      "id":"group",
      tidyselect::contains("cluster"),
      "visual_strat":"sensorimotor_strat"
    ) |>
    tidyr::pivot_longer(
      cols = "visual_strat":"sensorimotor_strat",
      names_to = "strategy",
      values_to = "score"
    ) |>
    dplyr::filter(.data$score != 0) |>
    dplyr::mutate(
      score = factor(.data$score, levels = 1:5),
      strategy = factor(
        .data$strategy,
        levels = c(
          "visual_strat",
          "spatial_strat",
          "verbal_strat",
          "semantic_strat",
          "sensorimotor_strat"
        ),
        labels = c(
          "Visual",
          "Spatial",
          "Verbal",
          "Semantic",
          "Sensorimotor"
          )
        ) |>
        add_factor_contrasts(
          n = c("_visual", "_spatial", "_verbal", "_semantic", "_sensorimotor"),
          base = 1
        )
    )

  return(df_pivoted)
}
