#' Get a long format data frame with the problem terms in a single column
#'
#' @param df A data frame containing the problem terms with columns for
#' `premise_1_rt`, `premise_2_rt`, `premise_3_rt`, and `conclusion_rt` along
#' with other relevant columns like `group`, `category`, and `problem`.
#'
#' @returns A data frame in long format with columns with the problem term a an
#' integer in a `term` column and as a factor in a `term_name` column.
#' @export
#'
#' @examples
#' get_clean_data()$df_expe |>
#'   filter_trials_on_rt() |>
#'   pivot_terms_longer() |>
#'   dplyr::select(id, problem, category, term, term_name, rt)
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
    ) |>
    dplyr::mutate(problem = as.factor(.data$problem))

  return(df_pivoted)
}

#' Get a long format data frame with the strategies gathered in a single column
#'
#' @param df A data frame containing the strategies data with columns for
#' `id`, `group`, `visual_strat`, `spatial_strat`, `verbal_strat`,
#' `semantic_strat`, and `sensorimotor_strat`.
#' @param base An integer indicating the base level for factor contrasts.
#' Default is 1 for the visual strategy.
#' @param ... Additional arguments passed to [add_factor_contrasts()].
#'
#' @returns A data frame in long format with columns for `id`, `group`,
#' `strategy`, and `score`, where `strategy` is a factor with levels for each
#' strategy and `score` is a factor indicating the level of strategy use.
#' @export
#'
#' @examples
#' get_clean_data()$df_survey |>
#'   pivot_strategies_longer() |>
#'   dplyr::select(id, group, strategy, score)
pivot_strategies_longer <- function(df, base = 1, ...) {
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
      score = factor(
        .data$score,
        levels = c(
          "no_use",
          "secondary_strat",
          "as_much_as_others",
          "mainly_this_strat",
          "only_this_strat"
        )
      ),
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
          base = base,
          ...
        )
    )

  return(df_pivoted)
}
