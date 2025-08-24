#' Add treatment contrasts to a factor
#'
#' @param vect A factor vector to which contrasts will be added.
#' @param n A vector of levels for the factor. Default is the levels of `vect`.
#' @param ... Additional arguments passed to `contr.treatment`.
#'
#' @returns A factor vector with treatment contrasts added.
#' @export
#'
#' @keywords internal
add_factor_contrasts <- function(vect, n = levels(vect), ...) {
  stats::contrasts(vect) <- stats::contr.treatment(n = n, ...)
  return(vect)
}

#' Convert all character variables in a data frame to factors
#'
#' @param df A data frame.
#'
#' @returns A data frame with all character variables converted to factors.
#' @export
factor_chr_vars <- function(df) {
  df_factored <-
    df |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character),
        ~as.factor(as.character(.))
      )
    )
  return(df_factored)
}

#' Convert the `category` column to a factor with contrasts
#'
#' @param df A data frame containing a `category` column.
#' @param n A vector of levels for the factor. Default is
#' `c("_control", "_spatial", "_visual")`.
#' @param base The base level for the contrasts. Default is 3 for the visual
#' category, as our hypothesis called for planned comparisons of the visual
#' category with the other two. This argument can be used to set a different
#' base level if desired.
#' @param ... Additional arguments passed to [add_factor_contrasts()].
#'
#' @returns A data frame with the `category` column converted to a factor with
#' contrasts added.
#' @export
#'
#' @examples
#' df <- factor_categories(experiment_data)
#' contrasts(df$category)
factor_categories <- function(
    df,
    n = c("_control", "_spatial", "_visual"),
    base = 3,
    ...
) {
  df_factored <-
    df |>
    dplyr::mutate(
      category =
        .data$category |>
        stringr::str_to_title() |>
        as.factor() |>
        add_factor_contrasts(n = n, base = base, ...)
    )
  return(df_factored)
}

#' Convert strategy columns to factors with optionally ordered levels
#'
#' @param df A data frame containing strategy columns to be converted to
#' factors.
#' @param ordered A logical indicating whether the factor levels should be
#' ordered.
#'
#' @returns A data frame with strategy columns converted to factors, with levels
#' ranging from 1 to 5 for ordered factors, or as character strings for
#' unordered factors.
#' @export
#'
#' @examples
#' # The filtering function removes participants with bad or incomplete data,
#' # notably those who did not complete the strategy questionnaire.
#' df <- survey_data |>
#'   filter_manually_identified_ids() |>
#'   factor_strategies(ordered = TRUE)
#' levels(df$visual_strat)
#' contrasts(df$visual_strat)
#'
#' df <- survey_data |>
#'   filter_manually_identified_ids() |>
#'   factor_strategies(ordered = FALSE)
#' levels(df$visual_strat)
#' contrasts(df$visual_strat)
factor_strategies <- function(df, ordered = TRUE) {
  df_factored <-
    df |>
    dplyr::rename(tidyselect::any_of(c(
      "visual_strat"       = "strats_dlc_q01_visual",
      "verbal_strat"       = "strats_dlc_q02_verbal",
      "spatial_strat"      = "strats_dlc_q03_spatial",
      "semantic_strat"     = "strats_dlc_q04_semantic",
      "sensorimotor_strat" = "strats_dlc_q05_sensorimotor",
      "other_strat"        = "strats_dlc_q06_other",
      "asso_strat_1"       = "strats_dlc_q07_letter_association_1",
      "asso_strat_2"       = "strats_dlc_q07_letter_association_2",
      "asso_strat_3"       = "strats_dlc_q07_letter_association_3"
    ))) |>
    dplyr::mutate(
      dplyr::across(
        "visual_strat":"sensorimotor_strat",
        # ~dplyr::case_when(
        #   . == 1 ~ 1,
        #   . == 2 ~ 2,
        #   . == 3 ~ 3,
        #   . == 4 ~ 4,
        #   . == 5 ~ 5,
        #   . == "no_use" ~ 1,
        #   . == "secondary_strat"  ~ 2,
        #   . == "as_much_as_others"  ~ 3,
        #   . == "mainly_this_strat"  ~ 4,
        #   . == "only_this_strat"  ~ 5,
        #   TRUE ~ 0
        #   ) |>
        ~factor(
          .x,
          levels = c(
            "no_use",
            "secondary_strat",
            "as_much_as_others",
            "mainly_this_strat",
            "only_this_strat"
          ),
          ordered = ordered
        )
      )
    ) |>
    dplyr::relocate("other_strat", .after = "sensorimotor_strat")
  return(df_factored)
}

#' Convert the `group` column to a factor with the desired VVIQ classification
#'
#' @param df A data frame containing a `group` column with VVIQ groups
#' among "Aphantasia", "Hypophantasia", "Typical" and "Hyperphantasia".
#' @param n_groups An integer indicating the number of groups to create, which
#' reflect specific classifications of VVIQ scores: two groups mean grouping
#' Aphants and Hypophants on one end and Typical and Hyperphants on the other,
#' three groups mean separating Aphants, Hypophants and Typical but removing
#' Hyperphants (that have been shown to have distinct characteristics), and four
#' groups mean separating and keeping all four groups. Default is 2.
#' @param contrast_base Optional. An integer indicating the base level for the
#' contrasts. By default, the base level is set to the Typical group for all
#' classifications. This argument allows you to set a different base level
#' (i.e., a different group to compare against for planned comparisons).
#' @param ... Additional arguments passed to [add_factor_contrasts()].
#'
#' @returns A data frame with the `group` column converted to a factor with
#' contrasts added, and the levels set according to the number of groups.
#' @export
#'
#' @examples
#' df <- factor_groups(survey_data, n_groups = 2)
#' contrasts(df$group)
#'
#' df <- factor_groups(survey_data, n_groups = 3)
#' contrasts(df$group)
#'
#' df <- factor_groups(survey_data, n_groups = 4)
#' contrasts(df$group)
#'
#' # Choosing Aphantasia as the reference level for the contrasts
#' # (i.e., for planned comparisons)
#' df <- factor_groups(survey_data, n_groups = 4, contrast_base = 1)
#' contrasts(df$group)
factor_groups <- function(
    df,
    n_groups = 2,
    contrast_base = NULL,
    ...
) {
  if (n_groups == 4) {
    levels <- c("Aphantasia", "Hypophantasia", "Typical","Hyperphantasia")
    n <- c(
      "_aphantasia",
      "_hypophantasia",
      "_typical",
      "_hyperphantasia"
    )
    base <- 3

  } else if (n_groups == 3) {
    levels <- c("Aphantasia", "Hypophantasia", "Typical")
    n <- c(
      "_aphantasia",
      "_hypophantasia",
      "_typical"
    )
    base <- 3

    df <-
      df |>
      dplyr::mutate(
        group = dplyr::case_when(
          .data$group == "hyperphantasia" ~ "typical",
          TRUE ~ .data$group
        )
      )

  } else if (n_groups == 2) {
    levels <- c("Aphantasia", "Typical")
    n <- c(
      "_aphantasia",
      "_typical"
    )
    base <- 2

    df <-
      df |>
      dplyr::mutate(
        group = dplyr::case_when(
          .data$group == "hypophantasia" ~ "aphantasia",
          .data$group == "hyperphantasia" ~ "typical",
          TRUE ~ .data$group
        )
      )

  } else stop("n_groups must be 2, 3 or 4.")

  if (!is.null(contrast_base)) {
    if (!contrast_base %in% seq_along(levels)) {
      stop("contrast_base must be a valid index of the levels.")
    }
    base <- contrast_base
  }

  df_factored <-
    df |>
    dplyr::mutate(
      group = .data$group |>
        stringr::str_to_title() |>
        factor(levels = levels) |>
        add_factor_contrasts(
          n = n,
          base = base,
          ...
        )
    )
  return(df_factored)
}

create_all_groups <- function(df, ...) {
  df_all_groups <-
    df |>
    dplyr::mutate(
      group_2 = dplyr::case_when(
          .data$group == "hypophantasia" ~ "aphantasia",
          .data$group == "hyperphantasia" ~ "typical",
          TRUE ~ .data$group
        ) |>
        stringr::str_to_title() |>
        factor(levels = c("Aphantasia", "Typical")) |>
        add_factor_contrasts(
          n = c(
            "_aphantasia",
            "_typical"
          ),
          base = 2,
          ...
        ),
      group_3 = dplyr::case_when(
          .data$group == "hyperphantasia" ~ "typical",
          TRUE ~ .data$group
        ) |>
        stringr::str_to_title() |>
        factor(levels = c("Aphantasia", "Hypophantasia", "Typical")) |>
        add_factor_contrasts(
          n = c(
            "_aphantasia",
            "_hypophantasia",
            "_typical"
          ),
          base = 3,
          ...
        ),
      group = .data$group |>
        stringr::str_to_title() |>
        factor(
          levels = c("Aphantasia", "Hypophantasia", "Typical","Hyperphantasia")
        ) |>
        add_factor_contrasts(
          n = c(
            "_aphantasia",
            "_hypophantasia",
            "_typical",
            "_hyperphantasia"
          ),
          base = 3,
          ...
        )
    ) |>
    dplyr::relocate(
      "group_2",
      "group_3",
      .after = "group"
    )
  return(df_all_groups)
}
