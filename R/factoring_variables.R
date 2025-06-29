#' Add treatment contrasts to a factor
#'
#' @param vect A factor vector to which contrasts will be added.
#' @param n A vector of levels for the factor. Default is the levels of `vect`.
#' @param ... Additional arguments passed to `contr.treatment`.
#'
#' @returns A factor vector with treatment contrasts added.
#' @export
add_factor_contrasts <- function(vect, n = levels(vect), ...) {
  contrasts(vect) <- contr.treatment(n = n, ...)
  return(vect)
}

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

factor_categories <- function(
    df,
    n = c("_control", "_spatial", "_visual"),
    base = 3,
    ...
) {
  df_factored <-
    df |>
    dplyr::mutate(
      category = category |>
        stringr::str_to_title() |>
        as.factor() |>
        add_factor_contrasts(n = n, base = base, ...)
    )
  return(df_factored)
}

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
        ~dplyr::case_when(
          . == 1 ~ 1,
          . == 2 ~ 2,
          . == 3 ~ 3,
          . == 4 ~ 4,
          . == 5 ~ 5,
          . == "no_use" ~ 1,
          . == "secondary_strat"  ~ 2,
          . == "as_much_as_others"  ~ 3,
          . == "mainly_this_strat"  ~ 4,
          . == "only_this_strat"  ~ 5,
          TRUE ~ 0
          ) |>
          factor(ordered = ordered)
      )
    ) |>
    dplyr::relocate("other_strat", .after = "sensorimotor_strat")
  return(df_factored)
}

factor_groups <- function(
    df,
    n_groups = 2,
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
          group == "hyperphantasia" ~ "typical",
          TRUE ~ group
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
          group == "hypophantasia" ~ "aphantasia",
          group == "hyperphantasia" ~ "typical",
          TRUE ~ group
        )
      )

  } else stop("n_groups must be 2, 3 or 4.")

  df_factored <-
    df |>
    dplyr::mutate(
      group = group |>
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
