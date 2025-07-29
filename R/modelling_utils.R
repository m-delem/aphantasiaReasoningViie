#' Build a formula based on the common model for accuracy and RT modelling
#'
#' @param vd Variable of interest, either "accuracy" or "rt_total".
#' @param grouping Grouping variable, typically "Group" or "cluster".
#'
#' @returns A formula object for the model.
#' @export
build_formula <- function(vd, grouping) {
  model_formula <- glue::glue(
    "{vd} ~ {grouping} * category + (category | id) + ({grouping} | problem)"
  ) |> formula()
  return(model_formula)
}

#' Create a weakly informative regularizing Gamma prior for the random effects
#'
#' @param gamma_mean A numeric value indicating the mean of the Gamma prior. The
#' higher the value, the less impact the prior has, but also the more chance of
#' singularity.
#'
#' @returns A data frame with the prior and class for the random effects.
#' @export
set_ranef_prior <- function(gamma_mean = 100) {
  return(
    data.frame(
      prior = glue::glue("gamma({gamma_mean}, 2.5)"),
      class = "ranef"
    )
  )
}

#' Check if the model is singular and print a message
#'
#' @param model A fitted model object.
#'
#' @returns Nothing. Prints a message indicating whether the model is singular
#' or not.
#' @export
get_singularity <- function(model) {
  rlang::check_installed("performance")

  if (performance::check_singularity(model)) {
    cat(
      "The model is singular, estimates should be interpreted with caution.\n"
    )
  } else {
    cat("The model is not singular, parameter estimates are trustworthy.\n")
  }
}

#' Get performance indices for a model in a clean format
#'
#' @param model A fitted model object.
#' @param ... Additional arguments passed to `performance::model_performance()`.
#'
#' @returns A formatted data frame with the performance indices of the model.
#' @export
get_performance <- function(model, ...) {
  rlang::check_installed("performance")

  performance::model_performance(
    model,
    metrics = "common",
    verbose = FALSE,
    ...
    ) |>
    format(digits = 3)
}

#' Get the fixed parameters of a model in a clean format
#'
#' @param model A fitted model object.
#' @param ... Additional arguments passed to `parameters::model_parameters()`.
#'
#' @returns A formatted data frame with the fixed parameters of the model.
#' @export
get_params <- function(model, ...) {
  rlang::check_installed("parameters")

  parameters::model_parameters(
    model,
    effects = "fixed",
    verbose = FALSE,
    ...
  ) |>
    format() |>
    dplyr::select(!c(
      tidyselect::contains("SE"),
      tidyselect::contains("df"),
      tidyselect::contains("z")
    ))
}

#' Get the pairwise contrasts of variables in a model
#'
#' @param model A fitted model object.
#' @param formula A formula specifying the variables for which to get the
#' contrasts. See `?emmeans::emmeans` for details.
#' @param at Optional. A list of values at which to evaluate the contrasts.
#' @param ... Additional arguments passed to `emmeans::contrast()`.
#'
#' @returns An emm_grid object with the pairwise contrasts of the specified
#' variables.
#' @export
get_contrast <- function(model, formula, at = NULL, ...) {
  rlang::check_installed("emmeans")

  emm_contrast <-
    model |>
    emmeans::emmeans(formula, type = "response", at = at) |>
    emmeans::contrast(method = "pairwise",  ...)

  return(emm_contrast)
}

#' Extract and format the confidence interval of an emmeans object
#'
#' @param emm_object An emm_grid object from which to extract the confidence
#' interval.
#'
#' @returns A data frame with the confidence interval formatted as a string.
#' @export
#'
#' @keywords internal
extract_emm_confint <- function(emm_object) {
  ci <-
    confint(emm_object) |>
    as.data.frame() |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~round(., 2))) |>
    tidyr::unite("CI", .data$asymp.LCL, .data$asymp.UCL, sep = ", ") |>
    dplyr::mutate(CI = paste0("[", .data$CI, "]")) |>
    dplyr::select("CI")
  return(ci)
}

#' Get the contrasts of a model and format them for reporting
#'
#' @param model A fitted model object.
#' @param formula A formula specifying the variables for which to get the
#' contrasts. See `?emmeans::emmeans` for details.
#' @param ... Additional arguments passed to `get_contrast()`.
#'
#' @returns A data frame with the pairwise contrasts of the specified variables
#' formatted for reporting.
#' @export
report_contrast <- function(model, formula, ...) {
  emm_contrast <- get_contrast(model, formula, ...)

  emm_contrast |>
    as.data.frame() |>
    dplyr::select(!c(
      tidyselect::contains("SE", ignore.case = FALSE),
      tidyselect::contains("df"),
      tidyselect::contains("null"),
      tidyselect::contains("z.ratio")
    )) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.factor),
        ~ . |>
          stringr::str_remove_all("_strat") |>
          stringr::str_to_title()
      ),
      dplyr::across(
        tidyselect::where(is.numeric),
        ~round(., 3)
      ),
      `95% CI` = extract_emm_confint(emm_contrast)
    ) |>
    dplyr::relocate(.data$`95% CI`, .before = "p.value") |>
    dplyr::rename(tidyselect::any_of(c(
      Contrast            = "contrast",
      Group               = "group",
      Strategy            = "strategy",
      `Category contrast` = "category_pairwise",
      `Strategy contrast` = "strategy_pairwise",
      `Group contrast`    = "group_pairwise",
      `Cluster contrast`  = "cluster_pairwise",
      `Cluster`           = "cluster",
      `Odds ratio`        = "odds.ratio",
      `Difference`        = "estimate"
    )))
}

#' Fit a cumulative link model (CLM) using the ordinal package
#'
#' @param formula A formula specifying the model to fit.
#' @param data A data frame containing the data to fit the model.
#' @param link A string specifying the link function to use. Default is
#' "probit".
#'
#' @returns A fitted clm object from the ordinal package.
#' @export
fit_clm <- function(formula, data, link = "probit") {
  rlang::check_installed("ordinal")

  fit <- ordinal::clm(formula = formula, data = data, link = link)
  return(fit)
}
