#' Build a formula based on the common model for accuracy and RT modelling
#'
#' @param vd Variable of interest, either "accuracy" or "rt_total".
#' @param grouping Grouping variable, e.g., "group_3", "cluster", etc.
#'
#' @returns A formula object for the model.
#' @export
#'
#' @examples
#' build_formula("accuracy", "group")
build_formula <- function(vd, grouping) {
  model_formula <- glue::glue(
    "{vd} ~ {grouping} * category + (category | id) + (1 | problem)"
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
#'
#' @examples
#' set_ranef_prior(100)
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
#'
#' @examples
#' df_expe <- get_clean_data()$df_expe
#'
#' if (require("glmmTMB", quietly = TRUE)) {
#'  model <- glmmTMB::glmmTMB(
#'   formula = accuracy ~ group_2 * category + (1 | id),
#'   data = df_expe,
#'   family = binomial(link = "logit"),
#'   prior = set_ranef_prior(20)
#'  )
#'
#'  get_singularity(model)
#' }
get_singularity <- function(model) {
  rlang::check_installed("performance")

  if (performance::check_singularity(model)) {
    message(
      "The model is singular, estimates should be interpreted with caution.\n"
    )
  } else {
    message("The model is not singular, parameter estimates are trustworthy.\n")
  }
}

#' Get performance indices for a model in a clean format
#'
#' @param model A fitted model object.
#' @param ... Additional arguments passed to `performance::model_performance()`.
#' @param metrics Can be "all", "common" or a character vector of metrics to be
#' computed, see `?performance::model_performance()` for details.
#' @param verbose Toggle warnings on or off. Default is FALSE.
#'
#' @returns A formatted data frame with the performance indices of the model.
#' @export
#'
#' @examples
#' df_expe <- get_clean_data()$df_expe
#'
#' if (require("glmmTMB", quietly = TRUE)) {
#'  model <- glmmTMB::glmmTMB(
#'   formula = accuracy ~ group_2 * category + (1 | id),
#'   data = df_expe,
#'   family = binomial(link = "logit"),
#'   prior = set_ranef_prior(20)
#'  )
#'
#'  get_performance(model)
#' }
get_performance <- function(
    model,
    metrics = "common",
    verbose = FALSE,
    ...
) {
  rlang::check_installed("performance")

  performance::model_performance(
    model,
    metrics = metrics,
    verbose = verbose,
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
#'
#' @examples
#' df_expe <- get_clean_data()$df_expe
#'
#' if (require("glmmTMB", quietly = TRUE)) {
#'  model <- glmmTMB::glmmTMB(
#'   formula = accuracy ~ group_2 * category + (1 | id),
#'   data = df_expe,
#'   family = binomial(link = "logit"),
#'   prior = set_ranef_prior(65)
#'  )
#'
#'  get_params(model)
#' }
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
#' @param type Type of response to be returned. Default is "response".
#' @param at Optional. A list of values at which to evaluate the contrasts.
#' @param method Method for computing the contrasts. Default is "revpairwise".
#' @param ... Additional arguments passed to `emmeans::contrast()`.
#'
#' @returns An emm_grid object with the pairwise contrasts of the specified
#' variables.
#' @export
#'
#' @examples
#' df_expe <- get_clean_data()$df_expe
#'
#' if (require("glmmTMB", quietly = TRUE)) {
#'  model <- glmmTMB::glmmTMB(
#'   formula = accuracy ~ group_2 * category + (1 | id),
#'   data = df_expe,
#'   family = binomial(link = "logit"),
#'   prior = set_ranef_prior(65)
#'  )
#'
#'  get_contrast(model, ~ category | group_2)
#' }
get_contrast <- function(
    model,
    formula,
    type = "response",
    at = NULL,
    method = "revpairwise",
    ...
) {
  rlang::check_installed("emmeans")

  emm_contrast <-
    model |>
    emmeans::emmeans(formula, type = type, at = at) |>
    emmeans::contrast(method = method,  ...)

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
    tidyr::unite("CI", "asymp.LCL", "asymp.UCL", sep = ", ") |>
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
#'
#' @examples
#' df_expe <- get_clean_data()$df_expe
#'
#' if (require("glmmTMB", quietly = TRUE)) {
#'  model <- glmmTMB::glmmTMB(
#'   formula = accuracy ~ group_2 * category + (1 | id),
#'   data = df_expe,
#'   family = binomial(link = "logit"),
#'   prior = set_ranef_prior(20)
#'  )
#'
#'  report_contrast(model, ~ category | group_2)
#' }
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
    dplyr::relocate("95% CI", .before = "p.value") |>
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
#'
#' @examples
#' df_strats <- get_clean_data()$df_survey |> pivot_strategies_longer()
#'
#' model <- fit_clm(score ~ group_2 * strategy, df_strats)
#' report_contrast(model, ~ group_2 | strategy)
fit_clm <- function(formula, data, link = "probit") {
  rlang::check_installed("ordinal")

  fit <- ordinal::clm(formula = formula, data = data, link = link)
  return(fit)
}

#' Fit a Bayesian model using the brms package with default settings
#'
#' @param ... Arguments passed to brms::brm(), such as formula, data, family,
#' priors, etc.
#' @param iterations Total number of iterations. This number is divided by the
#' number of cores for parallel processing. Default is 20000 (40k recommended
#' if Bayes Factors are needed).
#' @param warmup Number of warmup iterations added for each chain. Default is
#' 2000.
#' @param refresh Frequency of progress updates. Default is 500.
#' @param backend Backend to use for fitting the model. Default is "cmdstanr".
#' @param file_refit Condition for refitting the model. Default is "on_change".
#' @param file_compress Compression method for saving the model file. Default is
#' "xz".
#' @param sample_prior Logical. If TRUE, prior samples are drawn. If "only",
#' only prior samples are drawn. Default is FALSE.
#' FALSE
#' @param save_pars Parameters to save. Default is NULL.
#' @param adapt_delta Target acceptance rate for the NUTS sampler. Default is
#' 0.95.
#' @param seed Random seed for reproducibility. Default is 667.
#'
#' @returns A fitted brms model object.
#' @export
fit_brms_model <- function(
    ...,
    iterations = 24000,   # 40k recommended if BFs needed
    warmup = 2000,
    refresh = 500,
    backend = "cmdstanr",
    file_refit = "on_change",
    file_compress = "xz",
    sample_prior = FALSE, # TRUE if BFs needed
    save_pars = NULL,     # brms::save_pars(all = TRUE) if BFs needed
    adapt_delta = 0.95,
    seed = 667
) {
  # Set the folder to save the cmdstanr parameters
  options(cmdstanr_write_stan_file_dir = "models/stan/")

  # Create a folder for the models if necessary
  fs::dir_create("models/")

  # Parallel processing setup for 40k samples
  n_cores <- parallel::detectCores()
  n_iter  <- ceiling(iterations / n_cores) + warmup

  # Fit a brms model with the arguments in `...` and my default options
  brms::brm(
    ...,
    chains  = n_cores,
    cores   = n_cores,
    iter    = n_iter,
    warmup  = warmup,
    refresh = refresh,
    backend = backend,
    file_refit    = file_refit,
    file_compress = file_compress,
    sample_prior  = sample_prior,
    save_pars     = save_pars,
    control       = list(adapt_delta = adapt_delta),
    seed = seed
  )
}


#' Report the ROPE analysis for marginal effects
#'
#' @param marg_effects A marginaleffects object obtained with
#' [marginaleffects::avg_comparisons()] containing the contrasts to analyse.
#' @param ... Grouping variables for summarising the ROPE results.
#' @param digits Number of decimal places to round the results. Default is 3.
#'
#' @returns A data frame summarising the ROPE analysis with the estimates,
#' 95% CIs, and proportions of draws within, below, and above the ROPE.
#' @export
report_rope <- function(
    marg_effects,
    ...,
    digits = 3
) {
  rlang::check_installed("bayestestR", reason = "to compute ROPE ranges")
  rlang::check_installed("marginaleffects", reason = "to extract draws")

  range <- bayestestR::rope_range(attr(marg_effects, "marginaleffects")@model)

  rope_report <-
    marg_effects |>
    marginaleffects::posterior_draws() |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      Estimate = unique(.data$estimate) |> round(digits),
      "95% CI" = paste0(
        "[",
        round(unique(.data$conf.low), digits),
        ", ",
        round(unique(.data$conf.high), digits),
        "]"
      ),
      pd = bayestestR::p_direction(.data$draw)$pd |> round(digits),
      "In ROPE" =
        mean(.data$draw > range[1] & .data$draw < range[2]) |> round(digits),
      "< ROPE" = mean(.data$draw < range[1]) |> round(digits),
      "> ROPE" = mean(.data$draw > range[2]) |> round(digits)
    ) |>
    dplyr::ungroup()

  # If there is a "hypothesis" column, that means we are dealing with
  # interaction contrasts, and more formatting is needed to keep only the
  # contrasts of interest. This is very "experiment-specific" and tailored to
  # our design, so it may need to be adapted/removed for other uses.
  if ("hypothesis" %in% colnames(rope_report)) {
    rope_report <-
      rope_report |>
      tidyr::separate_wider_delim(
        .data$hypothesis, delim = ") - (",
        names = c("level_1", "level_2")
      ) |>
      dplyr::mutate(
        dplyr::across(
          c("level_1", "level_2"),
          ~ stringr::str_remove_all(., stringr::fixed("(")) |>
            stringr::str_remove_all(stringr::fixed(")"))
        )
      ) |>
      tidyr::separate_wider_delim(
        c("level_1", "level_2"), delim = " - ",
        names = c("cat_1", "cat_2"), names_sep = "_"
      ) |>
      tidyr::separate_wider_delim(
        tidyselect::contains("cat_2"), delim = " ",
        names = c("cat", "group"), names_sep = "_"
      ) |>
      dplyr::filter(
        .data$level_1_cat_1 == .data$level_2_cat_1 &
          .data$level_1_cat_2_cat == .data$level_2_cat_2_cat
      ) |>
      dplyr::mutate(
        dplyr::across(tidyselect::where(is.numeric), ~ round(., digits))
      ) |>
      tidyr::unite(
        "Category contrast",
        "level_1_cat_1":"level_1_cat_2_cat",
        sep = " - "
      ) |>
      tidyr::unite(
        "Grouping contrast",
        c("level_1_cat_2_group","level_2_cat_2_group"),
        sep = " - "
      ) |>
      dplyr::select(!c("level_2_cat_1", "level_2_cat_2_cat")) |>
      dplyr::arrange(`Grouping contrast`)
  }

  return(rope_report)
}
