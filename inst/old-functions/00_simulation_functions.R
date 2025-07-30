#' Simulate data, fit a model on it and test our hypothesis
#'
#' @description
#' This function simulates data based on the parameters provided, fits a model
#' to the simulated data, and extracts the p-values for the visual category
#' effect and the interaction between the aphantasia group and the visual
#' category.
#'
#' @param n_subj_per_group Number of subjects per group. The total number of
#' subjects will be `n_subj_per_group * 2`.
#' @param beta_vis Effect of the visual category on the RTs. This is the main
#' effect of interest.
#' @param method Method to fit the model. Can be one of "lm", "lmer" or "glmer".
#' @param interaction_mult Multiplier for the interaction effect between the
#' aphantasia group and the visual category. This is used to adjust the
#' interaction effect to be more pronounced than the main effect of the visual
#' category.
#' @param p A progressor object from the `progressr` package to update the
#' progress bar during the simulation.
#' @param ... Additional arguments passed to the `simulate_rt_data` function.
#' These can include parameters like `meanlog`, `sdlog`, `shift`, etc.
#'
#' @returns A list containing the p-values for the visual category effect and
#' the interaction between the aphantasia group and the visual category.
#' @export
simulate_rt_tests <- function(
    n_subj_per_group,
    beta_vis,
    method,
    interaction_mult = 1.2,
    p = progressr::progressor(),
    ...
) {
  # Simulate data
  df_rt <- simulate_rt_data(
    n_subj_per_group = n_subj_per_group,
    beta_vis         = beta_vis,
    beta_aph_vis     = -1 * beta_vis * interaction_mult,
    ...
  )

  # Fit the chosen model
  if (method == "lm") {
    model <- lm(rt_total ~ Group * category, data = df_rt)

  } else if (method == "lmer") {
    model <-
      lme4::lmer(rt_total ~ Group * category + (category | id), data = df_rt) |>
      suppressMessages() |>
      suppressWarnings()

  } else if (method == "glmer") {
    model <-
      lme4::glmer(
        formula = rt_total ~ group * category + (category | id),
        data    = df_rt,
        family  = Gamma(link = "identity"),
        control = lme4::glmerControl(optimizer = "bobyqa")
      ) |> suppressMessages() |> suppressWarnings()

  } else {
    stop("method must be one of 'lm', 'lmer' or 'glmer'")
  }

  # Extract the p-values
  p_vis <-
    parameters::parameters(model) |>
    dplyr::filter(Parameter == "categoryvisual") |>
    dplyr::pull(p)
  p_aph_vis <-
    parameters::parameters(model) |>
    dplyr::filter(Parameter == "groupaphantasia:categoryvisual") |>
    dplyr::pull(p)

  # Updating the progressr progress bar
  p()

  return(list(p_vis = p_vis, p_aph_vis = p_aph_vis))
}

#' Conduct a power analysis by simulation
#'
#' @description
#' This function simulates data for a range of parameters, fits a model to each
#' dataset, and extracts the p-values for the visual category effect and the
#' interaction between the aphantasia group and the visual category. It allows
#' the user to specify the range of sample sizes and the range of beta values
#' for the visual category effect. The results can be saved to a file.
#'
#' @param n_min Smallest number of participants per group to simulate. The total
#' number of participants will be `n_min * 2`.
#' @param n_max Largest number of participants per group to simulate. The total
#' number of participants will be `n_max * 2`.
#' @param n_step Step size for the number of participants per group to test.
#' @param beta_vis_min Smallest value for the visual category effect to test.
#' @param beta_vis_max Largest value for the visual category effect to test.
#' @param beta_step Step size for the visual category effect to test.
#' @param interaction_mult Multiplier for the interaction effect between the
#' aphantasia group and the visual category. This is used to adjust the
#' interaction effect to be more pronounced than the main effect of the visual
#' category.
#' @param method Method to fit the model. Can be one of "lm", "lmer" or "glmer".
#' @param n_simulations Number of simulations to run for each combination of
#' parameters. This allows for a more robust estimation of the power.
#' @param filename File to save the results in. If `NULL`, the results are not
#' saved.
#' @param ... Additional arguments passed to the `simulate_rt_tests` function.
#'
#' @returns A dataframe with the results of the power analysis, including the
#' p-values for the visual category effect and the interaction between the
#' aphantasia group and the visual category for each combination of parameters.
#' @export
simulate_rt_power <- function(
    n_min        = 20,
    n_max        = 40,
    n_step       = 10,
    beta_vis_min = 1,
    beta_vis_max = 3,
    beta_step    = 0.5,
    interaction_mult = 1.2,
    method        = "lm",
    n_simulations = 10,
    filename      = NULL,
    ...
) {
  # Verifying the model choice -------------------------------------------------
  if (method == "lm") {
    model_name    <- "linear model"
    time_estimate <- 0.07
  }
  else if (method == "lmer") {
    model_name    <- "linear mixed-effects model"
    time_estimate <- 0.15
  }
  else if (method == "glmer") {
    model_name    <- "generalized linear mixed-effects model"
    time_estimate <- 0.5
  } else stop("method must be one of 'lm', 'lmer' or 'glmer'")

  # Parameters -----------------------------------------------------------------
  n_per_group <- seq(n_min, n_max, n_step)
  betas_vis   <- seq(beta_vis_min, beta_vis_max, beta_step)

  # Number of parameter combinations
  n_combinations <- length(n_per_group) * length(betas_vis)
  n_sims_total   <- n_combinations * n_simulations

  total_time <- n_sims_total * time_estimate + 30
  if (total_time > 60) {
    readable_time <- lubridate::seconds_to_period(round(total_time))
  } else readable_time <- paste0(total_time, " seconds")

  # Checking if the setup is ok for the user -----------------------------------
  # Function to exit smoothly
  stop_quietly <- function() {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }

  choice <-
    menu(
      c("Yes", "No"),
      title = glue::glue_col(
        "With the chosen arguments, you will test {yellow {n_combinations} combinations} of parameters.
        - {n_min} to {n_max} participants, by steps of {n_step}
        - Model parameters from {beta_vis_min} to {beta_vis_max} for the visual category effect, by steps of {beta_step}.

        {yellow {n_simulations} simulations} will be computed for each combination, resulting in
        {red {n_sims_total} datasets} on which you will fit a {red {model_name}}.
        A single {model_name} takes around {time_estimate} seconds to fit on a dataset.
        If we add around 30 seconds to prepare parallelisation...

        {yellow ...Your power analysis is expected to take about {red {readable_time}}}.

        {green Should we proceed?}"
      )
    )
  if (choice == 2 | choice == 0) {
    print(glue::glue_col("\n{green Maybe next time then!}\n\n"))
    stop_quietly()
  }
  else print(glue::glue_col("\n\n{cyan Then all aboard the power train!}\n\n"))

  # Let's go! ------------------------------------------------------------------
  # Parallel setup
  print(glue::glue_col("Setting up parallel processing..."))
  print(glue::glue_col(
    "(I'll start a timer to see if I lied or not on the estimated time.)\n"
  ))
  start_time <- proc.time()[3]
  future::plan(future::multisession)
  print(glue::glue_col("{green Parallel processing all set.\n\n}"))

  # The parameter grid ---------------------------------------------------------
  grid <-
    tidyr::crossing(
      n_subj_per_group = n_per_group,
      beta_vis         = betas_vis,
      method           = method,
      n_simulations    = 1:n_simulations,
      interaction_mult = interaction_mult
    )

  # Simulation -----------------------------------------------------------------
  print(glue::glue_col(
    "{cyan Starting the power analysis by simulation...}\n\n"
  ))

  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(grid))
    simulation_stats <-
      furrr::future_pmap(
        .l = grid,
        .f = simulate_rt_tests,
        p  = p,
        .options = furrr::furrr_options(seed = TRUE)
      )
  })

  print(glue::glue_col(
    "{cyan All done! ---------------------------------------------}\n"
  ))

  elapsed <- proc.time()[3] - start_time
  if (elapsed > 60) elapsed <- lubridate::seconds_to_period(round(elapsed))
  else elapsed <- paste0(round(elapsed, 2), " seconds")
  print(glue::glue_col(
    "Simulations took {elapsed}. Have I lied? See for yourself.\n\n"
  ))

  simulation_results <-
    dplyr::bind_cols(grid, tibble::tibble(statistics = simulation_stats)) |>
    tidyr::unnest_wider(statistics)

  # Saving the results ---------------------------------------------------------
  if (!is.null(filename)) {
    print(glue::glue_col("Saving the results..."))
    try({
      path <- paste0("data/r-data-structures/dlc/", filename)
      saveRDS(simulation_results, file = here::here(path))
      print(glue::glue_col("{green Results saved!}\n\n"))
    })
  } else {
    print(glue::glue_col(
      "{yellow You did not provide a file path, so results were not saved}.
      Don't forget to save them, this probably took a long time to run!\n\n"
    ))
  }

  return(simulation_results)
}

#' Simulate strategies data for all four groups
#'
#' @description
#' This function simulates strategies data for the four groups of phantasia
#' based on the means and standard deviations provided. The data is simulated
#' using the `latent2likert::rlikert()` function, which generates
#' likert-type data based on latent variables. The function allows for
#' customization of the means and standard deviations for each group, as well as
#' the option to generate data in a format suitable for modelling with
#' `faux::add_contrast()`. The default means and standard deviations are based
#' on the findings of Reeder & Pounder (2024) on strategies used by
#' people with different levels of imagery.
#'
#' @param n Number of participants per group.
#' @param means_aph Numeric vector of means for the Aphantasia group.
#' @param means_hypo Numeric vector of means for the Hypophantasia group.
#' @param means_typical Numeric vector of means for the Typical group.
#' @param means_hyper Numeric vector of means for the Hyperphantasia group.
#' @param sd Standard deviation for all groups (default is 0.5).
#' @param modelling_version Logical indicating whether to return the data in a
#' format suitable for modelling with `faux::add_contrast()`. If `TRUE`, the
#' data will be returned with contrasts added for the groups and the
#' variables will be converted to ordered factors. If `FALSE`, the data will be
#' returned in its original format with the group names as character strings.
#' @param seed Optional seed for reproducibility of the random data generation.
#' @param ... Additional arguments passed to the function. Unused.
#'
#' @returns A tibble containing the simulated strategies data for the four
#' groups.
#' @export
simulate_strats_data <- function(
    n,
    means_aph     = c(-2, 1, -1.3, -1.6, -0.7),
    means_hypo    = c(-1.1, 0.1, -1.2, -1.6, -0.1),
    means_typical = c(0.1, 0.1, -1.5, -1.4, -0.3),
    means_hyper   = c(-0.1, -0.2, -1.7, -1.7, -0.9),
    sd = 0.5,
    modelling_version = FALSE,
    seed = NULL,
    ...
) {
  if (!is.null(seed)) set.seed(seed)
  # `faux` has a defect and has to be loaded everytime, otherwise
  # add_contrasts bugs
  pacman::p_load(faux)

  # Function to simulate strategies data for a single group --------------------
  simulate_group <- function(n, group, means = 0, sd = 1) {
    df_strats <-
      latent2likert::rlikert(
        size     = n,
        n_items  = 5,
        n_levels = 5,
        mean     = means,
        sd       = sd
      ) |>
      tibble::as_tibble() |>
      dplyr::rename(
        Visual   = 1,
        Spatial  = 2,
        Verbal   = 3,
        Semantic = 4,
        Sensorimotor = 5
      ) |>
      dplyr::mutate(
        id = paste0("subj_", stringr::str_to_lower(group), "_", 1:n),
        Group = group
      ) |>
      dplyr::relocate(id, Group)

    return(df_strats)
  }

  df_study <-
    dplyr::bind_rows(
      simulate_group(n, "Aphantasia",     means = means_aph,     sd = sd),
      simulate_group(n, "Hypophantasia",  means = means_hypo,    sd = sd),
      simulate_group(n, "Typical",        means = means_typical, sd = sd),
      simulate_group(n, "Hyperphantasia", means = means_hyper,   sd = sd)
    )

  if (modelling_version) {
    df_study <-
      dplyr::bind_rows(
        simulate_group(n, "aphantasia",     means = means_aph,     sd = sd),
        simulate_group(n, "hypophantasia",  means = means_hypo,    sd = sd),
        simulate_group(n, "typical",        means = means_typical, sd = sd),
        simulate_group(n, "hyperphantasia", means = means_hyper,   sd = sd)
      ) |>
      # Adding contrasts
      faux::add_contrast(
        "Group",
        contrast = "treatment",
        levels   = c(
          "typical",
          "aphantasia",
          "hypophantasia",
          "hyperphantasia"
        ),
        colnames = c(
          "_aphantasia",
          "_hypophantasia",
          "_hyperphantasia"
        )
      ) |>
      dplyr::mutate(
        dplyr::across(
          Visual:Sensorimotor,
          ~factor(., ordered = TRUE)
        )
      )
  }

  return(df_study)
}
