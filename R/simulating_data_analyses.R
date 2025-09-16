#' Simulate skewed RT data for the factorial design
#'
#' @description
#' This function simulates response time data for the factorial design of the
#' experiment. It was used for power analyses by simulation.
#'
#' Inspired by [Chris Jungerius](https://cjungerius.github.io/powersim/).
#'
#' @param n_subj_per_group Number of subjects per group. The total number of
#' subjects will be `n_subj_per_group * 2`.
#' @param meanlog Mean of the log-normal distribution for the base RT values.
#' @param sdlog Standard deviation of the log-normal distribution for the base
#' RT values.
#' @param shift Non-decision time, i.e., the minimum value of the RTs.
#' @param tau_0 By-subject random intercept standard deviation.
#' @param tau_vis By-subject random slope standard deviation for the visual
#' category.
#' @param tau_spa By-subject random slope standard deviation for the spatial
#' category.
#' @param beta_aph Effect of the aphantasia group on the RTs.
#' @param beta_vis Effect of the visual category on the RTs.
#' @param beta_spa Effect of the spatial category on the RTs.
#' @param beta_aph_vis Effect of the interaction between the aphantasia group
#' and the visual category on the RTs.
#' @param beta_aph_spa Effect of the interaction between the aphantasia group
#' and the spatial category on the RTs.
#' @param seed Random seed for reproducibility. If `NULL`, no seed is set.
#' @param ... Additional arguments passed to the function. Unused.
#'
#' @returns A dataframe with the simulated RT data.
#' @export
#'
#' @examples
#' # No main effects
#' df <- simulate_rt_data(100)
#' head(df)
#'
#' df |>
#'   dplyr::group_by(group, category) |>
#'   dplyr::reframe(
#'     mean_rt = mean(rt_total),
#'     median_rt = median(rt_total),
#'     sd_rt = sd(rt_total),
#'     min_rt = min(rt_total),
#'     max_rt = max(rt_total)
#'   )
#'
#' # Visual category effect around 2.5s
#' df <- simulate_rt_data(100, beta_vis = 2.5)
#' df |>
#'   dplyr::group_by(group, category) |>
#'   dplyr::reframe(
#'     mean_rt = mean(rt_total),
#'     median_rt = median(rt_total),
#'     sd_rt = sd(rt_total),
#'     min_rt = min(rt_total),
#'     max_rt = max(rt_total)
#'   )
simulate_rt_data <- function(
    n_subj_per_group,
    meanlog  = 2.1,
    sdlog    = 0.55,
    shift    = 5,
    # Group-level (varying) effects by-subject
    tau_0    = 0.9,
    tau_vis  = 0.75,
    tau_spa  = 0,
    # Population-level (overall) effects
    beta_aph = 0,
    beta_vis = 0,
    beta_spa = 0,
    beta_aph_vis = 0,
    beta_aph_spa = 0,
    seed = NULL,
    ...
) {
  rlang::check_installed("brms")
  rlang::check_installed("faux")

  # Set the random seed if provided
  if (!is.null(seed)) set.seed(seed)

  df_rt <-
    faux::add_random(id = n_subj_per_group * 2) |>
    # Each subject is assigned to a group
    faux::add_between(
      .by = "id",
      group = c("Aphantasia", "Typical")
    ) |>
    # Each subject has trials in the three categories
    faux::add_within(
      .by = "id",
      category = c("Visual", "Spatial", "Control")
    ) |>
    # Each category comprises 9 trials
    dplyr::mutate(trial = list(1:9)) |>
    tidyr::unnest_longer("trial") |>
    # Contrast coding the columns for the models
    factor_categories() |>
    factor_groups(n_groups = 2) |>
    # We add a random intercept and slope to each participant
    faux::add_ranef(
      "id",
      tau_0   = tau_0,
      tau_vis = tau_vis,
      tau_spa = tau_spa
    ) |>
    # Simulate the RT data based on the parameters
    dplyr::mutate(
      # The base RT value follows a shifted log-normal distribution
      beta_0 = brms::rshifted_lnorm(dplyr::n(), meanlog, sdlog, shift),
      aphantasia = ifelse(.data$group == "Aphantasia", 1, 0),
      visual  = ifelse(.data$category == "Visual", 1, 0),
      spatial = ifelse(.data$category == "Spatial", 1, 0),
      rt_total = .data$beta_0 + .data$tau_0 +
        beta_aph * .data$aphantasia +
        (beta_vis + .data$tau_vis) * .data$visual +
        (beta_spa + .data$tau_spa) * .data$spatial +
        beta_aph_vis * .data$aphantasia * .data$visual +
        beta_aph_spa * .data$aphantasia * .data$spatial
    ) |>
    dplyr::relocate("rt_total", .after = "category")

  return(df_rt)
}

#' Simulate data, fit a model on it and test our hypothesis
#'
#' @description
#' This function simulates data based on the parameters provided, fits a model
#' to the simulated data, and extracts the p-value for the group x
#' control-visual interaction contrast (our theoretical contrast of interest).
#' This function is used repeatedly in a power analysis by simulation.
#'
#' @param n_subj_per_group Number of subjects per group. The total number of
#' subjects will be `n_subj_per_group * 2`.
#' @param beta_vis Effect of the visual category on the RTs. This is used to
#' create the main effect of interest by adding this effect to the typical
#' group only (thus creating an interaction between group and category).
#' @param p A progressor object from the `progressr` package to update the
#' progress bar during the simulation.
#' @param ... Additional arguments passed to the `simulate_rt_data` function.
#' These can include parameters like `meanlog`, `sdlog`, `shift`, etc.
#'
#' @returns The p-value for the group x control-visual interaction contrast.
#' @export
#'
#' @examples
#' simulate_rt_test(10, 1)   # Low power, unlikely to detect the effect
#' simulate_rt_test(40, 1.5) # Better power, more likely
#' simulate_rt_test(150, 1)  # High power, highly likely even for a small effect
simulate_rt_test <- function(
    n_subj_per_group,
    beta_vis,
    p = progressr::progressor(),
    ...
) {
  rlang::check_installed("glmmTMB")
  rlang::check_installed("progressr")

  df <-
    simulate_rt_data(
      n_subj_per_group = n_subj_per_group,
      beta_vis = beta_vis,          # Visual category effect
      beta_aph_vis = -1 * beta_vis, # group x visual interaction
      ...
    )

  model <-
    glmmTMB::glmmTMB(
      rt_total ~ group * category + (category | id),
      data = df,
      family = stats::Gamma(link = "identity"),
      prior   = set_ranef_prior()
    ) |> suppressMessages() |> suppressWarnings()

  contrasts <- report_contrast(model, ~ group * category, interaction = TRUE)

  # Updating the progressr progress bar (for the power analysis function)
  p()

  return(contrasts$p.value[2])
}

#' Conduct a power analysis by simulation
#'
#' @description
#' This function simulates data for a range of parameters, fits a model to each
#' dataset, and extracts the p-values for the interaction contrast between the
#' group and the control-visual difference. It allows the user to specify the
#' range of sample sizes and the range of beta values for the visual category
#' effect size. This function is interactive, so try it out in the console to
#' see a breakdown of the parameters you chose and an estimate of the time
#' it will take to run the analysis.
#'
#' @param n_min Smallest number of participants per group to simulate. The total
#' number of participants will be `n_min * 2`.
#' @param n_max Largest number of participants per group to simulate. The total
#' number of participants will be `n_max * 2`.
#' @param n_step Step size for the number of participants per group to test.
#' @param beta_vis_min Smallest value for the visual category effect to test.
#' @param beta_vis_max Largest value for the visual category effect to test.
#' @param beta_step Step size for the visual category effect to test.
#' @param n_simulations Number of simulations to run for each combination of
#' parameters. This allows for a more robust estimation of the power.
#' @param time_estimate Estimated time (in seconds) to fit a single model on a
#' single dataset. This is used to estimate the total time the power analysis
#' will take.
#' @param test_skip Optional argument for testing purposes. See
#' [this Stack thread](https://stackoverflow.com/questions/65740390/how-to-test-a-function-that-depends-on-a-menu-user-input) about testing functions with
#' [utils::menu()] inputs.
#'
#' @returns A data frame with the results of the power analysis, including
#' the parameters used and the p-values obtained for each simulation.
#' @export
run_power_analysis <- function(
    n_min        = 20,
    n_max        = 40,
    n_step       = 10,
    beta_vis_min = 1,
    beta_vis_max = 3,
    beta_step    = 0.5,
    n_simulations = 10,
    time_estimate = 0.1,
    test_skip    = NA
) {
  rlang::check_installed("lubridate")
  rlang::check_installed("future")
  rlang::check_installed("furrr")
  rlang::check_installed("progressr")

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

  if (is.na(test_skip)) {
    choice <-
      utils::menu(
        c("Yes", "No"),
        title = glue::glue_col(
          "With the chosen arguments, you will test {yellow {n_combinations} combinations} of parameters.
          - {n_min} to {n_max} participants, by steps of {n_step}
          - Model parameters from {beta_vis_min} to {beta_vis_max} for the visual category effect, by steps of {beta_step}.

          {yellow {n_simulations} simulations} will be computed for each combination, resulting in
          {red {n_sims_total} datasets} on which you will fit a GLMM.
          A single GLMM takes around {time_estimate} seconds to fit on a dataset.
          If we add around 30 seconds to prepare parallelisation...

          {yellow ...Your power analysis is expected to take about {red {readable_time}}}.

          {green Should we proceed?}"
        )
      )
  } else if (test_skip == 0) {
    choice <- 0
  } else choice <- 1

  if (choice == 0 | choice == 2) {
    message(glue::glue_col("\n{green Maybe next time then!}\n\n"))
    stop_quietly()
  } else {
    message(glue::glue_col("\n\n{cyan Then all aboard the power train!}\n\n"))
  }

  # Let's go! ------------------------------------------------------------------
  # Parallel setup
  message(glue::glue_col("Setting up parallel processing..."))
  message(glue::glue_col(
    "(I'll start a timer to see if I lied or not on the estimated time.)\n"
  ))
  start_time <- proc.time()[3]
  future::plan(future::multisession)
  message(glue::glue_col("{green Parallel processing all set.\n\n}"))

  # The parameter grid ---------------------------------------------------------
  grid <-
    tidyr::crossing(
      n_subj_per_group = n_per_group,
      beta_vis         = betas_vis,
      n_simulations    = 1:n_simulations
    )

  # Simulation -----------------------------------------------------------------
  message(glue::glue_col(
    "{cyan Starting the power analysis by simulation...}\n\n"
  ))

  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(grid))
    p_values <-
      furrr::future_pmap(
        .l = grid,
        .f = simulate_rt_test,
        p  = p,
        .options = furrr::furrr_options(seed = TRUE)
      )
  })

  message(glue::glue_col(
    "{cyan All done! ---------------------------------------------}\n"
  ))

  elapsed <- proc.time()[3] - start_time
  if (elapsed > 60) elapsed <- lubridate::seconds_to_period(round(elapsed))
  else elapsed <- paste0(round(elapsed, 2), " seconds")
  message(glue::glue_col(
    "Simulations took {elapsed}. Have I lied? See for yourself.\n\n"
  ))

  simulation_results <-
    dplyr::bind_cols(grid, tibble::tibble(p_value = p_values)) |>
    tidyr::unnest("p_value")

  return(simulation_results)
}

#' Plot the results of a power analysis by simulation
#'
#' @description
#' This function plots the results of a power analysis by simulation. It takes
#' a data frame similar to those output by the `run_power_analysis()` function.
#'
#' @param power_results A data frame with the results of the power analysis by
#' simulation.
#' @param threshold The significance threshold to use for calculating power.
#' Default is 0.05.
#' @param ... Additional arguments passed to the `theme_pdf()` function for
#' customizing the plot theme.
#'
#' @returns A ggplot2 object with the power curves.
#' @export
#'
#' @examples
#' plot_power(power_results) # power_results is natively in the package
plot_power <- function(power_results, threshold = 0.05, ...) {
  p <-
    power_results |>
    dplyr::mutate(beta_vis = as.factor(.data$beta_vis)) |>
    dplyr::group_by(.data$n_subj_per_group, .data$beta_vis) |>
    dplyr::reframe(power = sum(.data$p_value <= threshold) / dplyr::n()) |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$n_subj_per_group,
      y = .data$power,
      color = .data$beta_vis,
      group = .data$beta_vis
    )) +
    ggplot2::geom_line(
      linewidth = 0.3,
      alpha = 0.3,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      size = 0.75,
      alpha = 0.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_smooth(
      linewidth = 0.35,
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      span = 0.4
    ) +
    ggplot2::geom_hline(
      linewidth = 0.35,
      yintercept = 0.8,
      linetype = "dashed",
      color = "grey"
    ) +
    ggplot2::geom_hline(
      linewidth = 0.35,
      yintercept = 0.9,
      linetype = "dashed",
      color = "red"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(0.025),
      breaks = scales::breaks_pretty(20)
    ) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(10)) +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::labs(
      x = "Number of subjects per group",
      y = "Power",
      color = "Effect size (in seconds) "
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1)) +
    theme_pdf(
      base_theme = ggplot2::theme_minimal,
      # Custom
      panel.grid.minor      = ggplot2::element_blank(),
      legend.text.position  = "top",
      legend.text           = ggplot2::element_text(
        size = 5,
        margin = ggplot2::margin(b = -1)
      ),
      legend.key.spacing.x  = grid::unit(1, "mm"),
      ...
    )

  return(p)
}

#' Simulate accuracy data for the factorial design
#'
#' @description
#' This function simulates accuracy data for the factorial design of the
#' experiment.
#'
#' It was designed for potential power analyses on accuracy data, but
#' it ended up being unused due to a lack of previous data to base the analyses
#' on. The power analyses were conducted on RT data instead.
#'
#' It creates a data frame with the following columns:
#' - `id`: A unique identifier for each subject.
#' - `Group`: The group to which the subject belongs (aphantasia or typical).
#' - `category`: The category of the trial (visual, spatial, or control).
#' - `trial`: The trial number (1 to 9).
#' - `accuracy`: The accuracy of the subject's response (1 for correct, 0 for
#' incorrect).
#' Inspired by [Lisa DeBruine](https://debruine.github.io/lmem_sim/articles/appendix3a_binomial.html).
#'
#' @param n_subj_per_group The number of subjects per group.
#' @param beta_0 The intercept for the model.
#' @param tau_0  The standard deviation of the random intercept for each
#' subject.
#' @param tau_vis The standard deviation of the random slope for the visual
#' category.
#' @param tau_spa The standard deviation of the random slope for the spatial
#' category.
#' @param beta_aph The fixed effect of the aphantasia group.
#' @param beta_vis The fixed effect of the visual category.
#' @param beta_spa The fixed effect of the spatial category.
#' @param beta_aph_vis The interaction between the aphantasia group and the
#' visual category.
#' @param beta_aph_spa The interaction between the aphantasia group and the
#' spatial category.
#' @param seed The seed for random number generation. If NULL, the seed is not
#' set.
#' @param ... Additional arguments passed to the function. Unused.
#'
#' @returns A data frame with the simulated accuracy data.
#' @export
#' @keywords internal
#'
#' @examples
#' df <- simulate_acc_data(100)
#' head(df)
#'
#' df |>
#'   dplyr::group_by(group, category) |>
#'   dplyr::reframe(
#'     mean_acc = mean(accuracy),
#'     median_acc = median(accuracy),
#'     sd_acc = sd(accuracy)
#'     )
simulate_acc_data <- function(
    n_subj_per_group,
    beta_0   = 1.36, # Intercept
    # Varying effects by-subject
    tau_0    = 0.05, # By-subject random intercept sd
    tau_vis  = 0.01, # By-subject random slope sd for visual category
    tau_spa  = 0.01, # By-subject random slope sd for spatial category
    # Fixed effects
    beta_aph = 0,    # Effect of aphantasia group
    beta_vis = 0,    # Effect of visual category
    beta_spa = 0,    # Effect of spatial category
    beta_aph_vis = 0, # Interaction between group and visual
    beta_aph_spa = 0, # Interaction between group and spatial
    seed = NULL,
    ...
) {
  rlang::check_installed("faux")

  # Set the random seed if provided
  if (!is.null(seed)) set.seed(seed)

  df_acc <-
    faux::add_random(id = n_subj_per_group * 2) |>
    # Each subject is assigned to a group
    faux::add_between(
      .by = "id",
      group = c("Aphantasia", "Typical")
    ) |>
    # Each subject has trials in the three categories
    faux::add_within(
      .by = "id",
      category = c("Visual", "Spatial", "Control")
    ) |>
    # Each category comprises 9 trials
    dplyr::mutate(trial = list(1:9)) |>
    tidyr::unnest_longer("trial") |>
    # Contrast coding the columns for the models
    factor_categories() |>
    factor_groups(n_groups = 2) |>
    # We add a random intercept and slope to each participant
    faux::add_ranef(
      "id",
      tau_0   = tau_0,
      tau_vis = tau_vis,
      tau_spa = tau_spa
    ) |>
    dplyr::mutate(
      aphantasia = ifelse(.data$group == "Aphantasia", 1, 0),
      visual  = ifelse(.data$category == "Visual", 1, 0),
      spatial = ifelse(.data$category == "Spatial", 1, 0),
      Y = beta_0 + .data$tau_0 +
        beta_aph * .data$aphantasia +
        (beta_vis + .data$tau_vis) * .data$visual +
        (beta_spa + .data$tau_spa) * .data$spatial +
        beta_aph_vis * .data$aphantasia * .data$visual +
        beta_aph_spa * .data$aphantasia * .data$spatial,
      # converting to probability of getting 1 with inverse logit
      pr = 1 / (1 + exp(-.data$Y)),
      # sampling from Bernoulli distribution
      accuracy = rbinom(dplyr::n(), 1, .data$pr)
    ) |>
    dplyr::mutate(
      n_correct = sum(.data$accuracy, na.rm = TRUE),
      n_trials  = dplyr::n(),
      mean_acc  = mean(.data$accuracy) |> round(2),
      .by = c("id", "group", "category")
    ) |>
    dplyr::relocate("accuracy", .after = "category")

  return(df_acc)
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
#' It was designed for potential power analyses on strategies data, but
#' it ended up being unused due to a lack of time and previous data to build
#' upon. The power analyses were conducted on RT data instead.
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
#' @keywords internal
#'
#' @examples
#' df <- simulate_strats_data(n = 100, seed = 123)
#' head(df)
#'
#' df |>
#'  dplyr::group_by(group) |>
#'  dplyr::reframe(dplyr::across(tidyselect::contains("_strat"), mean))
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
  rlang::check_installed("faux")
  rlang::check_installed("latent2likert")

  # Set the random seed if provided
  if (!is.null(seed)) set.seed(seed)

  # Function to simulate strategies data for a single group
  simulate_group <- function(n, group_name, means = 0, sd = 1) {
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
        "visual_strat"  = 1,
        "spatial_strat" = 2,
        "verbal_strat"  = 3,
        "semantic_strat" = 4,
        "sensorimotor_strat" = 5
      ) |>
      dplyr::mutate(
        id = paste0("subj_", stringr::str_to_lower(group_name), "_", 1:n),
        group = group_name
      ) |>
      dplyr::relocate("id", "group")

    return(df_strats)
  }

  df_study <-
    dplyr::bind_rows(
      simulate_group(n, "Aphantasia",     means = means_aph,     sd = sd),
      simulate_group(n, "Hypophantasia",  means = means_hypo,    sd = sd),
      simulate_group(n, "Typical",        means = means_typical, sd = sd),
      simulate_group(n, "Hyperphantasia", means = means_hyper,   sd = sd)
    ) |>
    factor_groups(n_groups = 4)

  if (modelling_version) {
    df_study <-
      df_study |>
      dplyr::mutate(
        dplyr::across(
          "visual_strat":"sensorimotor_strat",
          ~factor(., ordered = TRUE)
        )
      )
  }

  return(df_study)
}
