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
