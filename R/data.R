#' Participant's questionnaire data
#'
#' This dataset contains the responses to the questionnaires collected from the
#' participants. It has been gathered through several online experiments that
#' ran simulatneously on the
#' [innerexperiencelab.com](https://innerexperiencelab.com) website. It
#' contains the following:
#' - An encrypted unique participant `id` (that also allows to join the data
#'   with the experiment data)
#' - The `language` the experiment was conducted in (FR or EN)
#' - `age`
#' - `gender`
#' - `Group` (aphantasia, hypophantasia, typical, hyperphantasia or no_vviq)
#' - `country`
#' - `language_native` (the native language of the participant)
#' - `language_usual` (the language the participant usually uses)
#' - `job` (the job of the participant)
#' - `vviq_is_complete` (whether the participant completed the VVIQ)
#' - `vviq_total_score` (the total score of the VVIQ)
#' - All the VVIQ items
#' - `osivq_is_complete` (whether the participant completed the OSIVQ)
#' - `osivq_object`, `osivq_spatial`, `osivq_verbal` (the mean scores of the
#'   OSIVQ subscales)
#' - All the OSIVQ items
#' - `raven_is_complete` (whether the participant completed the Raven's
#'   Progressive Matrices)
#' - `raven_score` (the score of the Raven's Progressive Matrices)
#' - `nieq_is_complete` (whether the participant completed the NIEQ)
#' - All the NIEQ items on the two "frequency" and "proportion" sub-scales
#' - `*_false_info` (self-report of whether the participant provided false
#'   information on the questionnaires, people are usually honest about this)
#' - `met_issues` (self-report of whether the participant encountered issues
#'   during the experiment)
#' - `*_external_support` (self-report of whether the participant used
#'   external support during the experiment, e.g. paper)
#' - `strats_*` (self-report of the strategies used by the participant during
#'   the experiment)
#' - `has_adhd` (self-report of whether the participant has ADHD)
#' - `has_asd` (self-report of whether the participant has ASD)
#' - `has_dyslexia` (self-report of whether the participant has dyslexia)
#' - `has_other_neuro_trouble` (self-report of whether the participant has
#'   other neurological troubles)
#' - `has_treatment` (self-report of whether the participant is under treatment
#'   for a neurological trouble)
#' - `has_been_distracted` (self-report of whether the participant has been
#'   distracted during the experiment)
#' - `has_cheated` (self-report of whether the participant has used external
#'   help to answer the questions)
#'
#' Since the data has been collected in different experiments, not all
#' participants have NIEQ data, which was not central for this specific
#' paradigm. It has been included here for exploratory purposes.
#'
#' @source Data collected through online experiments
"survey_data"

#' Reasoning experiment data
#'
#' This dataset contains the results of the main reasoning experiment. It
#' contains the following columns:
#' - An encrypted unique participant `id` (that also allows to join the data
#'   with the survey data)
#' - `language` (the language the experiment was conducted in, FR or EN)
#' - `group` (aphantasia, hypophantasia, typical, hyperphantasia or no_vviq)
#' - `expe_phase` (the phase of the experiment)
#' - `trial_number` (the presentation order of the problems)
#' - `problem` (the problem presented to the participant)
#' - `category` (the category of the problem, visual, spatial or control)
#' - `premise_*_rt` (the reaction time of the participant for each premise)
#' - `conclusion_rt` (the reaction time of the participant for the conclusion,
#'   i.e., the response screen)
#' - `rt_total` (the total time spent on the trial)
#' - `response` (the participant's response, TRUE or FALSE)
#' - `correct_response` (the correct response to the problem, TRUE or FALSE)
#' - `accuracy` (the accuracy of the participant's response, TRUE or FALSE)
#'
#' @source Data collected through an online experiment
"experiment_data"

#' Results of the power analysis by simulation presented in the manuscript
#'
#' This dataset contains the results of the power analysis conducted by
#' fitting models on 147,000 simulated datasets with various sample sizes
#' and effect sizes. The results are used to estimate the statistical power
#' of the study to detect effects of different sizes with different sample
#' sizes. This analysis took 16 hours and 44 minutes on a machine with 40 cores,
#' so naturally the dataset is included here to avoid having to redo it. It
#' contains the same columns as the results of the `run_power_analysis()`:
#' - `n_subj_per_group` (number of subjects per group in the simulated dataset)
#' - `beta_vis` (the effect size used in the simulation, in seconds)
#' - `n_simulations` (the identifier of the simulation, from 1 to the number
#' of simulations per combination, here 350)
#' - `p_value` (the p-value of the effect of interest in the fitted model)
"power_results"
