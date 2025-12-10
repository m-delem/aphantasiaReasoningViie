# Package index

## Raw data

- [`survey_data`](https://m-delem.github.io/aphantasiaReasoningViie/reference/survey_data.md)
  : Participant's questionnaire data

- [`experiment_data`](https://m-delem.github.io/aphantasiaReasoningViie/reference/experiment_data.md)
  : Reasoning experiment data

- [`power_results`](https://m-delem.github.io/aphantasiaReasoningViie/reference/power_results.md)
  : Results of the power analysis by simulation presented in the
  manuscript

- [`nl_contrasts`](https://m-delem.github.io/aphantasiaReasoningViie/reference/nl_contrasts.md)
  :

  Results of the contrast analyses performed on the `mgcv` non-linear
  models

- [`nl_predictions`](https://m-delem.github.io/aphantasiaReasoningViie/reference/nl_predictions.md)
  :

  Model predictions computed with the `mgcv` non-linear model fits

## Data preparation

- [`get_clean_data()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_clean_data.md)
  : Wrapper function to get clean "analysis-ready" data

- [`get_clustered_data()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_clustered_data.md)
  : Wrapper function to get clean and clustered data immediately

- [`get_viie_data()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_viie_data.md)
  : Get data with the Visual Imagery Impedance Effect manually
  calculated

- [`filter_random_accuracy_ids()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/filter_random_accuracy_ids.md)
  : Filter participants with below random accuracy

- [`filter_manually_identified_ids()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/filter_manually_identified_ids.md)
  : Filter manually identified participant based on various criteria

- [`filter_suspicious_rt_ids()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/filter_suspicious_rt_ids.md)
  [`mark_suspicious_rt_ids()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/filter_suspicious_rt_ids.md)
  : Filter (or mark) participants with suspicious median RTs

- [`filter_trials_on_rt()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/filter_trials_on_rt.md)
  : Filter outlier trials based on mean response time per participant

- [`factor_categories()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/factor_categories.md)
  :

  Convert the `category` column to a factor with contrasts

- [`factor_groups()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/factor_groups.md)
  :

  Convert the `group` column to a factor with the desired VVIQ
  classification

- [`factor_strategies()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/factor_strategies.md)
  : Convert strategy columns to factors with optionally ordered levels

- [`factor_chr_vars()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/factor_chr_vars.md)
  : Convert all character variables in a data frame to factors

- [`compute_nieq_scores()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/compute_nieq_scores.md)
  : Compute NIEQ scores by combining the frequency and proportion items
  of each subscale

- [`pivot_strategies_longer()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/pivot_strategies_longer.md)
  : Get a long format data frame with the strategies gathered in a
  single column

- [`pivot_phases_longer()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/pivot_phases_longer.md)
  : Get a long format data frame with the problem terms in a single
  column

## Clustering

- [`cluster_osivq()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/cluster_osivq.md)
  : Cluster the OSIVQ data using consensus between various algorithms
- [`add_named_clusters()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_named_clusters.md)
  : Add a column with named cluster assignments to a data frame
- [`summarise_clustering()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/summarise_clustering.md)
  : Get the size and questionnaire means of clusters

## Modelling

- [`build_formula()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/build_formula.md)
  : Build a formula based on the common model for accuracy and RT
  modelling
- [`set_ranef_prior()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/set_ranef_prior.md)
  : Create a weakly informative regularizing Gamma prior for the random
  effects
- [`fit_clm()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/fit_clm.md)
  : Fit a cumulative link model (CLM) using the ordinal package
- [`fit_brms_model()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/fit_brms_model.md)
  : Fit a Bayesian model using the brms package with default settings
- [`report_rope()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/report_rope.md)
  : Report the ROPE analysis for marginal effects
- [`get_contrast()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_contrast.md)
  : Get the pairwise contrasts of variables in a model
- [`get_params()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_params.md)
  : Get the fixed parameters of a model in a clean format
- [`get_performance()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_performance.md)
  : Get performance indices for a model in a clean format
- [`get_singularity()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/get_singularity.md)
  : Check if the model is singular and print a message
- [`report_contrast()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/report_contrast.md)
  : Get the contrasts of a model and format them for reporting

## Visualisation

- [`plot_median_rt_distribution()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_median_rt_distribution.md)
  : Plot the distribution of the median RT across participants

- [`plot_superb_raincloud()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_superb_raincloud.md)
  [`plot_superb_jitter()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_superb_raincloud.md)
  [`plot_superb_categories()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_superb_raincloud.md)
  :

  Plot accuracy or RT data with the `superb` package

- [`plot_strategies_barplot()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_strategies_barplot.md)
  : Plot proportions of strategy use for groups as barplots

- [`plot_strategies_scores()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_strategies_scores.md)
  : Plot mean strategy scores for groups

- [`plot_nl()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_nl.md)
  : Plot model predictions from the non-linear GAMM results

- [`add_significance()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/add_significance.md)
  : Add significance label and line to a plot

- [`theme_pdf()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/theme_pdf.md)
  : Theme for elegant scientific vector figures

- [`save_ggplot()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/save_ggplot.md)
  : Custom ggsave wrapper set with Nature's formatting guidelines
  (width-locked)

## Simulation

- [`simulate_rt_data()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/simulate_rt_data.md)
  : Simulate skewed RT data for the factorial design
- [`simulate_rt_test()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/simulate_rt_test.md)
  : Simulate data, fit a model on it and test our hypothesis
- [`run_power_analysis()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/run_power_analysis.md)
  : Conduct a power analysis by simulation
- [`plot_power()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/plot_power.md)
  : Plot the results of a power analysis by simulation
