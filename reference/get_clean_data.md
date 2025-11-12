# Wrapper function to get clean "analysis-ready" data

This function retrieves and cleans the data for the experiment and
survey. It uses several helper functions to filter and format the data,
including
[`filter_random_accuracy_ids()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/filter_random_accuracy_ids.md),
[`filter_manually_identified_ids()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/filter_manually_identified_ids.md),
[`filter_suspicious_rt_ids()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/filter_suspicious_rt_ids.md),
[`factor_categories()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/factor_categories.md),
[`factor_groups()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/factor_groups.md),
[`factor_chr_vars()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/factor_chr_vars.md),
[`factor_strategies()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/factor_strategies.md),
and
[`compute_nieq_scores()`](https://m-delem.github.io/aphantasiaReasoningViie/reference/compute_nieq_scores.md).
The cleaned data is returned as a list containing two data frames:
`df_expe` and `df_survey`. The `df_expe` data frame contains the cleaned
experiment data, while the `df_survey` data frame contains the cleaned
survey data.

## Usage

``` r
get_clean_data(
  n_groups = 2,
  exclude_no_vviq = TRUE,
  exclude_no_osivq = TRUE,
  exclude_no_raven = TRUE,
  exclude_cheated = TRUE,
  exclude_distracted = TRUE,
  exclude_treatment = FALSE,
  exclude_adhd = FALSE,
  exclude_asd = FALSE,
  exclude_dyslexia = FALSE,
  exclude_other = FALSE,
  sd_mult = 2.25,
  verbose = FALSE
)
```

## Arguments

- n_groups:

  The number of groups to factor in the data. Must be 2, 3 or 4. 2
  divides the sample into Aphants and Typical imagers using the 32 VVIQ
  criterio, 3 divides the sample into Aphants (VVIQ = 16), Hypophants
  (VVIQ \< 32) and Typical imagers, and 4 also isolates Hyperphants with
  VVIQ \> 75.

- exclude_no_vviq:

  Logical, whether to exclude participants without VVIQ.

- exclude_no_osivq:

  Logical, whether to exclude participants without OSIVQ.

- exclude_no_raven:

  Logical, whether to exclude participants without Raven.

- exclude_cheated:

  Logical, whether to exclude participants who have cheated (based on
  self-report).

- exclude_distracted:

  Logical, whether to exclude participants who have been distracted
  (based on self-report).

- exclude_treatment:

  Logical, whether to exclude participants who have a treatment for a
  neurological or psychiatric disorder.

- exclude_adhd:

  Logical, whether to exclude participants who have ADHD.

- exclude_asd:

  Logical, whether to exclude participants who have ASD.

- exclude_dyslexia:

  Logical, whether to exclude participants who have dyslexia.

- exclude_other:

  Logical, whether to exclude participants who have other neurological
  troubles.

- sd_mult:

  A numeric value indicating how many standard deviations to use for
  identifying suspicious median RTs. The default is 2.25, which means
  that median RTs that are more than 2.25 standard deviations inferior
  to the mean are considered suspiciously fast and potential "spamming".

- verbose:

  A logical value indicating whether to print verbose messages about the
  filtering process. Default is `FALSE`.

## Value

A list containing two data frames:

- `df_expe`: The cleaned experiment data.

- `df_survey`: The cleaned survey data.

## Examples

``` r
clean_data <- get_clean_data(verbose = TRUE)
#> 
#> Sample size before accuracy analysis: 137
#> Participants below random accuracy (<= 50%): 8 (5.84%)
#> 
#> Sample size before manual examination: 137
#> Manually identified participants:
#> - N without VVIQ: 3 -> Excluded
#> - N without OSIVQ: 6 -> Excluded
#> - N without Raven: 2 -> Excluded
#> - N who cheated: 3 -> Excluded
#> - N who were distracted: 12 -> Excluded
#> - N who had treatment: 4 -> Included
#> - N with ADHD: 7 -> Included
#> - N with ASD: 5 -> Included
#> - N with dyslexia: 2 -> Included
#> - N with other neuro troubles: 2 -> Included
#> Participants to exclude: 24 (17.52%)
#> 
#> Sample size before median RTs analysis: 106
#> Participants with median RTs outside 2.25 SDs: 2 (1.89%)
head(clean_data$df_expe)
#> # A tibble: 6 × 29
#>   id       language group group_2 group_3 strategy_group expe_phase trial_number
#>   <fct>    <fct>    <fct> <fct>   <fct>   <fct>          <fct>             <int>
#> 1 acdn247… fr       Typi… Typical Typical No visual str… expe_bloc…            1
#> 2 acdn247… fr       Typi… Typical Typical No visual str… expe_bloc…            2
#> 3 acdn247… fr       Typi… Typical Typical No visual str… expe_bloc…            3
#> 4 acdn247… fr       Typi… Typical Typical No visual str… expe_bloc…            4
#> 5 acdn247… fr       Typi… Typical Typical No visual str… expe_bloc…            5
#> 6 acdn247… fr       Typi… Typical Typical No visual str… expe_bloc…            6
#> # ℹ 21 more variables: problem <int>, category <fct>, premise_1_rt <dbl>,
#> #   premise_2_rt <dbl>, premise_3_rt <dbl>, conclusion_rt <dbl>,
#> #   rt_total <dbl>, response <fct>, correct_response <fct>, accuracy <int>,
#> #   acc_perc <dbl>, visual_strat <ord>, verbal_strat <ord>,
#> #   spatial_strat <ord>, semantic_strat <ord>, sensorimotor_strat <ord>,
#> #   asso_strat_1 <fct>, other_strat <fct>, asso_strat_2 <fct>,
#> #   asso_strat_3 <fct>, median_rt <dbl>
head(clean_data$df_survey)
#> # A tibble: 6 × 113
#>   id          language   age gender group group_2 group_3 strategy_group country
#>   <fct>       <fct>    <int> <fct>  <fct> <fct>   <fct>   <fct>          <fct>  
#> 1 acdn247721… fr          24 f      Typi… Typical Typical No visual str… fra    
#> 2 ahos206230… fr          26 f      Apha… Aphant… Aphant… No visual str… fra    
#> 3 anoo201523… fr          23 m      Typi… Typical Typical Visual strate… fra    
#> 4 arje911192… fr          26 f      Typi… Typical Typical Visual strate… fra    
#> 5 auzb748856… fr          25 f      Typi… Typical Typical No visual str… fra    
#> 6 azcj317771… fr          28 m      Hypo… Aphant… Hypoph… Visual strate… fra    
#> # ℹ 104 more variables: language_native <fct>, language_usual <fct>, job <fct>,
#> #   education <fct>, field <fct>, vviq_is_complete <lgl>,
#> #   vviq_total_score <int>, vviq_q01 <int>, vviq_q02 <int>, vviq_q03 <int>,
#> #   vviq_q04 <int>, vviq_q05 <int>, vviq_q06 <int>, vviq_q07 <int>,
#> #   vviq_q08 <int>, vviq_q09 <int>, vviq_q10 <int>, vviq_q11 <int>,
#> #   vviq_q12 <int>, vviq_q13 <int>, vviq_q14 <int>, vviq_q15 <int>,
#> #   vviq_q16 <int>, osivq_is_complete <lgl>, osivq_object <dbl>, …
```
