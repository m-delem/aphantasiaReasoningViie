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
  type = "all",
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

- type:

  The type of data to return: "experiment", "survey", or "all".

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

A cleaned data frame or a list of cleaned data frames, depending on the
`type` parameter:

- If `type` is "experiment", returns the cleaned experiment data frame.

- If `type` is "survey", returns the cleaned survey data frame.

- If `type` is "all", returns a list containing both cleaned data
  frames.

## Examples

``` r
clean_data <- get_clean_data(type = "all", verbose = TRUE)
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
colnames(get_clean_data(type = "experiment"))
#>  [1] "id"                 "language"           "group_4"           
#>  [4] "group_2"            "group_3"            "strategy_group"    
#>  [7] "expe_phase"         "trial_number"       "problem"           
#> [10] "category"           "premise_1_rt"       "premise_2_rt"      
#> [13] "premise_3_rt"       "conclusion_rt"      "rt_total"          
#> [16] "response"           "correct_response"   "accuracy"          
#> [19] "acc_perc"           "visual_strat"       "verbal_strat"      
#> [22] "spatial_strat"      "semantic_strat"     "sensorimotor_strat"
#> [25] "asso_strat_1"       "other_strat"        "asso_strat_2"      
#> [28] "asso_strat_3"       "median_rt"         
colnames(get_clean_data(type = "survey"))
#>   [1] "id"                       "language"                
#>   [3] "age"                      "gender"                  
#>   [5] "group_4"                  "group_2"                 
#>   [7] "group_3"                  "strategy_group"          
#>   [9] "country"                  "language_native"         
#>  [11] "language_usual"           "job"                     
#>  [13] "education"                "field"                   
#>  [15] "vviq_is_complete"         "vviq_total_score"        
#>  [17] "vviq_q01"                 "vviq_q02"                
#>  [19] "vviq_q03"                 "vviq_q04"                
#>  [21] "vviq_q05"                 "vviq_q06"                
#>  [23] "vviq_q07"                 "vviq_q08"                
#>  [25] "vviq_q09"                 "vviq_q10"                
#>  [27] "vviq_q11"                 "vviq_q12"                
#>  [29] "vviq_q13"                 "vviq_q14"                
#>  [31] "vviq_q15"                 "vviq_q16"                
#>  [33] "osivq_is_complete"        "osivq_object"            
#>  [35] "osivq_spatial"            "osivq_verbal"            
#>  [37] "osivq_q01s"               "osivq_q02v"              
#>  [39] "osivq_q04v"               "osivq_q05s"              
#>  [41] "osivq_q06o"               "osivq_q07s"              
#>  [43] "osivq_q08v"               "osivq_q09v"              
#>  [45] "osivq_q11o"               "osivq_q12o"              
#>  [47] "osivq_q13o"               "osivq_q14s"              
#>  [49] "osivq_q16v"               "osivq_q17s"              
#>  [51] "osivq_q18o"               "osivq_q20o"              
#>  [53] "osivq_q23o"               "osivq_q26o"              
#>  [55] "osivq_q27s"               "osivq_q29o"              
#>  [57] "osivq_q30s"               "osivq_q31s"              
#>  [59] "osivq_q32s"               "osivq_q33o"              
#>  [61] "osivq_q34o"               "osivq_q35v"              
#>  [63] "osivq_q37v"               "osivq_q39v"              
#>  [65] "osivq_q40o"               "osivq_q41v"              
#>  [67] "osivq_q42s"               "osivq_q43o"              
#>  [69] "osivq_q44s"               "osivq_q45o"              
#>  [71] "raven_is_complete"        "raven_score"             
#>  [73] "nieq_is_complete"         "nieq_voice"              
#>  [75] "nieq_visual"              "nieq_emotions"           
#>  [77] "nieq_sensory"             "nieq_abstract"           
#>  [79] "nieq_freq_inner_voice"    "nieq_freq_mental_imagery"
#>  [81] "nieq_freq_emotions"       "nieq_freq_sensory_focus" 
#>  [83] "nieq_freq_unsymbolised"   "nieq_prop_inner_voice"   
#>  [85] "nieq_prop_mental_imagery" "nieq_prop_emotions"      
#>  [87] "nieq_prop_sensory_focus"  "nieq_prop_unsymbolised"  
#>  [89] "gave_false_info"          "what_false_info"         
#>  [91] "visual_strat"             "verbal_strat"            
#>  [93] "spatial_strat"            "semantic_strat"          
#>  [95] "sensorimotor_strat"       "asso_strat_1"            
#>  [97] "other_strat"              "asso_strat_2"            
#>  [99] "asso_strat_3"             "prognosis"               
#> [101] "neuro_trouble"            "treatment"               
#> [103] "has_adhd"                 "has_asd"                 
#> [105] "has_dyslexia"             "has_other_neuro_trouble" 
#> [107] "has_treatment"            "has_been_distracted"     
#> [109] "has_cheated"              "met_issues"              
#> [111] "issues"                   "used_external_support"   
#> [113] "what_external_support"   
```
