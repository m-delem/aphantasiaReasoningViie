# Create the clean datasets from the raw OSF data (see import_osf_data.R)
devtools::load_all()

data_paths <- here::here("inst/extdata") |> fs::dir_ls(glob = "*.rda")
load(data_paths[1]) # common_survey_data_full
load(data_paths[2]) # expe_reasoning_viie_data

survey_data <-
  dplyr::inner_join(
    common_survey_data_full |>
      dplyr::select(
        id:gender, group = Group, country,
        language_native, language_usual, job:treatment,
        vviq_is_complete, vviq_total_score:vviq_q16,
        osivq_is_complete,
        osivq_object  = object_mean,
        osivq_spatial = spatial_mean,
        osivq_verbal  = verbal_mean,
        osivq_q01s:osivq_q45o,
        raven_is_complete, raven_score,
        nieq_is_complete, nieq_freq_inner_voice:nieq_prop_unsymbolised,
        tidyselect::contains("false_info")
    ),
    expe_reasoning_viie_data |>
      dplyr::select(
        id, language,
        met_issues:what_external_support,
        tidyselect::contains("strats")
      ) |>
      dplyr::distinct(),
    by = "id"
  ) |>
  dplyr::relocate(language, .after = "id") |>
  # These are IDs that have been manually identified by examining the
  # self-reports in demographics or feedback questionnaires.
  dplyr::mutate(
    has_adhd = ifelse(
      id %in% c(
        "aacu64091390979054fksk",
        "arje91119258110263fkhy",
        "chtj291613971695392tusk",
        "kxdb18777623490710beas",
        "mcfh7605835623634kafe",
        "wgjv914211981148922tqcq",
        "zzin6422451504074hgiy"
      ), TRUE, FALSE
    ),
    has_asd = ifelse(
      id %in% c(
        "govc249714444628358nmcs",
        "kxuc65858692499750bxjd",
        "slar297251711194586ymwe",
        "uqof56314956259480caqe",
        "wgjv914211981148922tqcq"
      ), TRUE, FALSE
    ),
    has_dyslexia = ifelse(
      id %in% c(
        "btof87641287389657kppk",
        "njep4637599176738zcuf"
      ), TRUE, FALSE
    ),
    has_other_neuro_trouble = ifelse(
      id %in% c(
        "mnqo47349179516488qkoz",
        "wbwg888518836285526bvbw"
      ), TRUE, FALSE
    ),
    has_treatment = ifelse(
      treatment != "yes" | prognosis != "yes" | is.na(treatment),
      FALSE, TRUE
    ),
    has_been_distracted = ifelse(
      id %in% c(
        "akao550473973998319bmig",
        "bpbg819500163736444kpjq",
        "eba468419788916277jsum",
        "gxen13134778950801xhpe",
        "jccb899119951972196ookw",
        "jrvo66323815286541ules",
        "mcfh7605835623634kafe",
        "mdqm339933530726527rpkx",
        "uepg64719531843102bsfh",
        "zqqy76797125458479dgrr"
      ), TRUE, FALSE
    ),
    has_cheated = ifelse(
      id %in% c(
        "igvm65036039452810gojz",
        "sixo57424946541911mrdn",
        "wczz320907976180679kghc"
      ), TRUE, FALSE
    )
  ) |>
  dplyr::relocate(prognosis:treatment, .before = "has_adhd") |>
  dplyr::relocate(met_issues:what_external_support, .after = "has_cheated")

experiment_data <-
  expe_reasoning_viie_data |>
  dplyr::select(
    id, language, Group,
    expe_phase, trial_number, problem = trial_code,
    category:correct_response, accuracy
  ) |>
  dplyr::filter(expe_phase != "training")

usethis::use_data(survey_data, overwrite = TRUE)
usethis::use_data(experiment_data, overwrite = TRUE)
