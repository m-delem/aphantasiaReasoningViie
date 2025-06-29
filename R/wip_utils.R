compute_nieq_scores <- function(df) {
  df_scored <-
    df |>
    dplyr::mutate(
      nieq_voice    = (nieq_freq_inner_voice + nieq_prop_inner_voice) / 2,
      nieq_visual   = (nieq_freq_mental_imagery + nieq_prop_mental_imagery) / 2,
      nieq_emotions = (nieq_freq_emotions + nieq_prop_emotions) / 2,
      nieq_sensory  = (nieq_freq_sensory_focus + nieq_prop_sensory_focus) / 2,
      nieq_abstract = (nieq_freq_unsymbolised + nieq_prop_unsymbolised) / 2
    ) |>
    dplyr::relocate("nieq_voice":"nieq_abstract", .after = "nieq_is_complete")
  return(df_scored)
}

describe_survey_data <- function(df) {
  df_summary <-
    df |>
    dplyr::group_by(group) |>
    dplyr::reframe(
      n = dplyr::n(),
      female = sum(gender == "f"),
      other  = sum(!(gender %in% c("m", "f"))),
      vviq = mean(vviq_total_score, na.rm = TRUE) |> round(2),
      object  = mean(osivq_object, na.rm = TRUE) |> round(2),
      spatial = mean(osivq_spatial, na.rm = TRUE) |> round(2),
      verbal  = mean(osivq_verbal, na.rm = TRUE) |> round(2),
      raven = mean(raven_score, na.rm = TRUE) |> round(2),
      nieq_completed = sum(nieq_is_complete == TRUE),
      voice = mean(nieq_voice, na.rm = TRUE) |> round(2),
      visual = mean(nieq_visual, na.rm = TRUE) |> round(2),
      emotions = mean(nieq_emotions, na.rm = TRUE) |> round(2),
      sensory = mean(nieq_sensory, na.rm = TRUE) |> round(2),
      abstract = mean(nieq_abstract, na.rm = TRUE) |> round(2)
    )
  return(df_summary)
}
