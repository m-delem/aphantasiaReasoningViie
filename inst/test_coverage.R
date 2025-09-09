test_coverage_active_file(
  file = "R/simulating_data_analyses.R",
  # Excluding the utils::menu interactive choice from checks
  line_exclusions = list("R/simulating_data_analyses.R" = c(256:273, 324))
  )

test_coverage(
  # Excluding the utils::menu interactive choice from checks
  line_exclusions = list("R/simulating_data_analyses.R" = c(256:273, 324))
)
