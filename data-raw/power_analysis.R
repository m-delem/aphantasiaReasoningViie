# This is the code that was used to produce the power analysis presented in the
# manuscript. The 147,000 simulations took 16 hours to run so the results were
# saved as package data.
devtools::load_all()

power_results <-
  run_power_analysis(
    n_min        = 10,
    n_max        = 200,
    n_step       = 10,
    beta_vis_min = 0.5,
    beta_vis_max = 2.5,
    beta_step    = 0.1,
    n_simulations = 350
  )

usethis::use_data(power_results, overwrite = TRUE)
