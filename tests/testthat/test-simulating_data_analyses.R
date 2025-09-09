test_that("Simulation functions return objects with the correct classes", {
  df_rt  <- simulate_rt_data(100, seed = 123)
  df_acc <- simulate_acc_data(100, seed = 123)
  df_str <- simulate_strats_data(100, seed = 123, modelling_version = TRUE)
  p <- plot_power(power_results)

  expect_contains(class(df_rt),  c("tbl", "data.frame"))
  expect_contains(class(df_acc), c("tbl", "data.frame"))
  expect_contains(class(df_str), c("tbl", "data.frame"))
  expect_equal(class(df_str$visual_strat), c("ordered", "factor"))
  expect_equal(class(simulate_rt_test(40, 1.5)), "numeric")
  expect_error(class(run_power_analysis(test_skip = 0) |> suppressMessages()))
  expect_contains(
    class(
      run_power_analysis(
        n_min        = 10,
        n_max        = 10,
        n_step       = 10,
        beta_vis_min = 2,
        beta_vis_max = 2,
        beta_step = 0,
        n_simulations = 1,
        test_skip = 1
      ) |> suppressMessages()
    ),
    c("tbl", "data.frame")
  )
  expect_error(
    run_power_analysis(time_estimate = 30, test_skip = 0) |> suppressMessages()
  )
  expect_contains(class(p), c("gg", "ggplot"))
})


