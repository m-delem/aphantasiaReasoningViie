test_that("Miscellaneous plotting functions work as expected", {
  df <- get_clean_data("experiment", sd_mult = 99)

  p1 <- plot_median_rt_distribution(df, sd_mult = 2.25, base_size = 12)
  p2 <- plot_nl(nl_predictions$vviq_2, title = "Test")

  expect_contains(class(p1), c("gg", "ggplot"))
  expect_contains(class(p2), c("gg", "ggplot"))
})
