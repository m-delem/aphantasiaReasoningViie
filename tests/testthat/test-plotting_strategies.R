test_that("Strategy plotting functions work as expected", {
  df_long <-
    get_clean_data("survey") |>
    pivot_strategies_longer()

  p1 <-
    plot_strategies_scores(
      df_long, grouping = group_4,
      x_labels = c("A", "B", "C", "D")
    )
  p2 <- plot_strategies_barplot(df_long, grouping = group_4)

  expect_contains(class(p1), c("gg", "ggplot"))
  expect_contains(class(p2), c("gg", "ggplot"))
})
