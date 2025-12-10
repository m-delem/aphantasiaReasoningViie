library(superb)

test_that("The plotting suite that uses superb works correctly", {
  df_expe <- get_clean_data("experiment")
  df_rt   <- df_expe |> filter_trials_on_rt()

  p1 <- plot_superb_raincloud(df_rt, rt_total, group_2)
  p2 <- plot_superb_jitter(df_expe, accuracy, group_3)
  p3 <- plot_superb_categories(df_rt, rt_total, strategy_group)

  expect_contains(
    class(prepare_df_for_plotting(df_rt, rt_total)),
    c("tbl", "data.frame")
  )
  expect_contains(class(p1), c("gg", "ggplot"))
  expect_contains(class(p2), c("gg", "ggplot"))
  expect_contains(class(p3), c("gg", "ggplot"))
})
