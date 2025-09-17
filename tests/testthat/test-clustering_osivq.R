test_that("The clustering suite works as expected", {
  df <- get_clean_data()$df_survey
  clustering <- cluster_osivq(df)
  df2 <- df |> add_named_clusters(clustering)
  df3 <- summarise_clustering(df2)
  p <- plot_osivq_ternary(df2, plot_it = TRUE) |> suppressMessages()


  expect_contains(class(df), c("tbl", "data.frame"))
  expect_contains(class(df2), c("tbl", "data.frame"))
  expect_contains(class(df3), c("tbl", "data.frame"))
  expect_equal(class(clustering), "list")
  expect_contains(class(p), c("gg", "ggplot"))
})
