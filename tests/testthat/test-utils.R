test_that("ignore_unused_imports works properly", {
  expect_null(ignore_unused_imports())
})

test_that("data wrangling helpers work properly", {
  df <-
    survey_data |>
    filter_manually_identified_ids(verbose = FALSE) |>
    compute_nieq_scores() |>
    describe_survey_data()

  expect_contains(class(df),  c("tbl", "data.frame"))
})

library(superb)

test_that("ggplot2 helpers work properly", {
  df_expe <- get_clean_data()$df_expe
  p_acc <- plot_superb_jitter(df_expe, accuracy, group_2)

  expect_error(save_ggplot(p_acc, ncol = 3)) # 1 or 2 max
  expect_error(save_ggplot(p_acc)) # path missing

  save_path <- withr::local_tempfile(fileext = ".pdf")
  expect_invisible(
    save_ggplot(p_acc, path = save_path, ncol = 1, verbose = TRUE) |>
      suppressMessages()
  )
  expect_contains(
    class(
      save_ggplot(p_acc, save_path, 2, show = TRUE, verbose = FALSE)
    ),
    c("gg", "ggplot")
  )
  expect_contains(
    class(
      save_ggplot(p_acc, save_path, width = 90, show = TRUE, verbose = FALSE)
    ),
    c("gg", "ggplot")
  )

  group_effect <-
   tibble::tibble(
      x_star     = 1.5,
      y_star     = 1.08,
      stars      = "**",
      x_line     = x_star - 0.5,
      x_line_end = x_star + 0.5,
      y_line     = 1.05
    )
  expect_contains(
    class(add_significance(group_effect)[[1]]),
    c("gg", "ggproto")
  )
})
