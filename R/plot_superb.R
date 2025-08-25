plot_superb_raincloud <- function(
    df,
    dvar,
    grouping,
    title = NULL,
    y_title = NULL,
    dw = 0.4
) {
  library(superb)

  df_plot <-
    df |>
    dplyr::group_by(
      id, category,
      dplyr::across(c(
        tidyselect::contains("group"),
        tidyselect::contains("cluster")
      ))
    ) |>
    dplyr::reframe(
      mean_dv = mean({{ dvar }}, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      # reverse coding for clusters only
      dplyr::across(
        tidyselect::contains("cluster"),
        ~forcats::fct_relevel(
          .,
          rev(levels(dplyr::pull(df, tidyselect::contains("cluster"))))
        )
      )
    )

  grouping_str <- as.character(rlang::ensym(grouping))
  formula    <- stats::as.formula(
    glue::glue("mean_dv ~ {grouping_str} * category")
  )

  p <-
    superb::superb(
      formula,
      data = df_plot,
      plotLayout = "raincloud",
      pointParams = list(
        size = 1.5,
        alpha = 1,
        position = ggplot2::position_dodge(width = dw)
      ),
      jitterParams = list(
        size = 1,
        alpha = 0.1,
        position = ggplot2::position_jitterdodge(
          jitter.width  = 0.2,
          jitter.height = 0,
          dodge.width   = dw
        )
      ),
      errorbarParams = list(
        linewidth = 0.3,
        width = 0,
        position = ggplot2::position_dodge(width = dw)
      ),
      violinParams = list(
        trim = TRUE,
        width = 0.7,
        linewidth = 0.2
      ),
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = y_title
    ) +
    ggplot2::scale_x_discrete(
      expand = ggplot2::expansion(mult = 0, add = c(0, 0.7))
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(10),
    ) +
    ggplot2::scale_discrete_manual(
      name = "Problem category: ",
      aesthetics = c("color", "fill"),
      values = c(
        # Visual  = "#1b6096",
        # Control = "#793183",
        # Spatial = "#318f2c"
        Visual = palette.colors()[3],
        Control = palette.colors()[4],
        Spatial = palette.colors()[2]
      )
    ) +
    ggplot2::scale_shape_manual(
      name = "Problem category: ",
      values = c(19, 17, 18)
    ) +
    theme_pdf(
      base_theme = ggplot2::theme_minimal,
      axis_relative_size = 0.9,
      # Custom theme arguments
      panel.grid.major.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "grey80", fill = NA)
    )

  p$layers[[1]]$constructor$position$x <- 0
  p$layers[[2]]$position$jitter.width <- 0.05

  return(p)
}

plot_superb_jitter <- function(
    df,
    dvar,
    grouping,
    title = NULL,
    y_title = NULL
) {
  library(superb)

  df_plot <-
    df |>
    dplyr::group_by(
      id, category,
      dplyr::across(c(
        tidyselect::contains("group"),
        tidyselect::contains("cluster")
      ))
    ) |>
    dplyr::reframe(
      mean_dv = mean({{ dvar }}, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      # reverse coding for clusters only
      dplyr::across(
        tidyselect::contains("cluster"),
        ~forcats::fct_relevel(
          .,
          rev(levels(dplyr::pull(df, tidyselect::contains("cluster"))))
        )
      )
    )

  grouping_str <- as.character(rlang::ensym(grouping))
  formula    <- stats::as.formula(
    glue::glue("mean_dv ~ {grouping_str} * category")
  )

  p <-
    superb::superb(
      formula,
      data = df_plot,
      plotLayout = "pointjitter",
      pointParams = list(
        size = 2.25,
        alpha = 1
        # position = ggplot2::position_dodge(width = 0.5)
      ),
      jitterParams = list(
        size = 1.5,
        alpha = 0.1,
        position = ggplot2::position_jitterdodge(
          jitter.width  = 0.1,
          jitter.height = 0.01,
          dodge.width   = 0.5
        )
      ),
      errorbarParams = list(
        width = 0,
        linewidth = 0.5
        # position = ggplot2::position_dodge(width = 0.5)
      ),
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = y_title
    ) +
    ggplot2::scale_x_discrete(
      expand = ggplot2::expansion(mult = 0, add = 0.6)
    ) +
    ggplot2::scale_discrete_manual(
      name = "Problem category: ",
      aesthetics = c("color", "fill"),
      values = c(
        # Visual  = "#1b6096",
        # Control = "#793183",
        # Spatial = "#318f2c"
        Visual = palette.colors()[3],
        Control = palette.colors()[4],
        Spatial = palette.colors()[2]
      )
    ) +
    theme_pdf(
      base_theme = ggplot2::theme_minimal,
      axis_relative_size = 0.9,
      # Custom theme arguments
      panel.grid.major.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "grey80", fill = NA)
    )

  return(p)
}
