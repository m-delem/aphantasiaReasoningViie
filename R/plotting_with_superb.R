#' Helper function to prepare the data frame for plotting
#'
#' @description
#' This function groups the data by participant ID, category, and any grouping
#' or clustering variables, then calculates the mean of the dependent variable
#' for each group. It also reverses the factor levels for any clustering
#' variables to ensure correct ordering in plots.
#'
#' @param df A data frame containing the data to be plotted.
#' @param dvar The dependent variable to be averaged and plotted.
#'
#' @returns A data frame ready for plotting with the mean dependent variable
#' for each participant, category, and grouping/cluster combination.
#' @export
#'
#' @examples
#' df <- get_clean_data()$df_expe
#' df_rt <- filter_trials_on_rt(df)
#'
#' df_rt |> prepare_df_for_plotting(dvar = rt_total) |> head(12)
#'
#' @keywords internal
prepare_df_for_plotting <- function(
    df,
    dvar
) {
  df_plot <-
    df |>
    dplyr::group_by(
      .data$id, .data$category,
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

  return(df_plot)
}

#' Plot accuracy or RT data with the `superb` package
#'
#' @description
#' These functions create raincloud, jitter or line plots using the `superb`
#' package, which allows to compute and plot correct 95% confidence intervals
#' easily. `superb` allows for a lot of customisation, however it is not
#' fully explained in its documentation, so I had to do a lot of trial and
#' error to get the plots looking the way I wanted. The functions here wrap the
#' core [superb::superb()] function and have lot of (thankfully, optional)
#' arguments that make the customisation options I used more explicit.
#' They are set by default to make the figures look good in a small format for
#' PDF export (see [save_ggplot()]).
#'
#' `plot_superb_jitter` creates jitter plots to visualise the distribution
#' of individual participant means of a dependent variable (e.g., accuracy or
#' reaction time) across different categories and groups along with overall
#' means and 95% confidence intervals.
#'
#' `plot_superb_raincloud` creates "raincloud" plots that combine the jitter
#' plots with half-violins that represent the density of the distributions.
#'
#' `plot_superb_categories` switches the mapping of the x-axis and the colours
#' to have the three categories on the x-axis and the groupings as colours.
#' It uses the "line" plots from the `superb` package to connect the means
#' across categories for each group.
#'
#' @param df A data frame containing the data to be plotted.
#' @param dvar The dependent variable to be averaged and plotted, typically
#' `accuracy` or `rt_total`.
#' @param grouping A variable to group the data by, e.g., `group`,
#' `group_2`, `group_3`, `strategy_group`, etc.
#' @param title   Optional. Title for the plot.
#' @param x_title Optional. Title for the x-axis.
#' @param y_title Optional. Title for the y-axis.
#' @param dot_size The size of the dots that represent group means.
#' @param dot_alpha The alpha transparency of the dots that represent
#' group means.
#' @param jitter_size The size of the individual data points.
#' @param jitter_alpha The alpha transparency of the individual data points.
#' @param jitter_width The width of the jitter applied to individual data
#' points.
#' @param jitter_height The height of the jitter applied to individual data
#' points.
#' @param dodge_width The width of the dodge applied to separate groups.
#' @param errorbar_linewidth The line width of the black error bars representing
#' 95% confidence intervals around the group means.
#' @param errorbar_h_width The width of the horizontal black lines at the top
#' and bottom of error bars.
#' @param trim Logical, whether to trim the violins to the range of the data.
#' @param violin_width The width of the violins.
#' @param violin_linewidth The line width of the outline of the violins.
#' @param exp_mult A multiplier for the [ggplot2::expansion()] function to
#' increase the space between the axis and the data.
#' @param exp_add_left An additional value for the [ggplot2::expansion()]
#' function to increase the space on the left side of the x-axis.
#' @param exp_add_right An additional value for the [ggplot2::expansion()]
#' function to increase the space on the right side of the x-axis.
#' @param n_breaks The number of breaks passed to the [scales::breaks_pretty()]
#' function.
#' @param visual_colour A colour for the "Visual" category dots and violins.
#' Default is the Okabe-Ito palette's blue.
#' @param control_colour A colour for the "Control" category dots and violins.
#' Default is the Okabe-Ito palette's green.
#' @param spatial_colour A colour for the "Spatial" category dots and violins.
#' Default is the Okabe-Ito palette's orange.
#' @param axis_rel A numeric value for the relative size of the axis text
#' compared to the base size.
#' @param axis_rel_x A numeric value for the relative size of the x-axis text
#' compared to the axis text size (which already depends on base size). This
#' argument allows to dissociate the size of the x and y axes' texts.
#' @param legend_rel A numeric value for the relative size of the legend text
#' compared to the base size.
#' @param legend_name A name for the legend. Default is "Problem category: ".
#' @param border_colour A colour for the border around the plot area.
#' @param violin_position_adjust A numeric value adjusting the space between the
#' violins and the dots, which is unnecessarily wide in the `superb` package's
#' defaults. Default is 0 (to remove all that space).
#' @param jitter_adjust A numeric value adjusting the width of the jitter
#' applied to individual data points, which is unnecessarily wide in the
#' `superb` package's. Default is 0.05 (to reduce the jitter drastically). Works
#' in conjunction with `jitter_width`.
#' @param ... Additional arguments passed to the [theme_pdf()] function for
#' customising the plot theme.
#'
#' @returns A ggplot2 object showing the distribution of the dependent variable
#' across categories and groups, means, and 95% confidence intervals.
#' @export
#'
#' @examples
#' df_expe <- get_clean_data()$df_expe
#' df_rt <- filter_trials_on_rt(df_expe)
#'
#' if (require("superb", quietly = TRUE)) {
#'   plot_superb_jitter(
#'     df_expe, accuracy, group_3,
#'     title = "VVIQ 3 groups", y_title = "Mean accuracy",
#'     base_size = 12
#'   )
#' }
#' if (require("superb", quietly = TRUE)) {
#'   plot_superb_raincloud(
#'     df_rt, rt_total, group_2,
#'     title = "VVIQ 2 groups", y_title = "Mean total RT (s)",
#'     base_size = 12
#'   )
#' }
#' if (require("superb", quietly = TRUE)) {
#'   plot_superb_categories(
#'     df_rt, rt_total, strategy_group,
#'     title = "Participants grouped by their use of a mental imagery strategy",
#'     y_title = "Mean total RT (s)",
#'     base_size = 12
#'   )
#' }
plot_superb_raincloud <- function(
    df,
    dvar,
    grouping,
    title = NULL,
    x_title = NULL,
    y_title = NULL,
    dot_size = 1.5,
    dot_alpha = 1,
    jitter_size = 1,
    jitter_alpha = 0.1,
    jitter_width = 0.2,
    jitter_height = 0,
    dodge_width = 0.4,
    errorbar_linewidth = 0.3,
    errorbar_h_width = 0,
    trim = TRUE,
    violin_width = 0.7,
    violin_linewidth = 0.2,
    exp_mult = 0,
    exp_add_left = 0,
    exp_add_right = 0.6,
    n_breaks = 10,
    visual_colour = palette.colors()[3],
    control_colour = palette.colors()[4],
    spatial_colour = palette.colors()[2],
    axis_rel = 0.9,
    axis_rel_x = 1,
    legend_rel = 1,
    legend_name = "Problem category: ",
    border_colour = "grey80",
    violin_position_adjust = 0,
    jitter_adjust = 0.05,
    ...
) {
  rlang::check_installed("superb")

  df_plot <- prepare_df_for_plotting(df, {{ dvar }})

  grouping_str <- as.character(rlang::ensym(grouping))
  formula    <- stats::as.formula(
    glue::glue("mean_dv ~ {grouping_str} * category")
  )

  p <- (
    superb::superb(
      formula,
      data = df_plot,
      plotLayout = "raincloud",
      pointParams = list(
        size  = dot_size,
        alpha = dot_alpha,
        position = ggplot2::position_dodge(width = dodge_width)
      ),
      jitterParams = list(
        size = jitter_size,
        alpha = jitter_alpha,
        position = ggplot2::position_jitterdodge(
          jitter.width  = jitter_width,
          jitter.height = jitter_height,
          dodge.width   = dodge_width
        )
      ),
      errorbarParams = list(
        linewidth = errorbar_linewidth,
        width = errorbar_h_width,
        position = ggplot2::position_dodge(width = dodge_width)
      ),
      violinParams = list(
        trim  = trim,
        width = violin_width,
        linewidth = violin_linewidth
      ),
    ) +
    ggplot2::labs(
      title = title,
      x = x_title,
      y = y_title
    ) +
    ggplot2::scale_x_discrete(
      expand = ggplot2::expansion(
        mult = exp_mult,
        add = c(exp_add_left, exp_add_right))
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_pretty(n_breaks),
    ) +
    ggplot2::scale_discrete_manual(
      name = legend_name,
      aesthetics = c("color", "fill"),
      values = c(
        Visual  = visual_colour,
        Control = control_colour,
        Spatial = spatial_colour
      )
    ) +
    ggplot2::scale_shape_manual(
      name = legend_name,
      values = c(19, 17, 18)
    ) +
    theme_pdf(
      base_theme = ggplot2::theme_minimal,
      axis_relative_size = axis_rel,
      axis_relative_x = axis_rel_x,
      legend_relative = legend_rel,
      # Custom theme arguments
      panel.grid.major.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = border_colour, fill = NA),
      ...
    )
  ) |> suppressMessages()

  p$layers[[1]]$constructor$position$x <- violin_position_adjust
  p$layers[[2]]$position$jitter.width <- jitter_adjust

  return(p)
}

#' @rdname plot_superb_raincloud
#' @export
plot_superb_jitter <- function(
    df,
    dvar,
    grouping,
    title = NULL,
    x_title = NULL,
    y_title = NULL,
    dot_size = 1.5,
    dot_alpha = 1,
    jitter_size = 0.75,
    jitter_alpha = 0.1,
    jitter_width = 0.1,
    jitter_height = 0.01,
    dodge_width = 0.5,
    errorbar_linewidth = 0.5,
    errorbar_h_width = 0,
    exp_mult = 0,
    exp_add = 0.6,
    visual_colour = palette.colors()[3],
    control_colour = palette.colors()[4],
    spatial_colour = palette.colors()[2],
    legend_name = "Problem category: ",
    axis_rel = 0.9,
    border_colour = "grey80",
    ...
) {
  rlang::check_installed("superb")

  df_plot <- prepare_df_for_plotting(df, {{ dvar }})

  grouping_str <- as.character(rlang::ensym(grouping))
  formula    <- stats::as.formula(
    glue::glue("mean_dv ~ {grouping_str} * category")
  )

  p <- (
    superb::superb(
      formula,
      data = df_plot,
      plotLayout = "pointjitter",
      pointParams = list(
        size  = dot_size,
        alpha = dot_alpha
      ),
      jitterParams = list(
        size  = jitter_size,
        alpha = jitter_alpha,
        position = ggplot2::position_jitterdodge(
          jitter.width  = jitter_width,
          jitter.height = jitter_height,
          dodge.width   = dodge_width
        )
      ),
      errorbarParams = list(
        width = errorbar_h_width,
        linewidth = errorbar_linewidth
      ),
    ) +
    ggplot2::labs(
      title = title,
      x = x_title,
      y = y_title
    ) +
    ggplot2::scale_x_discrete(
      expand = ggplot2::expansion(mult = exp_mult, add = exp_add)
    ) +
    ggplot2::scale_discrete_manual(
      name = legend_name,
      aesthetics = c("color", "fill"),
      values = c(
        Visual  = visual_colour,
        Control = control_colour,
        Spatial = spatial_colour
      )
    ) +
    theme_pdf(
      base_theme = ggplot2::theme_minimal,
      axis_relative_size = axis_rel,
      # Custom theme arguments
      panel.grid.major.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = border_colour, fill = NA),
      ...
    )
  ) |> suppressMessages()

  return(p)
}

#' @param exp_add An additional value for the [ggplot2::expansion()]
#' function to increase the space on the sides of the x-axis.
#' @param aph_colour A colour for the "Aphantasia" category dots and violins.
#' Default is the Okabe-Ito palette's green.
#'
#' @param hypo_colour A colour for the "Hypophantasia" category dots and
#' violins. Default is the Okabe-Ito palette's orange.
#' @param typ_colour A colour for the "Typical" category dots and violins.
#' Default is the Okabe-Ito palette's blue.
#' @param hyper_colour A colour for the "Hyperphantasia" category dots and
#' violins. Default is the Okabe-Ito palette's black.
#' @param no_visual_colour A colour for the "No visual strategy" category dots
#' and violins. Default is the Okabe-Ito palette's green.
#' @param visual_strat_colour A colour for the "Visual strategy user" category
#' dots and violins. Default is the Okabe-Ito palette's blue.
#' @param panel_maj_y_colour A colour for the major grid lines on the y-axis.
#' Default is "grey70".
#' @param panel_min_y_colour A colour for the minor grid lines on the y-axis.
#' Default is "grey70".
#'
#' @rdname plot_superb_raincloud
#' @export
plot_superb_categories <- function(
    df,
    dvar,
    grouping,
    title = NULL,
    x_title = NULL,
    y_title = NULL,
    dot_size = 2.25,
    dot_alpha = 1,
    errorbar_linewidth = 0.5,
    errorbar_h_width = 0,
    exp_mult = 0,
    exp_add = 0.6,
    axis_rel = 1,
    axis_rel_x = 1.2,
    legend_name = NULL,
    legend_rel = 1.2,
    aph_colour = palette.colors()[3],
    hypo_colour = palette.colors()[2],
    typ_colour = palette.colors()[4],
    hyper_colour = palette.colors()[1],
    no_visual_colour = palette.colors()[3],
    visual_strat_colour = palette.colors()[4],
    panel_maj_y_colour = "grey70",
    panel_min_y_colour = "grey70",
    ...
) {
  rlang::check_installed("superb")

  df_plot <- prepare_df_for_plotting(df, {{ dvar }})

  grouping_str <- as.character(rlang::ensym(grouping))
  formula    <- stats::as.formula(
    glue::glue("mean_dv ~ category * {grouping_str}")
  )

  p <- (
    superb::superb(
      formula,
      data = df_plot,
      plotLayout = "line",
      pointParams = list(
        size = dot_size,
        alpha = dot_alpha
      ),
      errorbarParams = list(
        width = errorbar_h_width,
        linewidth = errorbar_linewidth
      ),
    ) +
    ggplot2::labs(
      title = title,
      x = x_title,
      y = y_title
    ) +
    ggplot2::scale_x_discrete(
      expand = ggplot2::expansion(mult = exp_mult, add = exp_add)
    ) +
    ggplot2::scale_discrete_manual(
      name = legend_name,
      aesthetics = c("color", "fill"),
      values = c(
        Aphantasia = aph_colour,
        Hypophantasia = hypo_colour,
        Typical = typ_colour,
        Hyperphantasia = hyper_colour,
        "No visual strategy" = no_visual_colour,
        "Visual strategy user" = visual_strat_colour
      )
    ) +
    theme_pdf(
      base_theme = ggplot2::theme_minimal,
      axis_relative_size = axis_rel,
      axis_relative_x = axis_rel_x,
      legend_relative = legend_rel,
      # Custom theme arguments
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_line(color = panel_min_y_colour),
      panel.grid.major.y = ggplot2::element_line(color = panel_maj_y_colour),
      ...
    )
  ) |> suppressMessages()

  return(p)
}
