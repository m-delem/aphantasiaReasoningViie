#' Plot the RTs by category only
#'
#' @param df A dataframe with the RTs and categories.
#' @param groups A string indicating the grouping variable. Default is "none".
#' @param version A string indicating the version of the plot. Either "cat",
#' "cat_group" or "group_cat". Default is "cat_group".
#' @param stat A function to compute the centrality measure. Default is median.
#' @param boxplots Logical. Whether to add boxplots to the plot. Default is 
#' TRUE.
#' @param violins Logical. Whether to add violins to the plot. Default is FALSE.
#' @param pointbars Logical. Whether to add pointbars to the plot. Default is 
#' FALSE.
#' @param view_table Logical. Whether to view the table of centrality measures.
#' Default is FALSE.
#' @param title A string indicating the title of the plot. Default is NULL.
#' @param subtitle A string indicating the subtitle of the plot. Default is 
#' NULL.
#' @param print_it Logical. Whether to print the plot. Default is FALSE.
#' @param y_max A numeric value indicating the maximum y-axis value. Default is 
#' 60.
#' @param ... Additional arguments passed to the `ggplot2::theme()` function.
#'
#' @returns A ggplot object with the RTs by category.
#' @export
plot_rt_violins <- function(
    df, 
    groups     = "none",
    version    = "cat_group",
    stat       = median, 
    boxplots   = TRUE,
    violins    = FALSE,
    pointbars  = FALSE,
    view_table = FALSE,
    title      = NULL,
    subtitle   = NULL,
    print_it   = FALSE,
    y_max      = 60,
    ...
) {
  # Helper function to reframe the df with a centrality measure ----------------
  get_centrality_wide <- function(df, centrality_stat, ...) {
    df_central <- 
      df |> 
      dplyr::group_by(...) |>
      dplyr::reframe(
        sd_total = sd(rt_total),
        se_total = sd_total / sqrt(length(rt_total)),
        rt_total = centrality_stat(rt_total),
        lower = rt_total - sd_total,
        upper = rt_total + sd_total,
      ) |> 
      dplyr::ungroup()
    
    return(df_central)
  }
  if (view_table) {
    View(get_centrality_wide(df, stat, id, {{ groups }}, category))
  }
  
  lw_smol <- 0.1
  lw_mid <- 0.2
  dot_cex <- 0.8
  dot_size <- 0.3
  dot_bees <- 0.6
  dot_alpha <- 0.2
  bees_alpha <- 0.3
  violin_alpha <- 0.3
  
  df <- df |> dplyr::mutate(category = stringr::str_to_title(category))
  
  # Version with categories only -----------------------------------------------
  if (version == "cat") {
    violin_width <- 0.7
    dodge <- 0.3
    
    p <- 
      df |> 
      ggplot2::ggplot(
        ggplot2::aes(
          x     = category, 
          y     = rt_total,
          fill  = category,
          color = category
        )
      ) +
      # Line connecting the mean per subject
      ggplot2::geom_line(
        data      = get_centrality_wide(df, stat, id, category),
        mapping   = ggplot2::aes(group = id),
        color     = "grey80",
        linewidth = lw_smol
      ) +
      # Mean per subject as dots
      ggbeeswarm::geom_beeswarm(
        data   = get_centrality_wide(df, stat, id, category),
        cex    = dot_cex,
        size   = dot_bees,
        alpha  = bees_alpha,
        stroke = 0,
        show.legend = FALSE
      ) +
      # Violin shapes for the distribution
      ggplot2::geom_violin(
        color          = "transparent",
        # draw_quantiles = c(0.25, 0.5, 0.75),
        # linewidth      = lw_smol,
        trim           = TRUE,
        alpha          = violin_alpha,
        width          = violin_width,
        na.rm          = TRUE,
        show.legend = FALSE
      ) + 
      # Line connecting the grand means
      ggplot2::geom_line(
        data      = get_centrality_wide(df, stat, category),
        mapping   = ggplot2::aes(group = 1),
        color     = "black",
        linewidth = lw_mid
      ) +
      # Group means +/- SE
      see::geom_pointrange2(
        data        = get_centrality_wide(df, stat, category),
        mapping     = ggplot2::aes(ymin = lower, ymax = upper),
        position    = ggplot2::position_dodge(width = dodge),
        linewidth   = lw_mid,
        size        = dot_size,
        show.legend = FALSE
      ) +
      labs(color = NULL, fill = NULL)
    
  # Version with categories on the x-axis and groups on the fill ---------------
  } else if (version == "cat_group") {
    violin_width <- 0.8
    dodge <- 0.5
    
    p <- 
      df |> 
      ggplot2::ggplot(
        ggplot2::aes(
          x     = category, 
          y     = rt_total,
          color = {{ groups }},
          fill  = {{ groups }}
        )
      ) +
      # Mean per subject as dots
      ggbeeswarm::geom_beeswarm(
        data        = get_centrality_wide(df, stat, id, category, {{ groups }}),
        dodge.width = dodge,
        cex         = dot_cex,
        size        = dot_bees,
        alpha       = bees_alpha,
        stroke      = 0,
        show.legend = FALSE
      ) + 
      ggplot2::labs(color = NULL, fill = NULL)
    
    if (boxplots) {
      p <- 
        p +
        ggplot2::geom_boxplot(
          position  = ggplot2::position_dodge(width = dodge),
          outlier.shape = NA,
          linewidth = lw_smol,
          alpha     = violin_alpha + .2,
          width     = 0.2,
          na.rm     = TRUE,
        )
    }
    if (violins) {
      p <-
        p +
        # Violin shapes for the distribution
        see::geom_violinhalf(
          colour   = "transparent",
          flip     = c(1, 3, 5),
          position = ggplot2::position_dodge(width = dodge),
          scale    = "width", 
          width    = 2,
          alpha    = violin_alpha,
          na.rm    = TRUE,
          trim     = TRUE
        )
    }
    if (pointbars) {
      p <- 
        p +
        # Group means +/- SE
        see::geom_pointrange2(
          data        = get_centrality_wide(df, stat, category, {{ groups }}),
          mapping     = ggplot2::aes(ymin = lower, ymax = upper),
          position    = ggplot2::position_dodge(width = dodge),
          linewidth   = lw_mid,
          size        = dot_size
        )
    }
  # Version with groups on the x-axis and categories on the fill ---------------
  } else if (version == "group_cat") {
    # violin_width <- 0.65
    violin_width <- 2
    dodge <- 0.5
    
    p <- 
      df |> 
      ggplot2::ggplot(
        ggplot2::aes(
          x      = {{ groups }}, 
          y      = rt_total, 
          fill   = category,
          colour = category
        )
      )
    
    if (violins) {
      dodge <- 0.8
      p <- 
        p +
        see::geom_violinhalf(
          flip     = c(1, 2, 3),
          # geom_violin(
          position  = ggplot2::position_dodge(width = dodge),
          color     = "transparent",
          trim      = TRUE,
          linewidth = lw_smol,
          alpha     = violin_alpha,
          width     = violin_width,
          na.rm     = TRUE
        ) +
        ggplot2::geom_line(
          data        = get_centrality_wide(df, stat, {{ groups }}, category),
          mapping     = ggplot2::aes(group = category),
          position    = ggplot2::position_dodge(width = dodge),
          linewidth   = lw_mid,
          alpha       = 0.4,
          show.legend = FALSE
        )
    }
    
    if (boxplots) {
      p <- 
        p +
        ggplot2::geom_boxplot(
          position  = ggplot2::position_dodge(width = dodge),
          outlier.shape = NA,
          linewidth = lw_smol,
          alpha     = violin_alpha + .2,
          width     = 0.2,
          na.rm     = TRUE,
        )
    }
    
    if (pointbars) {
      p <- 
        p + 
        # Group means +/- SE
        see::geom_pointrange2(
          data        = get_centrality_wide(df, stat, category, {{ groups }}),
          mapping     = ggplot2::aes(ymin = lower, ymax = upper),
          position    = ggplot2::position_dodge(width = dodge),
          linewidth   = lw_mid,
          size        = dot_size,
        )
    }
    
    p <-
      p +
      # Mean per subject as dots
      ggbeeswarm::geom_beeswarm(
        data        = get_centrality_wide(df, stat, id, category, {{ groups }}),
        dodge.width = dodge,
        cex         = dot_cex,
        size        = dot_bees,
        alpha       = bees_alpha,
        stroke      = 0,
        show.legend = FALSE
      )
    
  } else stop(print(glue::glue_col(
    "version must be '{blue cat}', '{cyan cat_group}' or '{green group_cat}'.\n"
    )))
  
  # Adding common geoms and styling --------------------------------------------
  p <- 
    p + 
    ggplot2::scale_y_continuous(
      limits = c(0, y_max),
      breaks = scales::breaks_pretty(n = 30), 
      expand = ggplot2::expansion(c(0, 0))
    ) + 
    ggplot2::labs(
      title    = title,
      subtitle = subtitle,
      y        = "Total RT (s)"
    ) +
    scale_discrete_phantasia() +
    theme_phantasia(
      panel.grid.major.x   = ggplot2::element_blank(),
      axis.title.x         = ggplot2::element_blank(),
      legend.position      = "bottom",
      legend.text.position = "bottom",
      legend.text          = ggplot2::element_text(size = 6),
      legend.key.size      = grid::unit(2, "mm"),
      legend.key.spacing.x = grid::unit(2, "mm"),
    ) +
    ggplot2::theme(...)
  
  if (print_it) print(p)
  return(p)
}