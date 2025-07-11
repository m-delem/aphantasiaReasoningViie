#' Plot posterior distributions of RT differences
#'
#' @param model A fitted brms model object.
#' @param type  A string indicating the type of plot to create. Can be either
#'             "contrasts" or "parameters". Default is "contrasts".
#' @param print_it Logical. Whether to print the plot. Default is FALSE.
#' @param y_annot  Numeric. The y-coordinate for the annotation text. Default is 
#' 5.75.
#' @param ... Additional arguments passed to the `ggplot2::theme()` function.
#'
#' @returns A ggplot object.
#' @export
plot_rt_posterior <- function(
    model,
    type = "contrasts",
    print_it = FALSE,
    y_annot = 5.75,
    x_override = NULL,
    ...
) {
  if (type == "contrasts") {
    x_title <- "Difference in RT (s)"
    x_lims  <- c(-6.5, 6.5)
    that    <- "contrast"
    
  } else if (type == "parameters") {
    x_title <- "Difference from the intercept (back-transformed in seconds)"
    x_lims  <- c(-7.5, 7.5)
    that    <- "b_"
    
  } else stop(glue::glue_col(
    "type must be '{cyan contrasts}' or '{cyan parameters}'."
  ))
  
  if (!is.null(x_override)) x_lims <- x_override
  
  p <-
    model |> 
    get_long_draws() |> 
    
    ggplot2::ggplot(
      ggplot2::aes(
        x = value, 
        y = name
      )
    )+
    ggplot2::geom_vline(
      xintercept = bayestestR::rope_range(model), 
      linetype = "dashed", 
      linewidth = 0.2,
      color = viridis::viridis(100)[1]
    ) +
    ggdist::stat_slabinterval(
      mapping = aes(
        fill = ggplot2::after_stat(
          abs(x) < abs(bayestestR::rope_range(model)[1])
        )
      ),
      .width = c(.5, .75, .89, .95),
      point_size = 0.75,
      interval_size_range = c(0.1, 1),
      scale = 0.70,
      alpha = 0.3,
      color = viridis::viridis(100)[1],
      na.rm = TRUE,
      show.legend = FALSE,
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        viridis::viridis(100)[55], 
        viridis::viridis(100)[5]
      )
    ) +
    ggplot2::annotate(
      x = 0,
      y = y_annot,
      geom = "text",
      label = "Region of\npractical\nequivalence",,
      color = viridis::viridis(100)[1],
      size = 2,
    ) +
    ggdist::stat_interval(
      .width = c(.5, .66, .75, .89, .95),
      position = ggplot2::position_nudge(y = -0.1),
      linewidth = 1.25,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(
      limits = x_lims, 
      expand = c(0, 0),
      breaks = scales::breaks_pretty(20)
    ) +
    ggplot2::scale_y_discrete(position = "left") +
    ggplot2::scale_color_viridis_d(
      name = "Highest Density Interval level",
      direction = -1
    ) +
    ggplot2::labs(x = x_title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(
        linewidth = 0.1, color = "grey92"),
      panel.grid.minor.x = ggplot2::element_line(
        linewidth = 0.1, color = "grey92"),
      panel.grid.major.y = ggplot2::element_line(
        linewidth = 0.1, color = "grey92"),
      
      axis.line.x  = ggplot2::element_blank(),
      axis.line.y  = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(
        size = 7, margin = ggplot2::margin(t = 7)),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y  = ggplot2::element_text(size = 7),
      axis.text.x  = ggplot2::element_text(size = 6),
      axis.ticks   = ggplot2::element_blank(),
      
      legend.position = "top",
      legend.justification = "left",
      legend.title    = ggplot2::element_text(
        size = 7, margin = ggplot2::margin(r = 7)),
      legend.text     = ggplot2::element_text(size = 7),
      legend.margin   = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(5, "mm"),
      legend.spacing.x   = grid::unit(7, "mm"),
      legend.key.size    = grid::unit(4, "mm"),
      legend.key.spacing = grid::unit(0.5, "mm"),
      legend.key.spacing.x = grid::unit(3, "mm"),
    ) +
    ggplot2::theme(...)
  
  if (print_it) print(p)
  return(p)
}


#' Plot posterior distributions of strategies
#'
#' @param model_list A list of fitted brms model objects, one for each strategy.
#' @param print_it Logical. Whether to print the plot. Default is FALSE.
#' @param ... Additional arguments passed to the `ggplot2::theme()` function.
#'
#' @returns A ggplot object.
#' @export
plot_strats_posterior <- function(
    model_list,
    print_it = FALSE,
    ...
) {
  strategies <- c("Visual", "Spatial", "Verbal", "Semantic", "Sensorimotor")
  
  p <- 
    tibble::tibble(
      strat = forcats::fct_inorder(strategies),
      draws = purrr::map(model_list, brms::as_draws_df)
    ) |> 
    tidyr::unnest(draws) |> 
    dplyr::select(
      strat, 
      Aphantasia = b_Group_aphantasia,
      Hypophantasia = b_Group_hypophantasia,
      Hyperphantasia = b_Group_hyperphantasia,
    ) |>
    tidyr::pivot_longer(!strat) |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = value, 
        y = factor(name, levels = c(
          "Hyperphantasia",
          "Hypophantasia", 
          "Aphantasia"
        )),
      )
    ) +
    ggplot2::geom_vline(
      xintercept = 0, 
      linetype = "dashed", 
      linewidth = 0.2,
      color = viridis::viridis(100)[25]
    ) +
    ggdist::stat_slabinterval(
      .width = c(.5, .75, .89, .95, .99),
      point_size = 0.75,
      interval_size_range = c(0.1, 1),
      scale = 0.70,
      alpha = 0.3,
      fill  = viridis::viridis(100)[10],
      color = viridis::viridis(100)[1],
      na.rm = TRUE
    ) +
    ggdist::stat_interval(
      .width = c(.5, .75, .89, .95, .99),
      position = ggplot2::position_nudge(y = -0.15),
      linewidth = 1,
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~ strat, ncol = 1) +
    ggplot2::scale_x_continuous(
      limits = c(-4, 4), 
      expand = c(0, 0),
      breaks = scales::breaks_pretty(10)
    ) +
    ggplot2::scale_color_viridis_d(name = "Credible Interval level") +
    ggplot2::labs(x = "Difference with the typical group (model intercept)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(
        linewidth = 0.1, color = "grey92"),
      panel.grid.minor.x = ggplot2::element_line(
        linewidth = 0.1, color = "grey92"),
      panel.grid.major.y = ggplot2::element_blank(),
      
      strip.text = ggplot2::element_text(size = 7, face = "bold"),
      
      axis.line.x  = ggplot2::element_blank(),
      axis.line.y  = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(
        size = 7, margin = ggplot2::margin(t = 7)),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y  = ggplot2::element_text(size = 7),
      axis.text.x  = ggplot2::element_text(size = 6),
      axis.ticks   = ggplot2::element_blank(),
      
      legend.position = "top",
      legend.justification = "left",
      legend.title    = ggplot2::element_text(
        size = 7, margin = ggplot2::margin(r = 7)),
      legend.text     = ggplot2::element_text(size = 7),
      legend.margin   = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(5, "mm"),
      legend.spacing.x   = grid::unit(7, "mm"),
      legend.key.size    = grid::unit(4, "mm"),
      legend.key.spacing = grid::unit(0.5, "mm"),
      legend.key.spacing.x = grid::unit(3, "mm"),
    ) +
    ggplot2::theme(...)
  
  if (print_it) print(p)
  return(p)
}