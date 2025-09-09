#' Plot model predictions
#'
#' @param model A fitted model object. This can be a `glmerMod` object from the
#' `lme4` package, a `glmmTMB` object from the `glmmTMB` package, or a
#' `brmsfit` object from the `brms` package.
#' @param limit A numeric value specifying the upper limit for the x-axis of the
#' plot. The default value is 80.
#' @param print_it Logical. If `TRUE`, the plot will be printed. Default is 
#' `FALSE`.
#' @param ... Additional arguments to be passed to the `ggplot2::theme()` 
#' function.
#' 
#' @returns A `ggplot` object representing the model predictions.
#' @export
plot_model_predictions <- function(
    model, 
    limit = 80, 
    print_it = FALSE,
    ...
) {
  if (class(model) == "glmerMod") {
    title <- "Frequentist GLMM (lme4 package) - Posterior Predictive Check"
  } else if (class(model) == "glmmTMB") {
    title <- "Frequentist GLMM (glmmTMB package) - Posterior Predictive Check"
  } else if (class(model) == "brmsfit") {
    title <- 
      "Bayesian Hierachical Model (brms package) - Posterior Predictive Check"
  } else title <- "Model Posterior predictive Check"
  
  p <- (
    model |> 
      performance::check_predictions(iterations = 100) |> 
      plot(linewidth = 0.2) + 
      ggplot2::scale_x_continuous(
        breaks = scales::breaks_pretty(15), 
        limits = c(0, limit),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(c(0, 0.05))) +
      see::scale_colour_okabeito(order = c(2, 3)) +
      ggplot2::labs(title = title, x = "Total RT") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(size = 7),
        plot.subtitle = ggplot2::element_text(size = 6, face = "italic"),
        
        panel.grid         = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(
          colour = "grey92", size = 0.1),
        panel.grid.minor.x = ggplot2::element_line(
          colour = "grey92", size = 0.1),
        
        axis.line.x  = ggplot2::element_line(colour = "grey92", size = 0.1),
        axis.ticks.x = ggplot2::element_line(colour = "grey92", size = 0.1),
        axis.title.x = ggplot2::element_text(
          size = 7, margin = ggplot2::margin(t = 7)),
        axis.text.x  = ggplot2::element_text(size = 6),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y  = ggplot2::element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.8, 0.8),
        legend.background = ggplot2::element_rect(
          fill = "white", linewidth = 0.1),
        legend.margin   = ggplot2::margin(1, 3, 1, 3),
        legend.key.size = grid::unit(10, "pt"),
        legend.text     = ggplot2::element_text(size = 5),
        legend.title    = ggplot2::element_blank(),
      ) +
      ggplot2::theme(...)
    ) |> 
    suppressMessages() |>
    suppressWarnings()
  # Silence the "out of bounds" warnings
  p$layers[[1]]$stat_params$na.rm <- TRUE
  
  if (print_it) print(p)
    
  return(p)
}