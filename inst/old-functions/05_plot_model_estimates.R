#' Plot model-estimated means
#'
#' @param model A fitted model object. This can be a `glmerMod` object from the
#' `lme4` package, a `glmmTMB` object from the `glmmTMB` package, or a
#' `brmsfit` object from the `brms` package.
#' @param var_1 Name of the variable to plot on the x-axis.
#' @param var_2 Name of the variable to use as a grouping factor for the color 
#' and fill aesthetics.
#' @param print_it Logical. If `TRUE`, the plot will be printed. Default is 
#' `FALSE`.
#' @param ... Additional arguments to be passed to the `ggplot2::theme()`
#' function.
#'
#' @returns A `ggplot` object representing the model-estimated means.
#' @export
plot_model_estimates <- function(
    model, 
    var_1 = NULL,
    var_2 = NULL,
    print_it = FALSE,
    ...
) {
  var_1_str <- deparse(substitute(var_1))
  var_2_str <- deparse(substitute(var_2))
  if (var_1_str == "NULL" | var_2_str == "NULL") {
    stop(glue::glue_col(
      "Please provide {cyan var_1} and {green var_2} arguments. ",
      "If you want to plot only one variable, use the same variable for ",
      "both arguments."
    ))
  }
  if (class(model) %in% c("lmerMod", "glmerMod")) {
    title <- "Frequentist GLMM (lme4 package)"
    y_title <- "Mean RT ± SE (s)"
    
  } else if ((class(model) == "glmmTMB")) {
    title <- "Frequentist GLMM estimates (glmmTMB package)"
    y_title <- "Mean RT ± SE (s)"
  } else if ((class(model) == "brmsfit")) {
    title <- "Bayesian Hierachical Model estimates (brms package)"
    y_title <- "Median RT & CrI (s)"
  } else {
    stop(glue::glue_col(
      "model class must be either '{magenta lmerMod}', '{blue glmerMod}', ",
      "{green glmmTMB} or '{cyan brmsfit}'."
    ))
  }
  if (var_1_str == var_2_str) {
    bye <- var_1_str
  } else {
    bye <- c(var_1_str, var_2_str)
  }
  
  estimated_means <- custom_model_estimates(model, by = bye)
  if (!c("lower" %in% colnames(estimated_means))) {
    stop(glue::glue_col(
      "{red The model could not estimate a standard error of the mean} ",
      "(probably due to {yellow singularity}). ",
      "The means estimated by this model are not reliable and should not be ",
      " plotted."
    ))
  }
  
  p <- 
    estimated_means |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = {{ var_1 }},
        y = estimate,
        color = {{ var_2 }},
        fill = {{ var_2 }}
      )
    ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = lower, ymax = upper),
      position = ggplot2::position_dodge(width = 0.5),
      size = 0.4
    ) +
    ggplot2::labs(
      title = title,
      y = y_title
    ) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 8)) +
    see::scale_colour_okabeito(order = c(3, 1, 2, 4, 5, 6, 7, 8)) +
    ggplot2::labs(color = "Relations:", fill = "Relations:") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(size = 7),
      legend.text  = ggplot2::element_text(size = 7),
      plot.title   = ggplot2::element_text(
        size = 7, 
        margin = ggplot2::margin(b = 10)
      ),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        size = 7, 
        margin = ggplot2::margin(r = 7)
      ),
      axis.text.x  = ggplot2::element_text(size = 7),
      axis.text.y  = ggplot2::element_text(size = 6),
      panel.grid   = ggplot2::element_line(linewidth = 0.2),
      panel.grid.major.x = ggplot2::element_blank(),
    ) +
    ggplot2::theme(...)
  
  if (print_it) print(p)
  return(p)
}