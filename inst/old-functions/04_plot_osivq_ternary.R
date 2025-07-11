#' Plot the OSIVQ scores of clusters in a ternary diagram
#'
#' @param df A dataframe containing `osivq_object`, `osivq_spatial`,
#'`osivq_verbal` and `cluster` columns.
#' @param dot_size Size of the dots in the plot.
#' @param plot_it Logical. If TRUE, the plot will be printed immediately.
#' @param ... Additional arguments passed to the `ggplot2::theme()` function.
#'
#' @returns A ggplot object representing the OSIVQ scores in a ternary diagram.
#' @export
plot_osivq_ternary <- function(
    df, 
    dot_size = 1.5, 
    plot_it  = FALSE, 
    ...
) {
  p <-
    df |> 
    dplyr::select(
      Object  = osivq_object,
      Spatial = osivq_spatial,
      Verbal  = osivq_verbal
    ) |> 
    coda.plot::ternary_diagram(
      group  = df$cluster,
      center = TRUE,
      scale  = TRUE
    ) +
    scale_discrete_phantasia() +
    theme_phantasia(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.background     = ggplot2::element_rect(fill = "grey92"),
      panel.grid.major     = ggplot2::element_line(
        colour    = "white",
        linewidth = 0.2
      ),
      panel.grid.minor     = ggplot2::element_line(
        colour    = "grey98",
        linewidth = 0.1
      ),
      line                 = ggplot2::element_line(
        colour    = "grey82",
        linewidth = 0.2
      ),
      legend.position      = "bottom",
      legend.justification = "center",
      legend.margin        = ggplot2::margin(t = -20),
    ) +
    ggplot2::theme(...)
  
  p$layers[[2]]$geom$default_aes$size   <- dot_size
  p$layers[[2]]$geom$default_aes$alpha  <- 0.6
  p$layers[[2]]$geom$default_aes$stroke <- 0.2
  
  if (plot_it) plot(p)
  return(p)
}