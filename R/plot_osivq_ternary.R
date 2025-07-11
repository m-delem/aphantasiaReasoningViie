#' Plot the OSIVQ scores of clusters in a ternary diagram
#'
#' @param df A dataframe containing `osivq_object`, `osivq_spatial`,
#'`osivq_verbal` and `cluster` columns.
#' @param dot_size Size of the dots in the plot.
#' @param plot_it Logical. If TRUE, the plot will be printed immediately.
#' @param colours A vector of colours to use for the clusters in the plot passed
#' to the [ggplot2::scale_discrete_manual()] function. Default is
#' [palette.colors()].
#' @param ... Additional arguments passed to the [theme_pdf()] function.
#'
#' @returns A ggplot object representing the OSIVQ scores in a ternary diagram.
#' @export
plot_osivq_ternary <- function(
    df,
    dot_size = 1.5,
    plot_it  = FALSE,
    colours = palette.colors(),
    ...
) {
  rlang::check_installed("coda.plot")

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
    ggplot2::scale_discrete_manual(
      name = NULL,
      aesthetics = c("color", "fill"),
      values = colours
    ) +
    theme_pdf(...) +
    ggplot2::theme(legend.margin = ggplot2::margin(b = -15))

  p$layers[[2]]$geom$default_aes$size   <- dot_size
  p$layers[[2]]$geom$default_aes$alpha  <- 0.6
  p$layers[[2]]$geom$default_aes$stroke <- 0.2

  if (plot_it) plot(p)
  return(p)
}
