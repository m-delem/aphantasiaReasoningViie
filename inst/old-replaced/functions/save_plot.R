#' Custom ggsave wrapper set with Nature's formatting guidelines
#'
#' @description
#' See:
#' https://www.nature.com/documents/NRJs-guide-to-preparing-final-artwork.pdf
#' It's pretty strict. A one column figure is 88 mm wide and a two column figure
#' is 180 mm wide. Depending on the length of the figure caption, there are
#' different maximum heights (see the PDF). Most figures types must be in vector
#' format to prevent quality loss when zooming in. Ever since I found these
#' guidelines, I use them for all figures, even if they are not for Nature...
#' Because it looks nice and I like it.
#'
#' @param plot     The plot to save.
#' @param path     The path to save the plot.
#' @param ncol     The number of columns for the plot. Either 1 (default) or 2.
#' @param height   The height of the plot in mm. Default is 90 mm.
#' @param width    Optional. The width of the plot in mm. If NULL (default), it
#'                 will be set to 88 mm for one-column figures and 180 mm for
#'                 two-column figures.
#' @param print_it Logical. Whether to print the plot in the console. Default is
#'                 FALSE.
#' @param verbose  Logical. Whether to print a message in the console. Default
#'                 is TRUE.
#' @param units    The units for the width and height. Default is "mm".
#' @param dpi      The resolution of the plot. Default is 600.
#' @param ...      Additional arguments passed to `ggsave()`.
#'
#' @returns Nothing. The function saves the plot to the specified path.
#' @export
save_plot <- function(
    plot,
    path,
    ncol     = 1,
    height   = 90,
    width    = NULL,
    print_it = FALSE,
    verbose  = TRUE,
    units    = "mm",
    dpi      = 600,
    ...
) {
  if (!is.null(width)) {
    width <- width
    colour <- "blue"
    shape  <- "Custom width"
  } else if (ncol == 1) {
    # Standard width for single column figures
    width  <- 88
    colour <- "green"
    shape  <- "One-column"
  } else if (ncol == 2) {
    # Standard width for double column figures
    width  <- 180
    colour <- "cyan"
    shape  <- "Two-column"
  } else stop(glue::glue_col("ncol must be {cyan 1} or {green 2}."))

  ggplot2::ggsave(
    filename = here::here(path),
    plot     = plot,
    width    = width,
    height   = height,
    units    = units,
    dpi      = dpi,
    ...
  )
  if (verbose) {
    print(glue::glue_col(
      "{magenta |-> {", colour, " {shape}} figure saved in {yellow {path}}.}"
    ))
  }
  if (print_it) print(plot) |> suppressMessages() |> suppressWarnings()
}
