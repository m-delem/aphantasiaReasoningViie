#' Add significance label and line to a plot
#' 
#' @param df A dataframe containing one column per variable in the
#' desired aesthetics (x, y, colour, etc.) and the following columns:
#' - `x_star`: x position of the star label
#' - `y_star`: y position of the star label
#' - `stars`: the star label (e.g., "*", "**", "***")
#' - `x_line`: x position of the start of the line
#' - `x_line_end`: x position of the end of the line
#' - `y_line`: y position of the line
#' @param size_star Size of the star label. Default is 2.5.
#' @param lw Line width of the significance line. Default is 0.2.
#'
#' @returns A list of ggplot2 layers that can be added to a ggplot object.
#' @export
add_significance <- function(
    df, 
    size_star = 2.5, 
    lw = 0.2
){
  list(
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(
        x     = x_star,
        y     = y_star,
        label = stars
      ),
      size        = size_star,
      color       = "black",
      inherit.aes = FALSE
    ),
    ggplot2::geom_segment(
      data = df,
      ggplot2::aes(
        x    = x_line,
        xend = x_line_end,
        y    = y_line,
        yend = y_line,
      ),
      color       = "black",
      linewidth   = lw,
      inherit.aes = FALSE
    )
  )
}