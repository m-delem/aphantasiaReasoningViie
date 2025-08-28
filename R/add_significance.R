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
#' @param color Color of the star label and line. Default is "black".
#' @param linetype Line type of the significance line. Default is "solid".
#'
#' @returns A list of ggplot2 layers that can be added to a ggplot object.
#' @export
add_significance <- function(
    df,
    size_star = 2.5,
    lw = 0.2,
    color = "black",
    linetype = "solid"
){
  list(
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(
        x     = .data$x_star,
        y     = .data$y_star,
        label = .data$stars
      ),
      size        = size_star,
      color       = color,
      inherit.aes = FALSE
    ),
    ggplot2::geom_segment(
      data = df,
      ggplot2::aes(
        x    = .data$x_line,
        xend = .data$x_line_end,
        y    = .data$y_line,
        yend = .data$y_line,
      ),
      color       = color,
      linewidth   = lw,
      linetype    = linetype,
      inherit.aes = FALSE
    )
  )
}
