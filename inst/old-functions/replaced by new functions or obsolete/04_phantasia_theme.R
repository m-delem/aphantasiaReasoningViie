#' My ggplot2 theme for this project
#' 
#' @description 
#' I encapsulated this in a function to make it easy to apply a lot of theme 
#' options to the numerous plots I designed. In particular, I often need to 
#' reduce the size of the font, lines, etc. to render nicely in PDF format, 
#' which is a very good option for beautiful vector graphics.
#' 
#' 
#' @param montserrat Logical. If `TRUE`, the theme will use the Montserrat font 
#' from Google Fonts. If `FALSE`, the theme will use the default font.
#' @param ... Additional arguments passed to the `theme()` function.
#'
#' @returns A ggplot2 theme object with the specified options.
#' @export
theme_phantasia <- function(
    montserrat = TRUE, 
    ...
) {
  rlang::check_installed("sysfonts")
  rlang::check_installed("showtext")
  
  if (montserrat) {
    # Adding Google's Montserrat font
    sysfonts::font_add_google("Montserrat")
    showtext::showtext_auto()
    family <- "Montserrat"
  } else family <- NULL
  
  phantasia_theme <-
    ggplot2::theme_minimal(
      base_size = 7,
      base_family = family
    ) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(
        size   = ggplot2::rel(1.1),
        hjust  = 0.5,
        face   = "plain",
        margin = ggplot2::margin(b = 3.5)
      ),
      plot.subtitle   = ggplot2::element_text(
        hjust  = 0.5,
        face   = "italic",
        margin = ggplot2::margin(b = 3.5)
      ),
      panel.grid      = ggplot2::element_line(
        linewidth = 0.2, 
        color = "grey92"
      ),
      axis.ticks      = ggplot2::element_line(
        linewidth = 0.2, 
        color = "grey92"
      ),
      # Axes titles and text ---------------
      axis.title.x    = ggplot2::element_text(
        margin = ggplot2::margin(t = 2, unit = "mm")
      ),
      axis.title.y    = ggplot2::element_text(
        margin = ggplot2::margin(r = 2, unit = "mm")
      ),
      axis.text       = ggplot2::element_text(size = ggplot2::rel(1)),
      axis.text.x     = ggplot2::element_text(
        size   = ggplot2::rel(1),
        margin = ggplot2::margin(t = 3.5)
      ),
      axis.text.x.top = ggplot2::element_text(
        size   = ggplot2::rel(1),
        margin = ggplot2::margin(b = 3.5)
      ),
      axis.text.y     = ggplot2::element_text(
        size   = ggplot2::rel(0.8),
        margin = ggplot2::margin(r = 3.5)
      ),
      axis.text.y.right = ggplot2::element_text(
        size   = ggplot2::rel(0.8),
        margin = ggplot2::margin(l = 3.5)
      ),
      # Facets (reusing the term "panel" is confusing...)
      strip.text      = ggplot2::element_text(
        size = ggplot2::rel(1), face = "bold"
      ),
      panel.spacing   = grid::unit(3, "mm"),
      # Legends -------------------
      legend.text     = ggplot2::element_text(
        size = ggplot2::rel(1)
      ),
      legend.position = "top",
      # Arrangement of multiple legends
      legend.box      = "vertical",
      # Spacing between the whole legend box and the plot area
      legend.box.margin    = ggplot2::margin(2, 0, 0, 0, unit = "mm"),
      legend.box.spacing   = grid::unit(3, "mm"),
      legend.box.just      = "center",
      legend.justification = "center",
      # Spacing around each separate legend (colour, fill, etc.)
      legend.margin        = ggplot2::margin(0, 0, 0, 0, unit = "mm"),
      legend.spacing.x     = grid::unit(4, "mm"),
      legend.spacing.y     = grid::unit(2, "mm"),
      # # Size of the "icon" in the key (dots, lines, etc.)
      legend.key.height    = grid::unit(2, "mm"),
      legend.key.width     = grid::unit(4, "mm"),
      # # Spacing around the whole keys (icons + text)
      legend.key.spacing.x = grid::unit(3, "mm"),
      legend.key.spacing.y = grid::unit(1, "mm"),
    ) +
    ggplot2::theme(...)
  
  return(phantasia_theme)

}
