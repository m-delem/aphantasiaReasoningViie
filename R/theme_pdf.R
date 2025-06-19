#' Theme for elegant scientific vector figures
#'
#' @description
#' This function creates a ggplot2 theme based on the guidelines from the
#' [Nature Branded Research Journals](https://www.nature.com/documents/NRJs-guide-to-preparing-final-artwork.pdf). It takes a default ggplot2 theme as an argument
#' and applies mostly size adjustments to the text and other elements. The
#' option to use custom fonts from Google Fonts is also built-in, the chosen
#' default being "Montserrat". As recommended by the NRJ, the base text size
#' is set to 7pt and all other text sizes are inferior to 7pt. The only
#' exception in the defaults is the title size, which should not be used in
#' journal figures anyway. The rest of the theme was designed to make figures
#' look good when confined into restricted spaces (88mm width for one column
#' or 180mm for two columns).
#'
#' @param base_theme A ggplot2 theme function, without parentheses or quotes.
#' The default is `ggplot2::theme_classic`.
#' @param family A string with the name of the font family to be used in the
#' theme. If not found by `sysfonts::font_add_google()`, the font will reset to
#' the default "sans" font (close to Arial).
#' @param base_size A numeric value for the base font size in points. The
#' default is 7pt, as recommended by the NRJ.
#' @param base_line A numeric value for the base line size in points. The
#' default is 0.2pt to look good in small vector figures.
#' @param title_hjust A numeric value for the horizontal justification of the
#' plot title and subtitle. The default is 0.5, which centers the title.
#' @param axis_relative_size A numeric value for the relative size of the axis
#' text compared to the base size. The default is 0.85, which is slightly
#' smaller than the base size.
#' @param axis_relative_x,axis_relative_y A numeric value for the relative size
#' of the x/y-axis text compared to the axis text size (which already depends on
#' base size). These arguments allow to dissociate the size of the x and y axes'
#' texts. The defaults are 1.
#' @param legend_relative A numeric value for the relative size of the legend
#' text compared to the base size. The default is 1.
#' @param ... Additional arguments passed to [ggplot2::theme()] (which can
#' override the defaults set here).
#'
#' @returns A ggplot2 theme object with the specified settings.
#' @export
#'
#' @examples
#' p <-
#'   iris |>
#'   dplyr::mutate(Species = stringr::str_to_title(Species)) |>
#'   ggplot2::ggplot(
#'     ggplot2::aes(
#'       x = Sepal.Length,
#'       y = Sepal.Width,
#'       color = Species,
#'       fill  = Species,
#'       size = Petal.Width
#'    )
#'  ) +
#'  ggplot2::geom_jitter(alpha = 0.5) +
#'  ggplot2::labs(
#'   title    = "Iris Dataset",
#'   subtitle = "A classic dataset for testing",
#'   caption  = "Source: R's built-in iris dataset",
#'   x = "Sepal Length (cm)",
#'   y = "Sepal Width (cm)"
#'  ) +
#'  ggplot2::scale_color_manual(
#'   values = palette.colors(palette = "Okabe-Ito")[c(1, 2, 3)]
#'  ) +
#'  ggplot2::scale_size_continuous(range = c(1, 2.5))
#'
#'  p + ggplot2::facet_wrap(~ Species) + theme_pdf(ggplot2::theme_bw)
#'  p + theme_pdf(
#'   family = "Roboto Slab",
#'   base_size = 12,
#'   legend.position = "right"
#'  )
theme_pdf <- function(
    base_theme = ggplot2::theme_classic,
    family     = "Montserrat",
    base_size  = 7,
    base_line  = 0.2,
    title_hjust = 0.5,
    axis_relative_size = 0.85,
    axis_relative_x = 1,
    axis_relative_y = 1,
    legend_relative = 1,
    ...
) {
  try(sysfonts::font_add_google(family), silent = TRUE)
  showtext::showtext_auto()

  frac_size <- base_size / 1.5
  half_size <- base_size / 2

  elegant_theme <-
    base_theme(
      base_size   = base_size,
      base_family = family,
      base_line_size = base_line,
      base_rect_size = base_line
    ) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(
        size  = ggplot2::rel(1.2),
        hjust = title_hjust,
        face  = "plain",
      ),
      plot.subtitle = ggplot2::element_text(
        size  = ggplot2::rel(1),
        hjust = title_hjust,
        face  = "italic",
      ),
      plot.tag      = ggplot2::element_text(
        size  = ggplot2::rel(1.2),
        hjust = 0.5,
        face  = "plain",
      ),
      plot.caption  = ggplot2::element_text(
        size  = ggplot2::rel(0.9),
        hjust = 1,
        vjust = 0,
        face  = "italic",
      ),

      # Axes titles and text
      axis.title.x       = ggplot2::element_text(
        margin = ggplot2::margin(t = frac_size)
      ),
      axis.title.y       = ggplot2::element_text(
        margin = ggplot2::margin(r = frac_size)
      ),
      axis.title.x.top   = ggplot2::element_text(
        margin = ggplot2::margin(b = frac_size)
      ),
      axis.title.y.right = ggplot2::element_text(
        margin = ggplot2::margin(l = frac_size)
      ),

      axis.text =
        ggplot2::element_text(size = ggplot2::rel(axis_relative_size)),
      axis.text.x =
        ggplot2::element_text(size = ggplot2::rel(axis_relative_x)),
      axis.text.x.top =
        ggplot2::element_text(size = ggplot2::rel(axis_relative_x)),
      axis.text.y =
        ggplot2::element_text(size = ggplot2::rel(axis_relative_y)),
      axis.text.y.right =
        ggplot2::element_text(size = ggplot2::rel(axis_relative_y)),

      # Facets (using both "strip" and "panel" is confusing...)
      panel.spacing = grid::unit(frac_size, "pt"),
      strip.text    = ggplot2::element_text(
        size = ggplot2::rel(1),
        face = "bold",
        margin = ggplot2::margin(half_size, half_size, half_size, half_size)
      ),

      # Legends
      legend.text = ggplot2::element_text(size = ggplot2::rel(legend_relative)),
      legend.position = "top",
      # Removing the margin of individual legends in favour of overall
      # .box.margin or .spacing (between legends, between legend box and plot)
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      # Spacing between the whole legend box and the plot area
      legend.box.spacing = grid::unit(base_size / 4, "pt"),
      # Additional margin around the whole legend box
      legend.box.margin  = ggplot2::margin(
        half_size, half_size, half_size, half_size
      ),
      # Arrangement of multiple legends
      legend.box = "vertical",
      # Spacing around each separate legend (colour, fill, etc.)
      legend.spacing.x     = grid::unit(base_size * 2, "pt"),
      legend.spacing.y     = grid::unit(base_size, "pt"),
      # # Size of the "icon" in the key (dots, lines, etc.)
      legend.key.height    = grid::unit(base_size * 1.25, "pt"),
      legend.key.width     = grid::unit(base_size * 1.25, "pt"),
      # # Spacing around the whole keys (icon + text <----> icon + text)
      legend.key.spacing.x = grid::unit(base_size * 1.25, "pt"),
      legend.key.spacing.y = grid::unit(base_size / 2, "pt"),
    ) +
    ggplot2::theme(...)

  return(elegant_theme)
}
