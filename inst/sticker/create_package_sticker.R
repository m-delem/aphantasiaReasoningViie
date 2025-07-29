raw_cropped <- cropcircles::hex_crop(
  images = "inst/sticker/sticker_art.jpg",
  border_colour = "#394049",
  border_size = 7
)

sysfonts::font_add_google("Montserrat")
showtext::showtext_auto()

p <-
  ggplot2::ggplot() +
  ggpath::geom_from_path(ggplot2::aes(.5, .5, path = raw_cropped)) +
  ggplot2::annotate(
    "text",
    x = .04,
    y = .44,
    label = "aphantasia\nReasoning\nVIIE",
    color = "white",
    family = "Montserrat",
    fontface = "bold",
    lineheight = 0.25,
    size = 38,
    hjust = 0
  ) +
  ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  ggplot2::theme_void()

ggplot2::ggsave(
  filename = "inst/sticker/sticker_hex.png",
  plot = p,
  width = 5.18,
  height = 6,
  dpi = 300
)

usethis::use_logo(here::here("inst/sticker/sticker_hex.png"))
