# Functions to plot the simulated DLC RT data ----------------------------------

# As violins -------------------------------------------------------------------
plot_rt_simu_violins <- function(df_simu, plot_size = "standard") {
  if (plot_size == "standard") {
    lw_smol  <- 0.5
    lw_mid   <- 1
    dot_size <- 0.5
    txt_big  <- 12
    txt_mid  <- 10
    txt_smol <- 9
  } else if (plot_size == "pdf") {
    lw_smol  <- 0.3
    lw_mid   <- 0.4
    dot_size <- 0.05
    txt_big  <- 7
    txt_mid  <- 6
    txt_smol <- 5
  } else stop("plot_size must be 'standard' or 'pdf'")
  
  # Function to calculate standard error
  # std_err <- function(x) sd(x) / sqrt(length(x)) # Too small
  std_err <- function(x) sd(x) / 4 # For visualisation purposes
  
  # Violin plots
  p_violins <- 
    ggplot() +
    see::geom_violinhalf(
      data = df_simu |> filter(rt < 40), 
      aes(x = category, y = rt, fill = group),
      colour = "transparent",
      flip = c(1, 3, 5),
      position = position_dodge(width = 0.3), 
      scale = "width",
      alpha = 0.5,
      width = 2.15
    ) +
    geom_pointrange(
      data = df_simu |> 
        group_by(group, category) |> 
        reframe(
          mean = mean(rt), 
          upper = mean + std_err(rt), 
          lower = mean - std_err(rt)
        ),
      aes(
        x = category, y = mean, 
        ymin = lower, ymax = upper,
        colour = group, fill = group),
      position = position_dodge(width = 0.3),
      linewidth = lw_mid,
      size = dot_size,
      show.legend = FALSE
    ) +
    see::scale_color_okabeito() +
    see::scale_fill_okabeito() +
    scale_y_continuous(
      breaks = scales::breaks_pretty(15),
      expand = expansion(0.025)
    ) +
    labs(color = NULL, fill = NULL, x = NULL, y = "Total RT (s)") +
    theme_minimal() +
    theme(
      plot.title = element_text(margin = margin(20, 0, 15, 0)),
      text = element_text(size = txt_big),
      axis.title.y = element_text(margin = margin(0, 10, 0, 5)),
      axis.text.y = element_text(size = txt_smol),
      axis.text.x = element_text(size = txt_big),
      panel.grid = element_line(linewidth = lw_smol),
      panel.grid.major.x = element_blank(),
      
      legend.position = "top",
      legend.margin = margin(0, 0, 0, 0),
      legend.key.size = unit(3, "mm"),
    )
  
  return(p_violins)
}

# Distribution comparisons with the real data ----------------------------------
plot_rt_simu_distribs <- function(
    df_simu,
    type = "category",
    n = 20
) {
  if (type == "subjects") {
    p_distribs <-
      df_simu |>
      group_by(id) |>
      nest() |> 
      ungroup() |> 
      sample_n(n) |>
      unnest(cols = data) |> 
      ggplot() +
      geom_density(
        aes(x = rt, colour = id),
        linewidth = 0.5
      ) +
      scale_color_viridis_d()
  } else if (type == "category") {
    p_distribs <-
      ggplot() +
      geom_density(
        data = df_simu |> filter(category == "Visual"),
        aes(x = rt),
        colour = okabeito_colors(1),
        linewidth = 1
      ) +
      geom_density(
        data = df_simu |> filter(category == "Spatial"),
        aes(x = rt),
        colour = okabeito_colors(2),
        linewidth = 1
      ) +
      geom_density(
        data = df_simu |> filter(category == "Control"),
        aes(x = rt),
        colour = okabeito_colors(3),
        linewidth = 1
      ) +
      annotate(
        "text", x = 30, y = 0.07,
        label = "Simulated visual",
        colour = okabeito_colors(1)
      ) +
      annotate(
        "text", x = 30, y = 0.055,
        label = "Simulated spatial",
        colour = okabeito_colors(2)
      ) +
      annotate(
        "text", x = 30, y = 0.04,
        label = "Simulated control",
        colour = okabeito_colors(3)
      )
  } else stop("type must be 'subjects' or 'category'")
  
  p_distribs <- 
    p_distribs +
    scale_x_continuous(
      breaks = breaks_pretty(20),
      oob = scales::squish,
      expand = expansion(c(0.025))
    ) +
    scale_y_continuous(expand = expansion(c(0, 0.025))) +
    labs(
      x = "Total RT (s)", y = "Density"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_line(colour = "grey82")
    )
  
  return(p_distribs)
}

