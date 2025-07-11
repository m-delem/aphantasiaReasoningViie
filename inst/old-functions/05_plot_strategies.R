# Macro-function to plot the strategy questionnaire simulations ----------------
plot_strategies <- function(
    df, 
    group_var,
    type    = "reeder_og", # or "reeder_line", "reeder_bar", "reverse"
    palette = "reeder", # or "okabe_ito"
    title   = "not_specified",
    subtitle = NULL,
    print_it = FALSE,
    ...
) {
  # Helper function to get the data in a long format and recode it -------------
  pivot_and_recode <- function(df) {
    df_long <- 
      df |> 
      tidyr::pivot_longer(
        cols = Visual:Sensorimotor,
        names_to = "strategy",
        values_to = "score"
      ) |> 
      mutate(
        strategy = factor(strategy, levels = c(
          "Visual", "Spatial", "Verbal", "Semantic", "Sensorimotor"
        ))
      )
    
    return(df_long)
  }
  
  # Adding statistics to the dataframe -----------------------------------------
  df_long <- 
    left_join(
      df |> pivot_and_recode(),
      df |> 
        pivot_and_recode() |>
        group_by({{ group_var }}, strategy) |> 
        reframe(
          mean = mean(score) |> round(2),
          sd   = sd(score) |> round(2),
          se   = sd / 1.5 |> round(2)
        ),
      by = join_by({{ group_var }}, strategy)
    )

  # Choosing Reeder's palette or my faithful Okabe-Ito palette -----------------
  if (palette == "reeder") {
    colours <- c(
      Visual  = "#1b6096", 
      Verbal  = "#793183", 
      Semantic = "#db6100", 
      Spatial = "#318f2c", 
      Sensorimotor = "#d4629e"
    )
  } else if (palette == "okabe_ito") {
    colours <- c(
      Visual   = see::okabeito_colors(2) |> as.character(), 
      Verbal   = see::okabeito_colors(3) |> as.character(), 
      Semantic = see::okabeito_colors(4) |> as.character(), 
      Spatial  = see::okabeito_colors(1) |> as.character(), 
      Sensorimotor = see::okabeito_colors(7) |> as.character()
    )
  } else {
    stop(glue_col("palette must be '{cyan reeder}' or '{green okabe_ito}'"))
  }
  
  # Plot parameters, tailored for PDF (might add an alternative later) ---------
  txt_big  <- 7
  txt_mid  <- 6
  txt_smol <- 5
  lw_big   <- 0.2
  lw_smol  <- 0.1
  dot_alpha <- 0.5
  
  # The labels for the Likert scale on the y-axis ------------------------------
  scale_labels <- c(
    "Not used",
    "Used secondary to\nanother strategy",
    "Used equally with\nanother strategy",
    "Dominantly used",
    "Only used"
  )
  
  # Strategy scales ------------------------------------------------------------
  plot_scales <- list(
    scale_y_continuous(
      breaks = seq(1:5), 
      labels = scale_labels,
      limits = c(0.8, 5.2),
      expand = expansion(c(0.01, 0))
    ),
    scale_color_manual(
      values = colours,
      limits = c(
        "Visual", "Verbal", "Semantic", "Spatial", "Sensorimotor"
      )
    ),
    scale_fill_manual(
      values = colours,
      limits = c(
        "Visual", "Verbal", "Semantic", "Spatial", "Sensorimotor"
      )
    )
  )
  
  # My style -------------------------------------------------------------------
  plot_styling <- list(
    labs(x = NULL, y = NULL, colour = NULL, fill = NULL),
    theme_minimal(),
    theme(
      plot.title = element_text(
        size = txt_big, 
        margin = margin(b = 5), 
        hjust = 1,
        face = "bold"
      ),
      plot.subtitle = element_text(
        size = txt_mid,
        margin = margin(b = 5),
        hjust = 1
      ),
      text = element_text(size = txt_big),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(linewidth = lw_big, color = "grey87"),
      axis.text.x  = element_text(margin = margin(t = 7)),
      axis.text.y  = element_text(margin = margin(r = 7)),
      axis.ticks.x = element_line(linewidth = lw_big, color = "black"),
      axis.ticks.y = element_line(linewidth = lw_big, color = "grey87"),
    )
  )
  
  # Legend theme ---------------------------------------------------------------
  plot_legend <-
    theme(
      legend.position = "top",
      legend.justification = "right",
      legend.margin = margin(1, 3, 1, 3),
      legend.text = element_text(size = txt_smol),
      legend.key.width = unit(2, "mm"),
      legend.key.spacing.x = unit(2, "mm"),
      legend.box.background = element_rect(
        colour = "black", 
        linewidth = lw_big
      ),
    )
  
  # For Reeder's version: function to plot the data for a single group ---------
  # We need to split the different groups to allow `fct_reorder` to sort the
  # bars independently for each group.
  plot_strats_per_group <- function(
    df, subgroup, colours,
    box_alpha = 0.7, box_widths = 0.5,
    dot_size = 0.6, dot_jitter = 0.1,
    dodge_width = 1
  ) {
    df_subgroup <- df |> filter(Group == subgroup)
    
    p_subgroup <-     
      df_subgroup |>
      ggplot(
        aes(
          x = Group,
          y = score,
          color = forcats::fct_reorder(strategy, mean, .desc = TRUE),
          fill  = forcats::fct_reorder(strategy, mean, .desc = TRUE),
        )
      ) +
      geom_point(
        size  = dot_size,
        alpha = dot_alpha,
        position = position_jitterdodge(
          jitter.width  = 0,
          jitter.height = dot_jitter, 
          dodge.width   = dodge_width
        ),
        show.legend = FALSE,
        na.rm = TRUE
      ) +
      # Not enough variance in the subgroups to do boxplots yet
      geom_boxplot(
        data = df_subgroup |> select(!c(id, score)) |> distinct(),
        aes(
          x = Group,
          middle = mean,
          ymin  = mean - se,
          lower = mean - se,
          upper = mean + se,
          ymax  = mean + se,
          fill  = forcats::fct_reorder(strategy, mean, .desc = TRUE),
        ),
        inherit.aes = FALSE,
        na.rm = TRUE,
        stat = "identity",
        colour = "black",
        linewidth = lw_smol,
        alpha = box_alpha,
        position = position_dodge(width = dodge_width),
        width = box_widths
      ) +
      plot_scales +
      plot_styling +
      theme(plot.margin = margin(0, 0, 0, 0))
    
    return(p_subgroup)
  } 
  
  # ----------------------------------------------------------------------------
  # Reeder version -------------------------------------------------------------
  if (type == "reeder_og") {
    if (title == "not_specified") {
      title <- "Strategy scores per group (style from Reeder & Pounder, 2024)"
    }
    
    # Merging the plots of the four groups to get a single plot
    p <- 
      plot_strats_per_group(df_long, "Aphantasia", colours) +
      plot_strats_per_group(df_long, "Hypophantasia", colours) +
      plot_strats_per_group(df_long, "Typical", colours) +
      plot_strats_per_group(df_long, "Hyperphantasia", colours) +
      labs(title = title, subtitle = subtitle) +
      plot_layout(
        nrow = 1,
        guides = "collect",
        axes = "collect"
      ) & plot_legend
  
  # Alternative version n°1 with a smooth line connecting each strategy --------
  } else if (type == "reeder_line") {
    dot_indiv_size <- 0.4
    dot_range_size <- 0.1
    dot_jitter <- 0.1
    dodge_width <- 0.5
    if (title == "not_specified") {
      title <- "Strategy scores per group (alternative view)"
    }
    
    p <-
      df_long |>
      ggplot(
        aes(
          x = {{ group_var }},
          y = score,
          color = forcats::fct_reorder(strategy, mean, .desc = TRUE),
          fill  = forcats::fct_reorder(strategy, mean, .desc = TRUE),
        )
      ) +
      geom_point(
        size  = dot_indiv_size,
        alpha = dot_alpha,
        position = position_jitterdodge(
          jitter.width  = 0,
          jitter.height = dot_jitter, 
          dodge.width   = dodge_width
        ),
        show.legend = FALSE,
        na.rm = TRUE
      ) +
      # Connecting the points from the same strategy with a smooth curve
      geom_smooth(
        data = df_long |> select(!c(id, score)) |> distinct(),
        aes(
          y = mean,
          group = forcats::fct_reorder(strategy, mean, .desc = TRUE),
          color = forcats::fct_reorder(strategy, mean, .desc = TRUE)
        ),
        method = "loess",
        span = 1,
        se = FALSE,
        position = position_dodge(width = dodge_width),
        linewidth = lw_big,
        show.legend = FALSE,
        na.rm = TRUE
      ) +
      # Adding the points and SD representations for each strategy
      geom_pointrange(
        data = df_long |> select(!c(id, score)) |> distinct(),
        aes(
          y = mean,
          ymin = mean - se,
          ymax = mean + se,
        ),
        position = position_dodge(width = dodge_width),
        size = dot_range_size,
        linewidth = lw_smol,
        na.rm = TRUE
      ) +
      labs(title = title, subtitle = subtitle) +
      plot_scales +
      plot_styling +
      plot_legend
  
  # Reeder's second bar plot with the strategy proportions  --------------------
  } else if (type == "reeder_bar") {
    dot_indiv_size <- 0.4
    dot_range_size <- 0.1
    # dot_alpha  <- 0.05
    dot_jitter <- 0.1
    dodge_width <- 0.5
    if (title == "not_specified") {
      title <- "Proportion of strategy use per group"
    }
    group_order <- c("Aphantasia", "Hypophantasia", "Typical", "Hyperphantasia")
    
    p <-
      df_long |>
      mutate(Group = forcats::fct_relevel(Group, rev(group_order))) |> 
      select({{ group_var }}, strategy, score) |>
      group_by({{ group_var }}, strategy) |> 
      mutate(n = n()) |> 
      group_by({{ group_var }}, strategy, score) |> 
      mutate(perc = round(n() / n, 3)) |> 
      distinct() |> 
      ungroup() |>
      mutate(
        perc_label = paste0(round(perc * 100, 0), "%"),
        label_black = ifelse(score >= 4, perc_label, ""),
        label_white = ifelse(score <= 3, perc_label, ""),
      ) |> 
      
      ggplot(
        aes(
          x = {{ group_var }},
          y = perc,
          fill = factor(score),
          color = factor(score),
        )
      ) +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(
        aes(label = label_black),
        color = "black",
        position = position_fill(vjust = 0.5),
        size = 1.5,
      ) +
      geom_text(
        aes(label = label_white),
        color = "white",
        position = position_fill(vjust = 0.5),
        size = 1.5,
      ) +
      
      labs(title = title, subtitle = subtitle) +
      scale_colour_viridis_d(labels = scale_labels) +
      scale_fill_viridis_d(labels = scale_labels) +
      scale_x_discrete(position = "top") +
      scale_y_reverse(expand = c(0, 0)) +
      coord_flip() +
      facet_wrap(
        ~ strategy,
        ncol = 1,
        strip.position = "top",
        labeller = label_wrap_gen(width = 10)
      ) +
      plot_styling +
      plot_legend +
      theme(
        plot.title = element_text(hjust = 0),
        strip.text = element_text(size = txt_mid),
        legend.text.position = "top",
        legend.justification = "left",
        legend.key.height = unit(2, "mm"),
        legend.margin = margin(3, 3, 3, 3),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(margin = margin(r = 4)),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.spacing.x = unit(3, "mm"),
        panel.spacing.y = unit(1, "mm"),
      )
    
  # Alternative with the axis and colour mapping reversed ----------------------
  } else if (type == "reverse") {
    dot_indiv_size <- 0.4
    dot_range_size <- 0.3
    # dot_alpha  <- 0.05
    dot_jitter <- 0.1
    dodge_width <- 0.5
    if (title == "not_specified") {
      title <- "Group scores per strategy (x-axis / legend switched)"
    }
    df_long <- df_long |> 
      mutate(
        strategy = strategy |>
          forcats::fct_relevel("Spatial") |> 
          forcats::fct_relevel("Sensorimotor", after = 1)
      )
    
    p <-
      df_long |>
      ggplot(
        aes(
          x = strategy,
          y = score,
          color = {{ group_var }},
          fill  = {{ group_var }},
        )
      ) +
      geom_point(
        size  = dot_indiv_size,
        alpha = dot_alpha,
        position = position_jitterdodge(
          jitter.width  = 0,
          jitter.height = dot_jitter, 
          dodge.width   = dodge_width
        ),
        show.legend = FALSE,
        na.rm = TRUE
      ) +
      # Smooth curve connecting each group
      geom_smooth(
        data = df_long |> select(!c(id, score)) |> distinct(),
        aes(
          y = mean,
          group = {{ group_var }}
        ),
        method = "loess",
        span = 0.6,
        se = FALSE,
        position = position_dodge(width = dodge_width),
        linewidth = lw_big,
        show.legend = FALSE,
        na.rm = TRUE
      ) +
      # Means and SD representation
      see::geom_pointrange2(
        data = df_long |> select(!c(id, score)) |> distinct(),
        aes(
          y = mean,
          ymin = mean - se,
          ymax = mean + se,
        ),
        position = position_dodge(width = dodge_width),
        size = dot_range_size,
        linewidth = lw_smol,
        na.rm = TRUE
      ) +
      labs(title = title, subtitle = subtitle) +
      plot_scales +
      plot_styling +
      plot_legend +
      scale_discrete_phantasia()
    
  } else stop(glue_col(
    "type must be '{cyan reeder_og}', ",
    "'{green reeder_line}', {yellow reeder_bar} or '{blue reverse}'"
    ))
  
  p <- p + theme(...)
  
  if (print_it) print(p)
  return(p)
}
