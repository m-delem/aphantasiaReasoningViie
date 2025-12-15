devtools::load_all()
library(patchwork)
library(superb)

df_expe <- get_clustered_data("experiment")
df_rt <- df_expe |> filter_trials_on_rt()
df_survey <- get_clustered_data("survey")
df_strats_long <- pivot_strategies_longer(df_survey, ordered = TRUE)

# Accuracy ---------------
star_size <- 2.25

pa1 <-
  plot_superb_jitter(
    df_expe, accuracy, group_2,
    title = "VVIQ 2 groups", y_title = "Mean accuracy"
  )
pa2 <-
  plot_superb_jitter(
    df_expe, accuracy, group_3,
    title = "VVIQ 3 groups", y_title = "Mean accuracy"
  )
pa3 <-
  plot_superb_jitter(
    df_expe, accuracy, cluster,
    title = "OSIVQ clusters", y_title = "Mean accuracy"
  )
pa <- pa1 + pa2 + pa3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

save_ggplot(
  pa,
  "inst/manuscript/figures/accuracy.pdf",
  ncol = 2,
  height = 75
)

# RT ---------------------
pr1 <-
  plot_superb_raincloud(
    df_rt, rt_total, group_2,
    title = "VVIQ 2 groups", y_title = "Mean total RT (s)"
  )
pr2 <-
  plot_superb_raincloud(
    df_rt, rt_total, group_3,
    title = "VVIQ 3 groups", y_title = "Mean total RT (s)"
  )
pr3 <-
  plot_superb_raincloud(
    df_rt, rt_total, cluster,
    title = "OSIVQ clusters", y_title = "Mean total RT (s)"
  )

pr <- pr1 + pr2 + pr3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

save_ggplot(
  pr,
  "inst/manuscript/figures/rt.pdf",
  ncol = 2,
  height = 90
)

# Strategies ------------
# Plotting proportions
pb1 <- plot_strategies_barplot(df_strats_long, group_2, title = "VVIQ 2 groups")
pb2 <- plot_strategies_barplot(df_strats_long, group_3, title = "VVIQ 3 groups")
pb3 <- plot_strategies_barplot(df_strats_long, cluster, title = "OSIVQ clusters")

pb <-
  pb1 + pb2 + pb3 +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

save_ggplot(
  pb,
  "inst/manuscript/figures/strat_proportions.pdf",
  ncol = 2,
  height = 140
)

# Plotting scores
size <- 3

ps1 <-
  plot_strategies_scores(
    df_strats_long,
    grouping = group_2,
    title = "VVIQ 2 groups"
  )
ps2 <-
  plot_strategies_scores(
    df_strats_long,
    grouping = group_3,
    title = "VVIQ 3 groups"
  )
ps3 <-
  plot_strategies_scores(
    df_strats_long,
    grouping = cluster,
    title = "OSIVQ clusters"
  )

ps <-
  ps1 + ps2 + ps3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

save_ggplot(
  ps,
  "inst/manuscript/figures/strat_scores.pdf",
  ncol = 2,
  height = 75
)

# Non-linear --------------------
pnl1 <-
  plot_nl(
    nl_predictions$vviq_2, title = "VVIQ 2 groups"
  )
pnl2 <-
  plot_nl(
    nl_predictions$vviq_3,
    title = "VVIQ 3 groups",
    plot.margin = ggplot2::margin(t = 10)
  )
pnl3 <-
  plot_nl(
    nl_predictions$osivq,
    title = "OSIVQ clusters",
    plot.margin = ggplot2::margin(t = 10)
  )

pnl <- pnl1 / pnl2 / pnl3 +
  patchwork::plot_layout(axes = "collect", guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

save_ggplot(
  pnl,
  "inst/manuscript/figures/nl.pdf",
  ncol = 2,
  height = 180
)
