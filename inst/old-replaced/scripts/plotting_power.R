power_results <- readRDS(here::here("inst/power-analyses/power_glmer_420comb_350sims_16h44min.rds"))

p <- plot_power(power_results)

save_plot(
  p,
  "inst/figures/power_results.pdf",
  ncol = 2,
  height = 90,
  print_it = TRUE,
  verbose = TRUE
)
