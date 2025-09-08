library(dplyr)
library(tidyr)

df_strats_long |>
  group_by(
    strategy,
    # cluster,
    score
  ) |>
  reframe(n = n()) |>
  pivot_wider(
    names_from = score,
    values_from = n,
    values_fill = 0
  ) |>
  rowwise() |>
  mutate(
    users = sum(c_across(3:6)),
    # users = sum(c_across(4:7)),
    non_users = no_use,
    total = sum(c_across(2:6))
    # total = sum(c_across(3:7))
  )
