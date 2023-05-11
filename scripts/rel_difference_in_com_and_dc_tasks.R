library(kit) # for pmean()
library(tidyverse)
areas <- read_csv("data/experiment_sequence.csv") |>
  filter(str_starts(task_type, "Com|DC")) |>
  mutate(
    rel_diff =
      abs(focal_area_1 - focal_area_2) /
        pmax(focal_area_1, focal_area_2)
  ) |>
  group_by(task_type) |>
  summarise(mean_rel_diff = mean(rel_diff))
