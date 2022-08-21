library(tidyverse)

gp1 <- read_csv("data/group1.csv") |>
  slice(3:n()) |>
  select(ends_with("Ti_Page Submit")) |>
  mutate(participant_id = row_number()) |>
  pivot_longer(-participant_id, names_to = "task_type", values_to = "time") |>
  mutate(
    task_type = str_replace(task_type, "(\\d).?Ti_Page\\sSubmit$", "\\1"),
    task_number = str_sub(task_type, start = -1),
    task_type = str_sub(task_type, end = -2),
    time = as.numeric(time)
  ) |>
  group_by(participant_id, task_type, task_number) |>
  summarise(time = sum(time))

# Remove wrong or missing answers