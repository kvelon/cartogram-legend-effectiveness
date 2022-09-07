library(tidyverse)
source("scripts/Util.R")

# Function to import data for one treatment group
data_for_group <- function(group_number) {

  # Import questionnaire with participants' answers
  gp <-
    read_csv(str_c("data/group", group_number, ".csv")) |>
    slice(3:n()) |>
    select(matches(c(
      "^CluAU\\d$",
      "^ComAU\\d(a|b_1)$",
      "^ComZo\\d(a|b_1)$",
      "^DCAU\\d[ab]$",
      "^DCZo\\d[ab]$",
      "^EstAU\\d$",
      "^FTAU\\d$"
    ))) |>
    mutate(participant_id = row_number()) |>
    pivot_longer(
      -participant_id,
      names_to = "task_part",
      values_to = "answer"
    ) |>
    mutate(
      task_part = str_replace(task_part, "_1", ""),
      answer = str_remove(answer, " brl"),
      numeric_answer = get_double_column(if_else(
        str_detect(
          task_part,
          "^(ComAU\\db)|(ComZo\\db)|(DCAU\\db)|(DCZb\\db)|(EstAU)"
        ),
        answer,
        NA_character_
      )),
      string_answer = if_else(
        !str_starts(task_part, "ComAU\\db|ComZo\\db|DCAU\\db|DCZb\\db|EstAU"),
        answer,
        NA_character_
      )
    )

  # Remove wrong and missing answers
  answer_key <- read_csv("data/answer_key.csv") # Correct answer options
  gp <-
    gp |>
    left_join(answer_key, by = "task_part") |>
    mutate(
      task = str_replace(task_part, "^(.*\\d).*", "\\1"),
      part = str_replace(task_part, "^.*\\d(.*)", "\\1"),
      keep_answer = case_when(
        is.na(correct_string) & is.na(numeric_answer) ~ FALSE,
        is.na(correct_number) & string_answer != correct_string ~ FALSE,
        !is.na(correct_number) & !is.na(numeric_answer) ~ TRUE,
        !is.na(correct_string) & string_answer == correct_string ~ TRUE
      )
    ) |>
    group_by(participant_id, task) |>
    summarise(keep_answer = all(keep_answer)) |>
    ungroup(participant_id, task) |>
    mutate(group = group_number)

  # Add information about treatment
  experiment_sequence <-
    read_csv("data/experiment_sequence.csv") |>
    mutate(task = str_c(task_type, task_number)) |>
    select(task, treatment = str_c("group_", group_number))
  gp <-
    gp |>
    left_join(experiment_sequence, by = "task") |>
    filter(treatment == "SeLG") |>
    mutate(task_type = str_sub(task, end = -2)) |>
    select(group, participant_id, task_type, keep_answer)

  # Add information about chosen grid size
  grid_size <- read_csv("data/grid_size_chosen_if_selectable.csv") |>
    pivot_longer(-1:-2, names_to = "task_type", values_to = "grid_size") |>
    mutate(task_type = recode(
      task_type,
      "Cluster" = "CluAU",
      "Compare AU" = "ComAU",
      "Compare Zone" = "ComZo",
      "Detect Change in AU" = "DCAU",
      "Detect Change in Zone" = "DCZo",
      "Estimate AU" = "EstAU",
      "Find Top" = "FTAU"
    )) |>
    rename(group = Group, participant_id = Participant)
  gp |>
    left_join(grid_size, by = c("group", "participant_id", "task_type")) |>
    select(-group, -participant_id)
}
data_for_analysis <-
  map_dfr(1:4, ~ bind_rows(data_for_group(.))) |>
  mutate(
    grid_size_1 = str_remove(grid_size, "/.*$"),
    grid_size_2 = if_else(
      str_detect(grid_size, "/"),
      str_remove(grid_size, "^.*/"),
      NA_character_
    ),
  ) |>
  select(-grid_size) |>
  pivot_longer(
    cols = matches("grid_size_\\d"),
    names_to = NULL,
    values_to = "grid_size"
  ) |>
  filter(!is.na(grid_size)) |>
  count(keep_answer, grid_size) |>
  pivot_wider(names_from = keep_answer, values_from = n) |>
  rename(false = `FALSE`, true = `TRUE`) |>
  mutate(
    total = false + true,
    percent = 100 * total / sum(total)
  )
print(data_for_analysis)
print(fisher.test(data_for_analysis[c("false", "true")]))
