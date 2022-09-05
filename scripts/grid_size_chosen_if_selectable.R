library(tidyverse)
source("scripts/Util.R")

# Function to import data for one treatment group
data_for_group <- function(group_number) {

  # Import questionnaire with response times, map familiarity and answers
  gp <-
    read_csv(str_c("data/group", group_number, ".csv")) |>
    slice(3:n()) |>
    select(matches(c(
      "^CluAU\\d$",
      "^ComAU\\d(a|b_1)$",
      "^ComZo\\d(a|b_1)$",
      "^DCAU\\d[ab]$",
      "^DCZo\\d[ab]$",
      "^EstAU\\d[ab]$",
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
        str_detect(task_part, "^(ComAU\\db)|(ComZo\\db)|(DCAU\\db)|(DCZb\\db)|(EstAU)"),
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
    

  View(gp)
}
grid_info <- read_csv("data/grid_size_chosen_if_selectable.csv") |>
  pivot_longer(-1:-2, names_to = "task_type", values_to = "grid_size")
