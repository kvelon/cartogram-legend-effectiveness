library(tidyverse)
source("scripts/Util.R")

data_for_group <- function(group_number) {

  # Import questionnaire with response times and answers
  gp <-
    read_csv(str_c("data/group", group_number, ".csv")) |>
    slice(3:n()) |>
    mutate(participant_id = row_number())

  # Make separate tibbles for response times, string answers and numeric
  # answers. We start with response times.
  times <-
    gp |>
    select(participant_id, ends_with("Ti_Page Submit")) |>
    pivot_longer(
      -participant_id,
      names_to = "task_part",
      values_to = "time"
    ) |>
    mutate(
      task_part = str_replace(task_part, "Ti_Page\\sSubmit$", ""),
      task_part = str_replace(task_part, "^EstAu", "EstAU"),
      time = as.numeric(time)
    )

  # String answers
  string_task_parts <- list(
    task_type = c("CluAU", "ComAU", "ComZo", "DCAU", "DCZo", "FTAU"),
    task_number = 1:4
  ) |>
    cross() |>
    map_chr(lift(str_c))
  string_task_parts <- if_else(
    str_starts(string_task_parts, "^Com|DC"),
    str_c(string_task_parts, "a"),
    string_task_parts
  )
  string_answers <-
    gp |>
    select(participant_id, string_task_parts) |>
    pivot_longer(
      -participant_id,
      names_to = "task_part",
      values_to = "string_answer"
    )

  # Numeric answers
  numeric_task_parts <- list(
    task_type = c("ComAU", "ComZo", "DCAU", "DCZo", "EstAU"),
    task_number = 1:4
  ) |>
    cross() |>
    map_chr(lift(str_c))
  numeric_task_parts <- case_when(
    str_starts(numeric_task_parts, "DC") ~ str_c(numeric_task_parts, "b"),
    str_starts(numeric_task_parts, "Com") ~ str_c(numeric_task_parts, "b_1"),
    TRUE ~ numeric_task_parts
  )
  million_string_to_number <- function(s) {
    case_when(
      is.na(s) ~ NA_real_,
      str_ends(s, "[mM]ill*ion") ~
        as.character(as.numeric(str_replace(s, "\\s?[mM]ill*ion", "")) * 1e6),
      TRUE ~ s
    )
  }
  numeric_answers <-
    gp |>
    select(participant_id, numeric_task_parts) |>
    pivot_longer(
      -participant_id,
      names_to = "task_part",
      values_to = "numeric_answer"
    ) |>
    mutate(
      task_part = str_replace(task_part, "_1", ""),
      numeric_answer = str_remove(numeric_answer, " brl"),
      numeric_answer = get_double_column(numeric_answer)
    )

  # Join the information about response times, numeric answers and string
  # answers
  gp <-
    times |>
    left_join(string_answers, by = c("participant_id", "task_part")) |>
    left_join(numeric_answers, by = c("participant_id", "task_part"))

  # Remove wrong or missing answers
  answer_key <- read_csv("data/answer_key.csv")
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
    summarise(
      time = sum(time),
      keep_answer = all(keep_answer)
    ) |>
    filter(keep_answer) |>
    mutate(group = group_number) |>
    select(group, participant_id, task, time)
}

experiment_sequence <-
  read_csv("data/experiment_sequence.csv") |>
  pivot_longer(
    starts_with("group"),
    names_to = "group",
    names_pattern = "^group_(\\d)$",
    names_transform = as.numeric,
    values_to = "treatment"
  )
experimental_data <-
  map_dfr(1:4, ~ bind_rows(data_for_group(.))) |>
  mutate(
    task_type = str_replace(task, "^(.*)\\d$", "\\1"),
    task_number = as.numeric(str_replace(task, "^.*(\\d)$", "\\1"))
  ) |>
  select(-task) |>
  left_join(experiment_sequence, by = c("task_type", "task_number", "group"))
