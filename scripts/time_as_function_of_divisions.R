library(tidyverse)
source("scripts/Util.R")

# Import questionnaire with response times and answers
gp1 <-
  read_csv("data/group1.csv") |>
  slice(3:n()) |>
  mutate(participant_id = row_number())

# Make separate tibbles for response times, numeric answers and string answers
times_gp1 <-
  gp1 |>
  select(participant_id, ends_with("Ti_Page Submit")) |>
  pivot_longer(-participant_id, names_to = "task_part", values_to = "time") |>
  mutate(
    task_part = str_replace(task_part, "Ti_Page\\sSubmit$", ""),
    task_part = str_replace(task_part, "^EstAu", "EstAU"),
    time = as.numeric(time)
  )
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
numeric_answers_gp1 <-
  gp1 |>
  select(participant_id, numeric_task_parts) |>
  pivot_longer(
    -participant_id,
    names_to = "task_part",
    values_to = "numeric_answer"
  ) |>
  mutate(
    task_part = str_replace(task_part, "_1", ""),
    numeric_answer = get_double_column(numeric_answer)
  )
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
string_answers_gp1 <-
  gp1 |>
  select(participant_id, string_task_parts) |>
  pivot_longer(
    -participant_id,
    names_to = "task_part",
    values_to = "string_answer"
  )

# Join the information about response times, numeric answers and string
# answers
times_gp1 <-
  times_gp1 |>
  left_join(numeric_answers_gp1, by = c("participant_id", "task_part")) |>
  left_join(string_answers_gp1, by = c("participant_id", "task_part"))
  
# # Remove wrong or missing answers
# answer_key <- read_csv("data/answer_key.csv")
