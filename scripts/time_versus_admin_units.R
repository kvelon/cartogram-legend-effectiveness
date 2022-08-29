library(AICcmodavg)
library(lme4)
library(nlme)
library(tidyverse)
source("scripts/Util.R")

# Function to import data for one treatment group
data_for_group <- function(group_number) {

  # Import questionnaire with response times, map familiarity and answers
  gp <-
    read_csv(str_c("data/group", group_number, ".csv")) |>
    slice(3:n()) |>
    mutate(participant_id = row_number())

  # Make separate tibbles for:
  # - map familiarity and response times.
  # - string answers.
  # - numeric answers.
  # We start with response times and map familiarity.
  familiarity_and_times <-
    gp |>
    rename_with(~ str_replace(., "^Fam", "fam")) |>
    select(participant_id, starts_with("fam"), ends_with("Ti_Page Submit")) |>
    pivot_longer(
      -c(participant_id, starts_with("fam")),
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

  # Join the information about response times, string answers, numeric
  # answers and map familiarity
  gp <-
    familiarity_and_times |>
    left_join(string_answers, by = c("participant_id", "task_part")) |>
    left_join(numeric_answers, by = c("participant_id", "task_part"))

  # Remove wrong and missing answers
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
      keep_answer = all(keep_answer),
      across(starts_with("Fam"), )
    ) |>
    ungroup(participant_id, task) |>
    filter(keep_answer) |>
    mutate(group = group_number) |>
    select(group, participant_id, starts_with("fam"), task, time)
}

experiment_sequence <-
  read_csv("data/experiment_sequence.csv") |>
  pivot_longer(
    starts_with("group"),
    names_to = "group",
    names_pattern = "^group_(\\d)$",
    names_transform = list(group = as.numeric),
    values_to = "treatment"
  )
experimental_data <-
  map_dfr(1:4, ~ bind_rows(data_for_group(.))) |>
  mutate(
    unique_participant_id = as.factor((group - 1) * 11 + participant_id),
    task_type = str_replace(task, "^(.*)\\d$", "\\1"),
    task_number = as.numeric(str_replace(task, "^.*(\\d)$", "\\1"))
  ) |>
  select(-task) |>
  left_join(experiment_sequence, by = c("task_type", "task_number", "group"))
data_for_regression <-
  experimental_data |>
  mutate(log10_time = log10(time)) |>
  select(
    log10_time,
    unique_participant_id,
    starts_with("Fam"),
    task_type,
    n_admin_div,
    treatment
  )

# Function to run a linear model without random effects
model_with_fixed_effects_only <- function(fixed_effect) {
  fe <-
    map_chr(fixed_effect, ~ str_c(" + ", .)) |>
    str_c(collapse = "")
  lm(
    as.formula(str_c("log10_time ~ task_type + n_admin_div + treatment", fe)),
    data = data_for_regression
  )
}
fixed_effects <- list(
  NULL,
  "fam1",
  "fam2",
  "fam3",
  c("fam1", "fam2"),
  c("fam1", "fam3"),
  c("fam2", "fam3"),
  c("fam1", "fam2", "fam3"),
  "unique_participant_id"
)
lm_list <-
  map(fixed_effects, model_with_fixed_effects_only) |>
  set_names(str_c(
    "lm_",
    map_chr(fixed_effects, ~ str_c(., collapse = "_"))
  ))
bictab_lm <- bictab(lm_list)

# Function to run a linear mixed-effect model with participant as random
# effect
model_with_random_effect <- function(fixed_effect) {
  fe <-
    map_chr(fixed_effect, ~ str_c(" + ", .)) |>
    str_c(collapse = "")
  lmer(
    as.formula(str_c(
      "log10_time ~ task_type + n_admin_div + treatment",
      fe,
      " + (1 | unique_participant_id)"
    )),
    data = data_for_regression,
    REML = FALSE
  )
}
fixed_effects_with_random <- fixed_effects[-length(fixed_effects)]
lmer_list <-
  map(fixed_effects_with_random, model_with_random_effect) |>
  set_names(str_c(
    "lmer_",
    map_chr(fixed_effects_with_random, ~ str_c(., collapse = "_"))
  ))
bictab_lmer <- bictab(lmer_list)

# Combined BIC table
bictab_combined <-
  bictab_lm |>
  bind_rows(bictab_lmer) |>
  as_tibble() |>
  select(modnames = Modnames, k = K, bic = BIC) |>
  arrange(bic) |>
  mutate(delta_bic = bic - first(bic))
bictab_combined

# Get p-values for unique_participant_id
model_lme <- lme(
  log10_time ~ task_type + n_admin_div + treatment,
  data = data_for_regression,
  random = ~ 1 | unique_participant_id,
  method = "REML"
)
t <- summary(model_lme)$tTable[, "t-value"]["n_admin_div"]
degrees_of_freedom <- summary(model_lme)$tTable[, "DF"]["n_admin_div"]
pt(t, degrees_of_freedom, lower.tail = FALSE) # p-value for one-sided test
