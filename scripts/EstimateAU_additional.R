library(broom)
library(rcompanion)
library(tidyverse)

source("scripts/Util.R")

est_au_columns <- str_c("EstAU", 1:4)
assignments <-
  tibble(
    group = rep(1:4, 4),
    geography = rep(est_au_columns, each = 4),
    treatment = ordered(
      c(
        "None", "StLG", "StLO", "SeLG",
        "StLG", "None", "SeLG", "StLO",
        "SeLG", "StLO", "None", "StLG",
        "StLO", "SeLG", "StLG", "None"
      ),
      levels = c("None", "StLO", "StLG", "SeLG")
    )
  )
import_group <- function(group, ignore_rows_before) {
  str_c("data/group", group, ".csv") |>
    read_csv(col_types = cols()) |>
    when(
      ignore_rows_before > 0 ~ slice(., -seq_len(ignore_rows_before)),
      ~.
    ) |>
    select(all_of(est_au_columns)) |>
    mutate(group = group)
}
est_au <- map2_dfr(1:4, rep(2, 4), import_group) |>
  pivot_longer(starts_with("EstAu"),
    names_to = "geography",
    values_to = "response"
  ) |>
  mutate(
    response = get_double_column(response),
    correct = geography |>
      recode(
        "EstAU1" = 1.8e6,
        "EstAU2" = 2.6e6,
        "EstAU3" = 2.0e5,
        "EstAU4" = 3.4e6
      ),
    normalised = if_else(
      (response - correct) / correct < 100,
      (response - correct) / correct,
      NA_real_
    )
  ) |>
  left_join(assignments, by = c("group", "geography"))


# Confirm that the assignments were correct
ggplot(est_au, aes(treatment, normalised)) +
  geom_boxplot(na.rm = TRUE)
cat("\tDescriptive statistics\n\n")
est_au |>
  group_by(treatment) |>
  summarise(
    median = median(normalised, na.rm = TRUE),
    var = var(normalised, na.rm = TRUE)
  ) |>
  print()

# Get the residuals of the responses (i.e. subtract the mean conditioned on
# the treatment from the response)
rsd <- aov(normalised ~ treatment, data = est_au)$residuals

# Test for normality
shapiro.test(rsd) |>
  print()

# Test for deviation of pseudomedians from zero
wilcox <-
  est_au |>
  group_by(treatment) |>
  summarise(
    wilcox.test(normalised, exact = FALSE, conf.int = TRUE) |>
      glance(),
    effect_size = wilcoxonOneSampleR(normalised, mu = 0)
  ) |>
  mutate(p.adj = p.adjust(p.value)) |>
  select(-method, -alternative)

cat("\tWilcoxon pairwise signed rank test for difference in pseudomedian\n\n")
print(wilcox)

# Test for difference in variance
fligner.test(normalised ~ treatment, data = est_au) |>
  print()

# Pairwise comparison of scale with Ansari-Bradley test
scale_test <- function(treatment_1, treatment_2) {
  sub_1 <- est_au$normalised[est_au$treatment == treatment_1]
  sub_2 <- est_au$normalised[est_au$treatment == treatment_2]

  # From https://www.mathworks.com/help/stats/ansaribradley.html:
  # "This test requires that the samples have equal medians. Under that
  # assumption, and if the distributions of the samples are continuous and
  # identical, the test is independent of the distributions. If the samples do
  # not have the same medians, the results can be misleading. In that case,
  # Ansari and Bradley recommend subtracting the median.
  ansari.test(
    sub_1 - median(sub_1, na.rm = TRUE),
    sub_2 - median(sub_2, na.rm = TRUE),
    conf.int = TRUE,
    exact = FALSE
  ) |>
    glance()
}
ansari <-
  unique(assignments$treatment) |>
  sort() |>
  (\(x) expand_grid(treatment_1 = x, treatment_2 = x))() |>
  filter(treatment_1 < treatment_2) |>
  mutate(map2_dfr(treatment_1, treatment_2, scale_test),
    p.adj = p.adjust(p.value)
  ) |>
  select(-method, -alternative)
cat("\tAnsari-Bradley pairwise test for difference in scale parameters\n\n")
print(ansari)
