
# Detect Change AU Analysis
library(tidyverse)
library(ggpubr)
library(rstatix)
library(cowplot)
library(exact2x2)

source("Util.R")

dc_au <- c("DCAU1a", "DCAU1b",
           "DCAU2a", "DCAU2b",
           "DCAU3a", "DCAU3b",
           "DCAU4a", "DCAU4b",
           "DCAU1aTi_Page Submit","DCAU1bTi_Page Submit",
           "DCAU2aTi_Page Submit","DCAU2bTi_Page Submit",
           "DCAU3aTi_Page Submit","DCAU3bTi_Page Submit",
           "DCAU4aTi_Page Submit","DCAU4bTi_Page Submit")

treatments <- c("None", "StLO", "StLG", "SeLG")

#############################
# Read in data
gp1 <- read_csv("../data/group1.csv") %>%
  slice(3:n()) %>%
  select(dc_au) %>%
  mutate(participant_id = row_number())

gp2 <- read_csv("../data/group2.csv") %>%
  slice(3:n()) %>%
  select(dc_au) %>%
  mutate(participant_id = row_number() + 11)

gp3 <- read_csv("../data/group3.csv") %>%
  slice(3:n()) %>%
  select(dc_au) %>%
  mutate(participant_id = row_number() + 22)

gp4 <- read_csv("../data/group4.csv") %>%
  slice(3:n()) %>%
  select(dc_au) %>%
  mutate(participant_id = row_number() + 33)

# 1 -------------------------------
# Treatment order: 4, 2, 1, 3
dcau1_answer <- 40000000

dcau1 <- bind_rows(gp4[, c("DCAU1a", "DCAU1b", "participant_id")],
                   gp2[, c("DCAU1a", "DCAU1b", "participant_id")],
                   gp1[, c("DCAU1a", "DCAU1b", "participant_id")], 
                   gp3[, c("DCAU1a", "DCAU1b", "participant_id")],
                   .id="Treatment") %>%
  mutate("Clean" = get_double_column(DCAU1b),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

dcau1_incorrect_count <- count_incorrect(dcau1$DCAU1a, "smaller")
dcau1_NA <- count_NA(dcau1$Clean)

dcau1_incorrect_indices <- get_incorrect_indices(dcau1$DCAU1a, "smaller")
dcau1_NA_indices <- get_NA_indices(dcau1$Clean)

dcau1$Flipped[dcau1_incorrect_indices] <- 
  dcau1$Flipped[dcau1_incorrect_indices] * -1

# Response time
dcau1_time <- bind_rows(gp4[,c("DCAU1aTi_Page Submit","DCAU1bTi_Page Submit")],
                        gp2[,c("DCAU1aTi_Page Submit","DCAU1bTi_Page Submit")],
                        gp1[,c("DCAU1aTi_Page Submit","DCAU1bTi_Page Submit")],
                        gp3[,c("DCAU1aTi_Page Submit","DCAU1bTi_Page Submit")],
                        .id="Treatment") %>%
  rename("Time1"="DCAU1aTi_Page Submit",
         "Time2"="DCAU1bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

p1a <- get_boxplot_ans(dcau1, dcau1_NA_indices, dcau1_NA, dcau1_answer,
                       "Detect Change AU 1 (Incorrect incl)",
                       normalize = FALSE)

p1an <- get_boxplot_ans(dcau1, dcau1_NA_indices, dcau1_NA, dcau1_answer,
                       "Detect Change AU 1  (Incorrect incl)",
                       normalize = TRUE)

p1b <- get_boxplot_ans(dcau1, dcau1_NA_indices, dcau1_NA, dcau1_answer,
                        "Detect Change AU 1  (Incorrect Flipped)",
                        normalize = FALSE, "Flipped")

p1bn <- get_boxplot_ans(dcau1, dcau1_NA_indices, dcau1_NA, dcau1_answer,
                        "Detect Change AU 1  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t1a <- get_boxplot_time(dcau1_time, dcau1_NA_indices)

# 2 -------------------------------
# Treatment order: 3, 4, 2, 1
dcau2_answer <- 190000

dcau2 <- bind_rows(gp3[, c("DCAU2a", "DCAU2b", "participant_id")], 
                   gp4[, c("DCAU2a", "DCAU2b", "participant_id")],
                   gp2[, c("DCAU2a", "DCAU2b", "participant_id")], 
                   gp1[, c("DCAU2a", "DCAU2b", "participant_id")],
                   .id="Treatment") %>%
  mutate("Clean" = get_double_column(DCAU2b),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

dcau2_incorrect_count <- count_incorrect(dcau2$DCAU2a, "smaller")
dcau2_NA <- count_NA(dcau2$Clean)

dcau2_incorrect_indices <- get_incorrect_indices(dcau2$DCAU2a, "smaller")
dcau2_NA_indices <- get_NA_indices(dcau2$Clean)

dcau2$Flipped[dcau2_incorrect_indices] <- 
  dcau2$Flipped[dcau2_incorrect_indices] * -1

# Response time
dcau2_time <- bind_rows(gp3[,c("DCAU2aTi_Page Submit","DCAU2bTi_Page Submit")],
                        gp4[,c("DCAU2aTi_Page Submit","DCAU2bTi_Page Submit")],
                        gp2[,c("DCAU2aTi_Page Submit","DCAU2bTi_Page Submit")],
                        gp1[,c("DCAU2aTi_Page Submit","DCAU2bTi_Page Submit")],
                        .id="Treatment") %>%
  rename("Time1"="DCAU2aTi_Page Submit",
         "Time2"="DCAU2bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

p2a <- get_boxplot_ans(dcau2, dcau2_NA_indices, dcau2_NA, dcau2_answer,
                       "Detect Change AU 2 (Incorrect incl)",
                       normalize = FALSE)

p2an <- get_boxplot_ans(dcau2, dcau2_NA_indices, dcau2_NA, dcau2_answer,
                        "Detect Change AU 2 (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

p2b <- get_boxplot_ans(dcau2, dcau2_NA_indices, dcau2_NA, dcau2_answer,
                       "Detect Change AU 2 (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p2bn <- get_boxplot_ans(dcau2, dcau2_NA_indices, dcau2_NA, dcau2_answer,
                       "Detect Change AU 2 (Incorrect Flipped)",
                       normalize = TRUE, "Flipped")

t2a <- get_boxplot_time(dcau2_time, dcau2_NA_indices)


# 3 -------------------------------
# Treatment order: 1, 3, 4, 2
dcau3_answer <- 200000

dcau3 <- bind_rows(gp1[, c("DCAU3a", "DCAU3b", "participant_id")], 
                   gp3[, c("DCAU3a", "DCAU3b", "participant_id")],
                   gp4[, c("DCAU3a", "DCAU3b", "participant_id")], 
                   gp2[, c("DCAU3a", "DCAU3b", "participant_id")],
                   .id="Treatment") %>%
  mutate("Clean" = get_double_column(DCAU3b),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

dcau3_incorrect_count <- count_incorrect(dcau3$DCAU3a, "smaller")
dcau3_NA <- count_NA(dcau3$Clean)

dcau3_incorrect_indices <- get_incorrect_indices(dcau3$DCAU3a, "smaller")
dcau3_NA_indices <- get_NA_indices(dcau3$Clean)

dcau3$Flipped[dcau3_incorrect_indices] <- 
  dcau3$Flipped[dcau3_incorrect_indices] * -1

# Response time
dcau3_time <- bind_rows(gp1[,c("DCAU3aTi_Page Submit","DCAU3bTi_Page Submit")],
                        gp3[,c("DCAU3aTi_Page Submit","DCAU3bTi_Page Submit")],
                        gp4[,c("DCAU3aTi_Page Submit","DCAU3bTi_Page Submit")],
                        gp2[,c("DCAU3aTi_Page Submit","DCAU3bTi_Page Submit")],
                        .id="Treatment") %>%
  rename("Time1"="DCAU3aTi_Page Submit",
         "Time2"="DCAU3bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p3a <- get_boxplot_ans(dcau3, dcau3_NA_indices, dcau3_NA, dcau3_answer,
                       "Detect Change AU 3 (Incorrect incl)",
                       normalize = FALSE)

p3an <- get_boxplot_ans(dcau3, dcau3_NA_indices, dcau3_NA, dcau3_answer,
                        "Detect Change AU 3 (Incorrect incl)",
                        normalize = TRUE)

p3b <- get_boxplot_ans(dcau3, dcau3_NA_indices, dcau3_NA, dcau3_answer,
                       "Detect Change AU 3 (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p3bn <- get_boxplot_ans(dcau3, dcau3_NA_indices, dcau3_NA, dcau3_answer,
                        "Detect Change AU 3 (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")


t3a <- get_boxplot_time(dcau3_time, dcau3_NA_indices)

# 4 -------------------------------
# Treatment order: 2, 1, 3, 4
dcau4_answer <- 11000

dcau4 <- bind_rows(gp2[, c("DCAU4a", "DCAU4b", "participant_id")], 
                   gp1[, c("DCAU4a", "DCAU4b", "participant_id")],
                   gp3[, c("DCAU4a", "DCAU4b", "participant_id")], 
                   gp4[, c("DCAU4a", "DCAU4b", "participant_id")],
                   .id="Treatment") %>%
  mutate("Clean" = get_double_column(DCAU4b),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

dcau4_incorrect_count <- count_incorrect(dcau4$DCAU4a, "smaller")
dcau4_NA <- count_NA(dcau4$Clean)

dcau4_incorrect_indices <- get_incorrect_indices(dcau4$DCAU4a, "smaller")
dcau4_NA_indices <- get_NA_indices(dcau4$Clean)

dcau4$Flipped[dcau4_incorrect_indices] <- 
  dcau4$Flipped[dcau4_incorrect_indices] * -1

# Response time
dcau4_time <- bind_rows(gp2[,c("DCAU4aTi_Page Submit","DCAU4bTi_Page Submit")],
                        gp1[,c("DCAU4aTi_Page Submit","DCAU4bTi_Page Submit")],
                        gp3[,c("DCAU4aTi_Page Submit","DCAU4bTi_Page Submit")],
                        gp4[,c("DCAU4aTi_Page Submit","DCAU4bTi_Page Submit")],
                        .id="Treatment") %>%
  rename("Time1"="DCAU4aTi_Page Submit",
         "Time2"="DCAU4bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))


# Plots
p4a <- get_boxplot_ans(dcau4, dcau4_NA_indices, dcau4_NA, dcau4_answer,
                       "Detect Change AU 4 (Incorrect incl)",
                       normalize = FALSE)

p4an <- get_boxplot_ans(dcau4, dcau4_NA_indices, dcau4_NA, dcau4_answer,
                        "Detect Change AU 4 (Incorrect incl)",
                        normalize = TRUE)

p4b <- get_boxplot_ans(dcau4, dcau4_NA_indices, dcau4_NA, dcau4_answer,
                       "Detect Change AU 4 (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p4bn <- get_boxplot_ans(dcau4, dcau4_NA_indices, dcau4_NA, dcau4_answer,
                        "Detect Change AU 4 (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")


t4a <- get_boxplot_time(dcau4_time, dcau4_NA_indices)

###################################
#####   Response Aggregated   #####
###################################

dcau_all <- get_aggre_df(dcau1, dcau2, dcau3, dcau4,
                         dcau1_answer, dcau2_answer,
                         dcau3_answer, dcau4_answer,
                         dcau1_NA_indices, dcau2_NA_indices,
                         dcau3_NA_indices, dcau4_NA_indices,
                         "Flipped")

kw_res <- get_kruskal_res(dcau_all) # p.value = 0.3369

pall_title <- chi2_and_main_p(kw_res)

pall <- get_aggre_plot(dcau_all, pall_title)

pall <- pall + scale_y_continuous(limits = c(-3, 3))

###################################
#####   Time Aggregated      ######
###################################

dcau_all_time <- bind_rows(dcau1_time[-dcau1_NA_indices, ],
                           dcau2_time[-dcau2_NA_indices, ],
                           dcau3_time[-dcau3_NA_indices, ], 
                           dcau4_time[-dcau4_NA_indices, ])

kw_time <- get_kruskal_time(dcau_all_time) # p.value = 0.02193

tall_title <- chi2_and_main_p(kw_time)

tall <- get_aggre_time(dcau1_time, dcau2_time,
                       dcau3_time, dcau4_time,
                       dcau1_NA_indices, dcau2_NA_indices,
                       dcau3_NA_indices, dcau4_NA_indices,
                       tall_title)

pairwise.wilcox.test(dcau_all_time$Time,
                     factor(dcau_all_time$Treatment),
                     p.adjust.method = "holm",
                     paired = FALSE)  # No significant pairwise differences


###################################
######      NA Aggregated     #####
###################################

dcau_NA_all <- bind_rows(dcau1[, c("treatment", "participant_id", "answer")], 
                         dcau2[, c("treatment", "participant_id", "answer")], 
                         dcau3[, c("treatment", "participant_id", "answer")], 
                         dcau4[, c("treatment", "participant_id", "answer")])

dcau_NA_all$answer <- factor(as.character(dcau_NA_all$answer))
dcau_NA_all$treatment <- factor(dcau_NA_all$treatment, levels = treatments)

cqtest <- cochran_qtest(dcau_NA_all, answer ~ treatment | participant_id)
cqtest$p.value <- cqtest$p

pbar_title <- chi2_and_main_p(cqtest)
pbar <- get_NA_barplot(dcau_NA, pbar_title)

dcau_pairwise_mcnemar <- pairwise_mcnemar_test(dcau_NA_all, 
                                               answer ~ treatment | participant_id,
                                               p.adjust.method = "holm") %>%
  filter(!is.nan(p))

pbar <- pbar + stat_pvalue_manual(dcau_pairwise_mcnemar,
                                  label = "p.adj.signif",
                                  hide.ns = TRUE,
                                  y.position = c(65, 72, 81, 88))

###################################
# Confidence Interval for NA pairwise results
###################################

na_pairwise_effect_ci(dcau_NA_all, c("None", "StLO"))
na_pairwise_effect_ci(dcau_NA_all, c("None", "StLG"))
na_pairwise_effect_ci(dcau_NA_all, c("None", "SeLG"))
na_pairwise_effect_ci(dcau_NA_all, c("StLO", "SeLG"))

###################################
######      Combine plots     #####
###################################

title <-
  ggdraw() +
  draw_label("Detect Change in Administrative Unit",
             fontface = "bold.italic",
             size = 15) +
  theme(plot.background = element_rect(fill = "#e6e6e6", color = NA)) 

bottom_row <- plot_grid(pall, tall, pbar, ncol = 3, rel_heights = c(3/10, 4/10, 4/10))
pcombined <- plot_grid(title,
                       bottom_row,
                       nrow = 2,
                       rel_heights = c(0.12, 1))

#saveRDS(pcombined, file = "../rdata/Combined_DCAU.rds")
#ggsave("Combined_DCAU.pdf", pcombined, path = "../plots/", width = 6, height = 4)

###################################
# Summary Data
###################################
summary(dcau_all$Normal, na.rm=TRUE)

dcau_all %>%
  mutate(id = rep(1:44, 4)) %>%
  pivot_wider(names_from = Treatment, values_from = Normal) %>%
  summary

sum(dcau_NA / 176) * 100

dcau_NA_all %>%
  group_by(treatment) %>%
  summarise(propor = sum(answer == TRUE)/44 * 100)

summary(dcau_all_time)

for (i in 1:4) {
  print(c("Treatment", i))
  print(summary(dcau_all_time[dcau_all_time$Treatment == i, "Time"]))
}