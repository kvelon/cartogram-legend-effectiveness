# Compare Zone Analysis

library(tidyverse)
library(ggpubr)
library(rstatix)
library(cowplot)
library(exact2x2)

source("Util.R")

com_zo <- c("ComZo1a", "ComZo1b_1",
            "ComZo2a", "ComZo2b_1",
            "ComZo3a", "ComZo3b_1",
            "ComZo4a", "ComZo4b_1",
            "ComZo1aTi_Page Submit","ComZo1bTi_Page Submit",
            "ComZo2aTi_Page Submit","ComZo2bTi_Page Submit",
            "ComZo3aTi_Page Submit","ComZo3bTu_Page Submit",
            "ComZo4aTi_Page Submit","ComZo4bTi_Page Submit")

treatments <- c("None", "StLO", "StLG", "SeLG")

#############################
# Read in data
gp1 <- read_csv("../data/group1.csv") %>%
  slice(3:n()) %>%
  select(com_zo) %>%
  mutate(participant_id = row_number())

gp2 <- read_csv("../data/group2.csv") %>%
  slice(3:n()) %>%
  select(com_zo) %>%
  mutate(participant_id = row_number() + 11)

gp3 <- read_csv("../data/group3.csv") %>%
  slice(3:n()) %>%
  select(com_zo) %>%
  mutate(participant_id = row_number() + 22)

gp4 <- read_csv("../data/group4.csv") %>%
  slice(3:n()) %>%
  select(com_zo) %>%
  mutate(participant_id = row_number() + 33)

# 1 -------------------------------
# Treatment order: 3, 1, 4, 2 
comzo1_answer <- 25500

comzo1 <- bind_rows(gp3[, c("ComZo1a", "ComZo1b_1", "participant_id")], 
                    gp1[, c("ComZo1a", "ComZo1b_1", "participant_id")],
                    gp4[, c("ComZo1a", "ComZo1b_1", "participant_id")], 
                    gp2[, c("ComZo1a", "ComZo1b_1", "participant_id")],
                    .id="Treatment") %>%
  mutate("Clean" = get_double_column(ComZo1b_1),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

comzo1_incorrect_count <- count_incorrect(comzo1$ComZo1a, "higher")
comzo1_NA_count <- count_NA(comzo1$Clean)

comzo1_incorrect_indices <- get_incorrect_indices(comzo1$ComZo1a, "higher")
comzo1_NA_indices <- get_NA_indices(comzo1$Clean)

comzo1$Flipped[comzo1_incorrect_indices] <- 
  comzo1$Flipped[comzo1_incorrect_indices] * -1

# Response time
comzo1_time <- bind_rows(gp3[,c("ComZo1aTi_Page Submit","ComZo1bTi_Page Submit")],
                         gp1[,c("ComZo1aTi_Page Submit","ComZo1bTi_Page Submit")],
                         gp4[,c("ComZo1aTi_Page Submit","ComZo1bTi_Page Submit")],
                         gp2[,c("ComZo1aTi_Page Submit","ComZo1bTi_Page Submit")],
                         .id="Treatment") %>%
  rename("Time1"="ComZo1aTi_Page Submit",
         "Time2"="ComZo1bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p1a <- get_boxplot_ans(comzo1, comzo1_NA_indices, comzo1_NA_count, comzo1_answer,
                       "Compare Zone 1 (Incorrect incl)",
                       normalize = FALSE)

p1an <- get_boxplot_ans(comzo1, comzo1_NA_indices, comzo1_NA_count, comzo1_answer,
                        "Compare Zone 1 (Incorrect incl)",
                        normalize = TRUE)

p1b <- get_boxplot_ans(comzo1, comzo1_NA_indices, comzo1_NA_count, comzo1_answer,
                       "Compare Zone 1  (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p1bn <- get_boxplot_ans(comzo1, comzo1_NA_indices, comzo1_NA_count, comzo1_answer,
                        "Compare Zone 1  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t1a <- get_boxplot_time(comzo1_time, comzo1_NA_indices)

# 2 -------------------------------
# Treatment order: 4, 2, 3, 1 
comzo2_answer <- 25000

comzo2 <- bind_rows(gp4[, c("ComZo2a", "ComZo2b_1", "participant_id")], 
                    gp2[, c("ComZo2a", "ComZo2b_1", "participant_id")],
                    gp3[, c("ComZo2a", "ComZo2b_1", "participant_id")], 
                    gp1[, c("ComZo2a", "ComZo2b_1", "participant_id")],
                    .id="Treatment") %>%
  mutate("Clean" = get_double_column(ComZo2b_1),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

comzo2_incorrect_count <- count_incorrect(comzo2$ComZo2a, "lower")
comzo2_NA_count <- count_NA(comzo2$Clean)

comzo2_incorrect_indices <- get_incorrect_indices(comzo2$ComZo2a, "lower")
comzo2_NA_indices <- get_NA_indices(comzo2$Clean)

comzo2$Flipped[comzo2_incorrect_indices] <- 
  comzo2$Flipped[comzo2_incorrect_indices] * -1

# Response time
comzo2_time <- bind_rows(gp4[,c("ComZo2aTi_Page Submit","ComZo2bTi_Page Submit")],
                         gp2[,c("ComZo2aTi_Page Submit","ComZo2bTi_Page Submit")],
                         gp3[,c("ComZo2aTi_Page Submit","ComZo2bTi_Page Submit")],
                         gp1[,c("ComZo2aTi_Page Submit","ComZo2bTi_Page Submit")],
                         .id="Treatment") %>%
  rename("Time1"="ComZo2aTi_Page Submit",
         "Time2"="ComZo2bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p2a <- get_boxplot_ans(comzo2, comzo2_NA_indices, comzo2_NA_count, comzo2_answer,
                       "Compare Zone 2 (Incorrect incl)",
                       normalize = FALSE)

p2an <- get_boxplot_ans(comzo2, comzo2_NA_indices, comzo2_NA_count, comzo2_answer,
                        "Compare Zone 2 (Incorrect incl)",
                        normalize = TRUE)

p2b <- get_boxplot_ans(comzo2, comzo2_NA_indices, comzo2_NA_count, comzo2_answer,
                       "Compare Zone 2  (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p2bn <- get_boxplot_ans(comzo2, comzo2_NA_indices, comzo2_NA_count, comzo2_answer,
                        "Compare Zone 2  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t2a <- get_boxplot_time(comzo2_time, comzo2_NA_indices)

# 3 -------------------------------
# Treatment order: 2, 3, 1, 4 
comzo3_answer <- 4000000

comzo3 <- bind_rows(gp2[, c("ComZo3a", "ComZo3b_1", "participant_id")], 
                    gp3[, c("ComZo3a", "ComZo3b_1", "participant_id")],
                    gp1[, c("ComZo3a", "ComZo3b_1", "participant_id")], 
                    gp4[, c("ComZo3a", "ComZo3b_1", "participant_id")],
                    .id="Treatment") %>%
  mutate("Clean" = get_double_column(ComZo3b_1),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

comzo3_incorrect_count <- count_incorrect(comzo3$ComZo3a, "smaller")
comzo3_NA_count <- count_NA(comzo3$Clean)

comzo3_incorrect_indices <- get_incorrect_indices(comzo3$ComZo3a, "smaller")
comzo3_NA_indices <- get_NA_indices(comzo3$Clean)
comzo3_outliers <- c(comzo3_NA_indices, 29)

comzo3$Flipped[comzo3_incorrect_indices] <- 
  comzo3$Flipped[comzo3_incorrect_indices] * -1

# Response time
comzo3_time <- bind_rows(gp2[,c("ComZo3aTi_Page Submit","ComZo3bTu_Page Submit")],
                         gp3[,c("ComZo3aTi_Page Submit","ComZo3bTu_Page Submit")],
                         gp1[,c("ComZo3aTi_Page Submit","ComZo3bTu_Page Submit")],
                         gp4[,c("ComZo3aTi_Page Submit","ComZo3bTu_Page Submit")],
                         .id="Treatment") %>%
  rename("Time1"="ComZo3aTi_Page Submit",
         "Time2"="ComZo3bTu_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p3a <- get_boxplot_ans(comzo3, comzo3_outliers, comzo3_NA_count, comzo3_answer,
                       "Compare Zone 3 (Incorrect incl)",
                       normalize = FALSE)

p3an <- get_boxplot_ans(comzo3, comzo3_outliers, comzo3_NA_count, comzo3_answer,
                        "Compare Zone 3 (Incorrect incl)",
                        normalize = TRUE)

p3b <- get_boxplot_ans(comzo3, comzo3_outliers, comzo3_NA_count, comzo3_answer,
                       "Compare Zone 3  (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p3bn <- get_boxplot_ans(comzo3, comzo3_outliers, comzo3_NA_count, comzo3_answer,
                        "Compare Zone 3  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t3a <- get_boxplot_time(comzo3_time, comzo3_outliers)

# 4 -------------------------------
# Treatment order: 1, 4, 2, 3 
comzo4_answer <- 250000

comzo4 <- bind_rows(gp1[, c("ComZo4a", "ComZo4b_1", "participant_id")], 
                    gp4[, c("ComZo4a", "ComZo4b_1", "participant_id")],
                    gp2[, c("ComZo4a", "ComZo4b_1", "participant_id")], 
                    gp3[, c("ComZo4a", "ComZo4b_1", "participant_id")],
                    .id="Treatment") %>%
  mutate("Clean" = get_double_column(ComZo4b_1),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

comzo4_incorrect_count <- count_incorrect(comzo4$ComZo4a, "larger")
comzo4_NA_count <- count_NA(comzo4$Clean)

comzo4_incorrect_indices <- get_incorrect_indices(comzo4$ComZo4a, "larger")
comzo4_NA_indices <- get_NA_indices(comzo4$Clean)

comzo4$Flipped[comzo4_incorrect_indices] <- 
  comzo4$Flipped[comzo4_incorrect_indices] * -1

# Response time
comzo4_time <- bind_rows(gp1[,c("ComZo4aTi_Page Submit","ComZo4bTi_Page Submit")],
                         gp4[,c("ComZo4aTi_Page Submit","ComZo4bTi_Page Submit")],
                         gp2[,c("ComZo4aTi_Page Submit","ComZo4bTi_Page Submit")],
                         gp3[,c("ComZo4aTi_Page Submit","ComZo4bTi_Page Submit")],
                         .id="Treatment") %>%
  rename("Time1"="ComZo4aTi_Page Submit",
         "Time2"="ComZo4bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p4a <- get_boxplot_ans(comzo4, comzo4_NA_indices, comzo4_NA_count, comzo4_answer,
                       "Compare Zone 4 (Incorrect incl)",
                       normalize = FALSE)

p4an <- get_boxplot_ans(comzo4, comzo4_NA_indices, comzo4_NA_count, comzo4_answer,
                        "Compare Zone 4 (Incorrect incl)",
                        normalize = TRUE)

p4b <- get_boxplot_ans(comzo4, comzo4_NA_indices, comzo4_NA_count, comzo4_answer,
                       "Compare Zone 4  (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p4bn <- get_boxplot_ans(comzo4, comzo4_NA_indices, comzo4_NA_count, comzo4_answer,
                        "Compare Zone 4  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t4a <- get_boxplot_time(comzo4_time, comzo4_NA_indices)


###################################
# Aggregated
###################################
comzo_all <- get_aggre_df(comzo1, comzo2, comzo3, comzo4,
                          comzo1_answer, comzo2_answer,
                          comzo3_answer, comzo4_answer,
                          comzo1_NA_indices, comzo2_NA_indices,
                          comzo3_outliers, comzo4_NA_indices,
                          "Flipped")

kw_res <- get_kruskal_res(comzo_all) # p.value = 0.5363

pall_title <- chi2_and_main_p(kw_res)

pall <- get_aggre_plot(comzo_all, pall_title)

pall <- pall + scale_y_continuous(limits = c(-3, 3))

# Response Time Aggregated ----------------------------------------

comzo_all_time <- bind_rows(comzo1_time[-comzo1_NA_indices, ], 
                            comzo2_time[-comzo2_NA_indices, ],
                            comzo3_time[-comzo3_outliers, ], 
                            comzo4_time[-comzo4_NA_indices, ])

comzo_all_time$Treatment <- plyr::mapvalues(comzo_all_time$Treatment,
                                            from = c("1", "2", "3", "4"),
                                            to = c("None", "StLO", "StLG", "SeLG")) %>%
  factor(levels = c("None", "StLO", "StLG", "SeLG"))

# Statistical Analysis
kw_time <- get_kruskal_time(comzo_all_time) # p.value = 8.808e-07
pairwise.wilcox.test(comzo_all_time$Time,
                     factor(comzo_all_time$Treatment),
                     p.adjust.method = "holm",
                     paired = FALSE)

tall_title <- chi2_and_main_p(kw_time)

tall <- get_aggre_time(comzo1_time, comzo2_time,
                       comzo3_time, comzo4_time,
                       comzo1_NA_indices, comzo2_NA_indices,
                       comzo3_outliers, comzo4_NA_indices,
                       tall_title)

# Add significant p-values
comzo_time_pairwise <- pairwise_wilcox_test(comzo_all_time,
                                            Time ~ Treatment,
                                            p.adjust.method = "holm",
                                            paired = FALSE,
                                            detailed = TRUE)

tall <- tall + stat_pvalue_manual(comzo_time_pairwise,
                                  label = "p.adj.signif",
                                  hide.ns = TRUE,
                                  y.position = c(230, 250, 270, 290))

# NA Analysis
comzo_NA_count <- comzo1_NA_count + comzo2_NA_count + comzo3_NA_count + comzo4_NA_count
comzo_Xsq <- get_chisq(comzo_NA_count)    # pval = 6.09e-05
get_pairwise_results(comzo_NA_count)

comzo_NA_all <- bind_rows(comzo1[, c("treatment", "participant_id", "answer")], 
                          comzo2[, c("treatment", "participant_id", "answer")], 
                          comzo3[, c("treatment", "participant_id", "answer")], 
                          comzo4[, c("treatment", "participant_id", "answer")])

comzo_NA_all$answer <- factor(as.character(comzo_NA_all$answer))
comzo_NA_all$treatment <- factor(comzo_NA_all$treatment, levels = treatments)

cqtest <- cochran_qtest(comzo_NA_all, answer ~ treatment | participant_id)
cqtest$p.value <- cqtest$p

pbar_title <- chi2_and_main_p(cqtest)

pbar <- get_NA_barplot(comzo_NA_count, pbar_title)

# Plot significant p-values for NA barplot
# comzo_NonNA <- 44 - comzo_NA_count
# comzo_NA_df <- data.frame("NA" = comzo_NA_count,
#                           "Non-NA" = comzo_NonNA,
#                           row.names = c("None", "LO", "LG", "SLG"))
# 
# comzo_NA_pairwise <- pairwise_prop_test(comzo_NA_df,
#                                         p.adjust.method = "holm")

comzo_pairwise_mcnemar <- pairwise_mcnemar_test(comzo_NA_all, 
                                                answer ~ treatment | participant_id,
                                                p.adjust.method = "holm") %>%
  filter(!is.nan(p))

pbar <- pbar + stat_pvalue_manual(comzo_pairwise_mcnemar,
                                  label = "p.adj.signif",
                                  hide.ns = TRUE,
                                  y.position = c(50, 60, 70, 80))

###################################
# Confidence Interval for NA pairwise results
###################################

pairwise_effect_ci <- function(NA_all, pair) {
  
  m <- NA_all %>%
    filter(treatment %in% pair) %>%
    group_by(factor(treatment, levels = rev(treatments))) %>%
    summarise(nores = sum(answer == TRUE),
              gotres = sum(!answer == TRUE)) %>%
    select(-1) %>%
    as.matrix() %>%
    t()
  
  fisher.exact(m, or = 1, alternative = "two.sided",
               tsmethod = "central", conf.int = TRUE, conf.level = 0.95)
}

pairwise_effect_ci(comzo_NA_all, c("None", "StLG"))
pairwise_effect_ci(comzo_NA_all, c("None", "SeLG"))
pairwise_effect_ci(comzo_NA_all, c("StLO", "StLG"))
pairwise_effect_ci(comzo_NA_all, c("StLO", "SeLG"))

## END OF CIs
#################################################

# Combined plot
title <-
  ggdraw() +
  draw_label("Compare Zones",
             fontface = "bold.italic",
             size = 15) +
  theme(plot.background = element_rect(fill = "#e6e6e6", color = NA)) 

bottom_row <- plot_grid(pall, tall, pbar, ncol = 3, rel_heights = c(3/10, 4/10, 4/10))
pcombined <- plot_grid(title,
                       bottom_row,
                       nrow = 2,
                       rel_heights = c(0.12, 1))

#saveRDS(pcombined, file = "../rdata/Combined_ComZo.rds")
#ggsave("Combined_ComZo.pdf", pcombined, path = "../plots/", width = 6, height = 4)

###################################
# Summary Data
###################################
summary(comzo_all$Normal, na.rm=TRUE)

comzo_all %>%
  mutate(id = rep(1:44, 4)) %>%
  pivot_wider(names_from = Treatment, values_from = Normal) %>%
  summary

sum(comzo_NA_count / 176) * 100

comzo_NA_all %>%
  group_by(treatment) %>%
  summarise(propor = sum(answer == TRUE)/44 * 100)

summary(comzo_all_time)

for (i in treatments) {
  print(c("Treatment", i))
  print(summary(comzo_all_time[comzo_all_time$Treatment == i, "Time"]))
}

########################################
#               Unused                 #
########################################

# Grouped 
pgrouped <- get_grouped_ans(comzo1, comzo2, comzo3, comzo4,
                            comzo1_answer, comzo2_answer,
                            comzo3_answer, comzo4_answer,
                            comzo1_NA_indices, comzo2_NA_indices,
                            comzo3_outliers, comzo4_NA_indices,
                            comzo1_NA_count, comzo2_NA_count,
                            comzo3_NA_count, comzo4_NA_count,
                            "Compare Zone", "Flipped")[[2]]

tgrouped <- get_grouped_time(comzo1_time, comzo2_time,
                             comzo3_time, comzo4_time,
                             comzo1_NA_indices, comzo2_NA_indices,
                             comzo3_outliers, comzo4_NA_indices,
                             "Response Time Grouped (Seconds)")