# Detect Change Zone Analysis

library(tidyverse)
library(ggpubr)
library(rstatix)
library(cowplot)
library(exact2x2)

source("Util.R")

dc_zo <- c("DCZo1a", "DCZo1b",
           "DCZo2a", "DCZo2b",
           "DCZo3a", "DCZo3b",
           "DCZo4a", "DCZo4b",
           "DCZo1aTi_Page Submit","DCZo1bTi_Page Submit",
           "DCZo2aTi_Page Submit","DCZo2bTi_Page Submit",
           "DCZo3aTi_Page Submit","DCZo3bTi_Page Submit",
           "DCZo4aTi_Page Submit","DCZo4bTi_Page Submit")

treatments <- c("None", "StLO", "StLG", "SeLG")

#############################
# Read in data
gp1 <- read_csv("../data/group1.csv") %>%
  slice(3:n()) %>%
  select(dc_zo) %>%
  mutate(participant_id = row_number())

gp2 <- read_csv("../data/group2.csv") %>%
  slice(3:n()) %>%
  select(dc_zo) %>%
  mutate(participant_id = row_number() + 11)

gp3 <- read_csv("../data/group3.csv") %>%
  slice(3:n()) %>%
  select(dc_zo) %>%
  mutate(participant_id = row_number() + 22)

gp4 <- read_csv("../data/group4.csv") %>%
  slice(3:n()) %>%
  select(dc_zo) %>%
  mutate(participant_id = row_number() + 33)

# 1 -------------------------------
# Treatment order: 2, 1, 4, 3
dczo1_answer <- 300000

dczo1 <- bind_rows(gp2[, c("DCZo1a", "DCZo1b", "participant_id")], 
                   gp1[, c("DCZo1a", "DCZo1b", "participant_id")],
                   gp4[, c("DCZo1a", "DCZo1b", "participant_id")], 
                   gp3[, c("DCZo1a", "DCZo1b", "participant_id")],
                   .id="Treatment") %>%
  mutate("Clean" = get_double_column(DCZo1b),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

dczo1_incorrect_count <- count_incorrect(dczo1$DCZo1a, "smaller")
dczo1_NA_count <- count_NA(dczo1$Clean)

dczo1_incorrect_indices <- get_incorrect_indices(dczo1$DCZo1a, "smaller")
dczo1_NA_indices <- get_NA_indices(dczo1$Clean)

dczo1$Flipped[dczo1_incorrect_indices] <- 
  dczo1$Flipped[dczo1_incorrect_indices] * -1

# Response time
dczo1_time <- bind_rows(gp2[,c("DCZo1aTi_Page Submit","DCZo1bTi_Page Submit")],
                        gp1[,c("DCZo1aTi_Page Submit","DCZo1bTi_Page Submit")],
                        gp4[,c("DCZo1aTi_Page Submit","DCZo1bTi_Page Submit")],
                        gp3[,c("DCZo1aTi_Page Submit","DCZo1bTi_Page Submit")],
                        .id="Treatment") %>%
  rename("Time1"="DCZo1aTi_Page Submit",
         "Time2"="DCZo1bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p1a <- get_boxplot_ans(dczo1, dczo1_NA_indices, dczo1_NA_count, dczo1_answer,
                       "Detect Change Zone 1 (Incorrect incl)",
                       normalize = FALSE)

p1an <- get_boxplot_ans(dczo1, dczo1_NA_indices, dczo1_NA_count, dczo1_answer,
                        "Detect Change Zone 1  (Incorrect incl)",
                        normalize = TRUE)

p1b <- get_boxplot_ans(dczo1, dczo1_NA_indices, dczo1_NA_count, dczo1_answer,
                       "Detect Change Zone 1  (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p1bn <- get_boxplot_ans(dczo1, dczo1_NA_indices, dczo1_NA_count, dczo1_answer,
                        "Detect Change Zone 1  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t1a <- get_boxplot_time(dczo1_time, dczo1_NA_indices)

# 2 -------------------------------
# Treatment order: 3, 4, 1, 2
dczo2_answer <- 2000000

dczo2 <- bind_rows(gp3[, c("DCZo2a", "DCZo2b", "participant_id")], 
                   gp4[, c("DCZo2a", "DCZo2b", "participant_id")],
                   gp1[, c("DCZo2a", "DCZo2b", "participant_id")], 
                   gp2[, c("DCZo2a", "DCZo2b", "participant_id")],
                   .id="Treatment") %>%
  mutate("Clean" = get_double_column(DCZo2b),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

dczo2_incorrect_count <- count_incorrect(dczo2$DCZo2a, "smaller")
dczo2_NA_count <- count_NA(dczo2$Clean)

dczo2_incorrect_indices <- get_incorrect_indices(dczo2$DCZo2a, "smaller")
dczo2_NA_indices <- get_NA_indices(dczo2$Clean)
dczo2_outliers <- c(dczo2_NA_indices, 43)  # Outlier > 50 normalized score

dczo2$Flipped[dczo2_incorrect_indices] <- 
  dczo2$Flipped[dczo2_incorrect_indices] * -1

# Response time
dczo2_time <- bind_rows(gp3[,c("DCZo2aTi_Page Submit","DCZo2bTi_Page Submit")],
                        gp4[,c("DCZo2aTi_Page Submit","DCZo2bTi_Page Submit")],
                        gp1[,c("DCZo2aTi_Page Submit","DCZo2bTi_Page Submit")],
                        gp2[,c("DCZo2aTi_Page Submit","DCZo2bTi_Page Submit")],
                        .id="Treatment") %>%
  rename("Time1"="DCZo2aTi_Page Submit",
         "Time2"="DCZo2bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p2a <- get_boxplot_ans(dczo2, dczo2_outliers, dczo2_NA_count, dczo2_answer,
                       "Detect Change Zone 2 (Incorrect incl)",
                       normalize = FALSE)

p2an <- get_boxplot_ans(dczo2, dczo2_outliers, dczo2_NA_count, dczo2_answer,
                        "Detect Change Zone 2  (Incorrect incl)",
                        normalize = TRUE)

p2b <- get_boxplot_ans(dczo2, dczo2_outliers, dczo2_NA_count, dczo2_answer,
                       "Detect Change Zone 2  (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p2bn <- get_boxplot_ans(dczo2, dczo2_outliers, dczo2_NA_count, dczo2_answer,
                        "Detect Change Zone 2  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t2a <- get_boxplot_time(dczo2_time, dczo2_outliers)

# 3 -------------------------------
# Treatment order: 1, 3, 2, 4
dczo3_answer <- 45500

dczo3 <- bind_rows(gp1[, c("DCZo3a", "DCZo3b", "participant_id")], 
                   gp3[, c("DCZo3a", "DCZo3b", "participant_id")],
                   gp2[, c("DCZo3a", "DCZo3b", "participant_id")], 
                   gp4[, c("DCZo3a", "DCZo3b", "participant_id")],
                   .id="Treatment") %>%
  mutate("Clean" = get_double_column(DCZo3b),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

dczo3_incorrect_count <- count_incorrect(dczo3$DCZo3a, "smaller")
dczo3_NA_count <- count_NA(dczo3$Clean)

dczo3_incorrect_indices <- get_incorrect_indices(dczo3$DCZo3a, "smaller")
dczo3_NA_indices <- get_NA_indices(dczo3$Clean)
dczo3_outliers <- c(dczo3_NA_indices, 2, 37)  # Outlier >20 normal score

dczo3$Flipped[dczo3_incorrect_indices] <- 
  dczo3$Flipped[dczo3_incorrect_indices] * -1

# Response time
dczo3_time <- bind_rows(gp1[,c("DCZo3aTi_Page Submit","DCZo3bTi_Page Submit")],
                        gp3[,c("DCZo3aTi_Page Submit","DCZo3bTi_Page Submit")],
                        gp2[,c("DCZo3aTi_Page Submit","DCZo3bTi_Page Submit")],
                        gp4[,c("DCZo3aTi_Page Submit","DCZo3bTi_Page Submit")],
                        .id="Treatment") %>%
  rename("Time1"="DCZo3aTi_Page Submit",
         "Time2"="DCZo3bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p3a <- get_boxplot_ans(dczo3, dczo3_outliers, dczo3_NA_count, dczo3_answer,
                       "Detect Change Zone 3 (Incorrect incl)",
                       normalize = FALSE)

p3an <- get_boxplot_ans(dczo3, dczo3_outliers, dczo3_NA_count, dczo3_answer,
                        "Detect Change Zone 3  (Incorrect incl)",
                        normalize = TRUE)

p3b <- get_boxplot_ans(dczo3, dczo3_outliers, dczo3_NA_count, dczo3_answer,
                       "Detect Change Zone 3  (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p3bn <- get_boxplot_ans(dczo3, dczo3_outliers, dczo3_NA_count, dczo3_answer,
                        "Detect Change Zone 3  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t3a <- get_boxplot_time(dczo3_time, dczo3_outliers)

# 4 -------------------------------
# Treatment order: 4, 2, 3, 1
dczo4_answer <- 10000000

dczo4 <- bind_rows(gp4[, c("DCZo4a", "DCZo4b", "participant_id")], 
                   gp2[, c("DCZo4a", "DCZo4b", "participant_id")],
                   gp3[, c("DCZo4a", "DCZo4b", "participant_id")], 
                   gp1[, c("DCZo4a", "DCZo4b", "participant_id")],
                   .id="Treatment") %>%
  mutate("Clean" = get_double_column(DCZo4b),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

dczo4_incorrect_count <- count_incorrect(dczo4$DCZo4a, "smaller")
dczo4_NA_count <- count_NA(dczo4$Clean)

dczo4_incorrect_indices <- get_incorrect_indices(dczo4$DCZo4a, "smaller")
dczo4_NA_indices <- get_NA_indices(dczo4$Clean)

dczo4$Flipped[dczo4_incorrect_indices] <- 
  dczo4$Flipped[dczo4_incorrect_indices] * -1

# Response time
dczo4_time <- bind_rows(gp4[,c("DCZo4aTi_Page Submit","DCZo4bTi_Page Submit")],
                        gp2[,c("DCZo4aTi_Page Submit","DCZo4bTi_Page Submit")],
                        gp3[,c("DCZo4aTi_Page Submit","DCZo4bTi_Page Submit")],
                        gp1[,c("DCZo4aTi_Page Submit","DCZo4bTi_Page Submit")],
                        .id="Treatment") %>%
  rename("Time1"="DCZo4aTi_Page Submit",
         "Time2"="DCZo4bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p4a <- get_boxplot_ans(dczo4, dczo4_NA_indices, dczo4_NA_count, dczo4_answer,
                       "Detect Change Zone 4 (Incorrect incl)",
                       normalize = FALSE)

p4an <- get_boxplot_ans(dczo4, dczo4_NA_indices, dczo4_NA_count, dczo4_answer,
                        "Detect Change Zone 4  (Incorrect incl)",
                        normalize = TRUE)

p4b <- get_boxplot_ans(dczo4, dczo4_NA_indices, dczo4_NA_count, dczo4_answer,
                       "Detect Change Zone 4  (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p4bn <- get_boxplot_ans(dczo4, dczo4_NA_indices, dczo4_NA_count, dczo4_answer,
                        "Detect Change Zone 4  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t4a <- get_boxplot_time(dczo4_time, dczo4_NA_indices)

###################################
# Aggregated
###################################
dczo_all <- get_aggre_df(dczo1, dczo2, dczo3, dczo4,
                          dczo1_answer, dczo2_answer,
                          dczo3_answer, dczo4_answer,
                          dczo1_NA_indices, dczo2_outliers,
                          dczo3_outliers, dczo4_NA_indices,
                          "Flipped")

kw_res <- get_kruskal_res(dczo_all) # p.value = 0.07

pall_title <- chi2_and_main_p(kw_res)

pall <- get_aggre_plot(dczo_all, pall_title)

pall <- pall + scale_y_continuous(limits = c(-3, 3))

# Response Time Aggregated ----------------------------------------
dczo_all_time <- bind_rows(dczo1_time[-dczo1_NA_indices, ], 
                           dczo2_time[-dczo2_outliers, ],
                           dczo3_time[-dczo3_outliers, ], 
                           dczo4_time[-dczo4_NA_indices, ])

dczo_all_time$Treatment <- plyr::mapvalues(dczo_all_time$Treatment,
                                           from = c("1", "2", "3", "4"),
                                           to = c("None", "StLO", "StLG", "SeLG")) %>%
  factor(levels = c("None", "StLO", "StLG", "SeLG"))

# Statistical Analysis
kw_time <- get_kruskal_time(dczo_all_time) # p.value = 0.00083
pairwise.wilcox.test(dczo_all_time$Time,
                     factor(dczo_all_time$Treatment),
                     p.adjust.method = "holm",
                     paired = FALSE)

tall_title <- chi2_and_main_p(kw_time)

tall <- get_aggre_time(dczo1_time, dczo2_time,
                       dczo3_time, dczo4_time,
                       dczo1_NA_indices, dczo2_outliers,
                       dczo3_outliers, dczo4_NA_indices,
                       tall_title)

# Add significant p-values
dczo_time_pairwise <- pairwise_wilcox_test(dczo_all_time,
                                           Time ~ Treatment,
                                           p.adjust.method = "holm",
                                           paired = FALSE,
                                           detailed = TRUE)

tall <- tall + stat_pvalue_manual(dczo_time_pairwise,
                                  label = "p.adj.signif",
                                  hide.ns = TRUE,
                                  y.position = c(245, 280))

# NA Analysis
dczo_NA_count <- dczo1_NA_count + dczo2_NA_count + dczo3_NA_count + dczo4_NA_count
# dczo_Xsq <- get_chisq(dczo_NA_count)    # pval = 0.0149 
# get_pairwise_results(dczo_NA_count)

dczo_NA_all <- bind_rows(dczo1[, c("treatment", "participant_id", "answer")], 
                         dczo2[, c("treatment", "participant_id", "answer")], 
                         dczo3[, c("treatment", "participant_id", "answer")], 
                         dczo4[, c("treatment", "participant_id", "answer")])

dczo_NA_all$answer <- factor(as.character(dczo_NA_all$answer))
dczo_NA_all$treatment <- factor(dczo_NA_all$treatment, levels = treatments)

cqtest <- cochran_qtest(dczo_NA_all, answer ~ treatment | participant_id)
cqtest$p.value <- cqtest$p

pbar_title <- chi2_and_main_p(cqtest)

pbar <- get_NA_barplot(dczo_NA_count, pbar_title)

# Plot significant p-values for NA barplot

# dczo_NonNA <- 44 - dczo_NA_count
# dczo_NA_df <- data.frame("NA" = dczo_NA_count,
#                           "Non-NA" = dczo_NonNA,
#                           row.names = c("None", "LO", "LG", "SLG"))
# 
# dczo_NA_pairwise <- pairwise_prop_test(dczo_NA_df,
#                                        p.adjust.method = "holm")

dczo_pairwise_mcnemar <- pairwise_mcnemar_test(dczo_NA_all, 
                                               answer ~ treatment | participant_id,
                                               p.adjust.method = "holm") %>%
  filter(!is.nan(p))

pbar <- pbar + stat_pvalue_manual(dczo_pairwise_mcnemar,
                                  label = "p.adj.signif",
                                  hide.ns = TRUE,
                                  y.position = c(50))


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

pairwise_effect_ci(dczo_NA_all, c("None", "SeLG"))

# Combined plot
title <-
  ggdraw() +
  draw_label("Detect Change in Zone",
             fontface = "bold.italic",
             size = 15) +
  theme(plot.background = element_rect(fill = "#e6e6e6", color = NA)) 

bottom_row <- plot_grid(pall, tall, pbar, ncol = 3, rel_heights = c(3/10, 4/10, 4/10))
pcombined <- plot_grid(title,
                       bottom_row,
                       nrow = 2,
                       rel_heights = c(0.12, 1))

#saveRDS(pcombined, file = "../rdata/Combined_DCZo.rds")
#ggsave("Combined_DCZo.pdf", pcombined, path = "../plots/", width = 6, height = 4)

###################################
# Summary Data
###################################
summary(dczo_all$Normal, na.rm=TRUE)

dczo_all %>%
  mutate(id = rep(1:44, 4)) %>%
  pivot_wider(names_from = Treatment, values_from = Normal) %>%
  summary

sum(dczo_NA_count / 176) * 100

dczo_NA_all %>%
  group_by(treatment) %>%
  summarise(propor = sum(answer == TRUE)/44 * 100)

summary(dczo_all_time)

for (i in treatments) {
  print(c("Treatment", i))
  print(summary(dczo_all_time[dczo_all_time$Treatment == i, "Time"]))
}


########################################
#               Unused                 #
########################################

# Grouped
pgrouped <- get_grouped_ans(dczo1, dczo2, dczo3, dczo4,
                            dczo1_answer, dczo2_answer,
                            dczo3_answer, dczo4_answer,
                            dczo1_NA_indices, dczo2_outliers,
                            dczo3_outliers, dczo4_NA_indices,
                            dczo1_NA_count, dczo2_NA_count,
                            dczo3_NA_count, dczo4_NA_count,
                            "Detect Change Zone", "Flipped")[[2]]

tgrouped <- get_grouped_time(dczo1_time, dczo2_time,
                             dczo3_time, dczo4_time,
                             dczo1_NA_indices, dczo2_outliers,
                             dczo3_outliers, dczo4_NA_indices,
                             "Response Time Aggregated (Seconds)")