# Estimate AU Analysis
library(tidyverse)
library(ggpubr)
library(cowplot)
library(rstatix)
library(onewaytests)
library(exact2x2)

source("Util.R")

est_au <- c("EstAU1", "EstAU2", "EstAU3", "EstAU4",
            "EstAU1Ti_Page Submit", "EstAu2Ti_Page Submit",
            "EstAu3Ti_Page Submit", "EstAU4Ti_Page Submit")

treatments <- c("None", "StLO", "StLG", "SeLG")

# Read in data
gp1 <- read_csv("../data/group1.csv") %>%
  slice(3:n()) %>%
  select(est_au) %>%
  mutate(participant_id = row_number())

gp2 <- read_csv("../data/group2.csv") %>%
  slice(3:n()) %>%
  select(est_au) %>%
  mutate(participant_id = row_number() + 11)

gp3 <- read_csv("../data/group3.csv") %>%
  slice(3:n()) %>%
  select(est_au) %>%
  mutate(participant_id = row_number() + 22)

gp4 <- read_csv("../data/group4.csv") %>%
  slice(3:n()) %>%
  select(est_au) %>%
  mutate(participant_id = row_number() + 33)

# 1 -------------------------------
# Treatment order: 1, 3, 2, 4
estau1_answer <- 1800000

estau1 <- bind_rows(gp1[, c("EstAU1", "participant_id")], 
                    gp3[, c("EstAU1", "participant_id")],
                    gp2[, c("EstAU1", "participant_id")], 
                    gp4[, c("EstAU1", "participant_id")],
                   .id="Treatment") %>%
          mutate("Clean" = get_double_column(EstAU1),
                 answer = is.na(Clean),
                 treatment = rep(treatments, each = 11))

estau1_NA <- count_NA(estau1$Clean)
estau1_NA_indices <- get_NA_indices(estau1$Clean)

# Response time
estau1_time <- bind_rows(gp1[,"EstAU1Ti_Page Submit"],
                         gp3[,"EstAU1Ti_Page Submit"],
                         gp2[,"EstAU1Ti_Page Submit"],
                         gp4[,"EstAU1Ti_Page Submit"],
                         .id="Treatment") %>%
  rename("TimeString"="EstAU1Ti_Page Submit") %>%
  mutate(Time=as.double(TimeString))

# Plots
p1 <- get_boxplot_ans(estau1, estau1_NA_indices, estau1_NA,
                     estau1_answer,
                     "Estimate AU Question 1",
                     normalize = FALSE)

p1n <- get_boxplot_ans(estau1, estau1_NA_indices, estau1_NA,
                       estau1_answer,
                       "Estimate AU Question 1 (Normalised)",
                       normalize = TRUE)


t1 <- get_boxplot_time(estau1_time, estau1_NA_indices)

# 2 -------------------------------
# Treatment order: 2, 4, 1, 3
estau2_answer <- 2600000

estau2 <- bind_rows(gp2[, c("EstAU2", "participant_id")], 
                    gp4[, c("EstAU2", "participant_id")],
                    gp1[, c("EstAU2", "participant_id")], 
                    gp3[, c("EstAU2", "participant_id")],
                    .id="Treatment") %>%
  mutate("Clean" = get_double_column(EstAU2),
         answer = is.na(Clean),
         treatment = rep(treatments, each = 11))

estau2_NA <- count_NA(estau2$Clean)
estau2_NA_indices <- get_NA_indices(estau2$Clean)

# Response time
estau2_time <- bind_rows(gp2[,"EstAu2Ti_Page Submit"],
                         gp4[,"EstAu2Ti_Page Submit"],
                         gp1[,"EstAu2Ti_Page Submit"],
                         gp3[,"EstAu2Ti_Page Submit"],
                         .id="Treatment") %>%
  rename("TimeString"="EstAu2Ti_Page Submit") %>%
  mutate(Time=as.double(TimeString))

# Plots
p2 <- get_boxplot_ans(estau2, estau2_NA_indices, estau2_NA,
                      estau2_answer,
                      "Estimate AU Question 2")

p2n <- get_boxplot_ans(estau2, estau2_NA_indices, estau2_NA,
                       estau2_answer,
                       "Estimate AU Question 2 (Normalized)", 
                       normalize=TRUE)

t2 <- get_boxplot_time(estau2_time, estau2_NA_indices)

# 3 -------------------------------
# Treatment order: 3, 2, 4, 1
estau3_answer <- 200000

estau3 <- bind_rows(gp3[, c("EstAU3", "participant_id")], 
                    gp2[, c("EstAU3", "participant_id")],
                    gp4[, c("EstAU3", "participant_id")], 
                    gp1[, c("EstAU3", "participant_id")],
                    .id="Treatment") %>%
  mutate("Clean" = get_double_column(EstAU3),
         answer = is.na(Clean),
         treatment = rep(treatments, each = 11))

estau3_NA <- count_NA(estau3$Clean)
estau3_NA_indices <- get_NA_indices(estau3$Clean) 
estau3_outliers <- c(estau3_NA_indices, 21)

# Response time
estau3_time <- bind_rows(gp3[,"EstAu3Ti_Page Submit"],
                         gp2[,"EstAu3Ti_Page Submit"],
                         gp4[,"EstAu3Ti_Page Submit"],
                         gp1[,"EstAu3Ti_Page Submit"],
                         .id="Treatment") %>%
  rename("TimeString"="EstAu3Ti_Page Submit") %>%
  mutate(Time=as.double(TimeString))

p3 <- get_boxplot_ans(estau3, estau3_outliers,
                      estau3_NA, estau3_answer,
                      "Estimate AU Question 3", FALSE)

p3n <- get_boxplot_ans(estau3, estau3_outliers,
                      estau3_NA, estau3_answer,
                      "Estimate AU Question 3 (Normalized)", 
                      normalize = TRUE)

t3 <- get_boxplot_time(estau3_time, estau3_outliers)

# 4 -------------------------------
# Treatment order: 4, 1, 3, 2
estau4_answer <- 3400000

estau4 <- bind_rows(gp4[, c("EstAU4", "participant_id")], 
                    gp1[, c("EstAU4", "participant_id")],
                    gp3[, c("EstAU4", "participant_id")], 
                    gp2[, c("EstAU4", "participant_id")],
                    .id="Treatment") %>%
  mutate("Clean" = get_double_column(EstAU4),
         answer = is.na(Clean),
         treatment = rep(treatments, each = 11))

estau4_NA <- count_NA(estau4$Clean)
estau4_NA_indices <- get_NA_indices(estau4$Clean)

# Response time
estau4_time <- bind_rows(gp4[,"EstAU4Ti_Page Submit"],
                         gp1[,"EstAU4Ti_Page Submit"],
                         gp3[,"EstAU4Ti_Page Submit"],
                         gp2[,"EstAU4Ti_Page Submit"],
                         .id="Treatment") %>%
  rename("TimeString"="EstAU4Ti_Page Submit") %>%
  mutate(Time=as.double(TimeString))

# Plots
p4 <- get_boxplot_ans(estau4, estau4_NA_indices, estau4_NA,
                      estau4_answer,
                      "Estimate AU Question 4")

p4n <- get_boxplot_ans(estau4, estau4_NA_indices, estau4_NA,
                       estau4_answer,
                       "Estimate AU Question 4 (Normalized)",
                       normalize = TRUE)

t4 <- get_boxplot_time(estau4_time, estau4_NA_indices)

###################################
# Aggregated
###################################
estau_all <- get_aggre_df(estau1, estau2, estau3, estau4,
                          estau1_answer, estau2_answer,
                          estau3_answer, estau4_answer,
                          estau1_NA_indices, estau2_NA_indices,
                          estau3_outliers, estau4_NA_indices)

kw_res <- get_kruskal_res(estau_all) # p.value = 0.43

pall_title <- chi2_and_main_p(kw_res)

pall <- get_aggre_plot(estau_all, pall_title)

pall <- pall + scale_y_continuous(limits = c(-1.5, 1.5)) +
  ylab("(response - correct answer) / correct answer")

# pall <- pall + scale_y_continuous(limits = c(-3, 3))

# Response Time Aggregated --------------------------
estau_all_time <- bind_rows(estau1_time[-estau1_NA_indices, ], 
                            estau2_time[-estau2_NA_indices, ],
                            estau3_time[-estau3_NA_indices, ],
                            estau4_time[-estau4_NA_indices, ])

kw_time <- get_kruskal_time(estau_all_time) # p.value = 0.9296

tall_title <- chi2_and_main_p(kw_time)

tall <- get_aggre_time(estau1_time, estau2_time,
                       estau3_time, estau4_time,
                       estau1_NA_indices, estau2_NA_indices,
                       estau3_outliers, estau4_NA_indices,
                       tall_title)

# NA Analysis
estau_NA <- estau1_NA + estau2_NA + estau3_NA + estau4_NA
estau_Xsq <- get_chisq(estau_NA)  # pval = 1.98e-08
get_pairwise_results(estau_NA)

# Cochran Q Test
estau_NA_all <- bind_rows(estau1[, c("treatment", "participant_id", "answer")], 
                          estau2[, c("treatment", "participant_id", "answer")], 
                          estau3[, c("treatment", "participant_id", "answer")], 
                          estau4[, c("treatment", "participant_id", "answer")])

#estau_NA_all$answer <- factor(as.character(estau_NA_all$answer))

cqtest <- cochran_qtest(estau_NA_all, answer ~ treatment | participant_id)
cqtest$p.value <- cqtest$p


pbar_title <- chi2_and_main_p(cqtest)
pbar <- get_NA_barplot(estau_NA, pbar_title)

# Plot significant p-values

estau_pairwise_mcnemar <- pairwise_mcnemar_test(estau_NA_all, 
                                                answer ~ treatment | participant_id,
                                                p.adjust.method = "holm") %>%
  filter(!is.nan(p))


pbar <- pbar + stat_pvalue_manual(estau_pairwise_mcnemar,
                                  label = "p.adj.signif",
                                  hide.ns = TRUE,
                                  y.position = c(57, 50, 71, 64))

###################################
# Confidence Interval for NA pairwise results
###################################

pairwise_effect_ci <- function(NA_all, pair) {
  
  m <- NA_all %>%
    filter(treatment %in% pair) %>%
    group_by(factor(treatment, levels = treatments)) %>%
    summarise(nores = sum(answer),
              gotres = sum(!answer)) %>%
    select(-1) %>%
    as.matrix() %>%
    t()
  
  fisher.exact(m, or = 1, alternative = "two.sided",
               tsmethod = "central", conf.int = TRUE, conf.level = 0.95)
}

pairwise_effect_ci(estau_NA_all, c("None", "StLG"))
pairwise_effect_ci(estau_NA_all, c("None", "SeLG"))
pairwise_effect_ci(estau_NA_all, c("StLO", "StLG"))
pairwise_effect_ci(estau_NA_all, c("StLO", "SeLG"))

na_pairwise_effect_ci <- function(NA_all, pair) {
  
  m <- NA_all %>%
    filter(treatment %in% pair) %>%
    group_by(factor(treatment, levels = treatments)) %>%
    summarise(nores = sum(answer),
              gotres = sum(!answer)) %>%
    select(-1) %>%
    as.matrix() %>%
    t()
  
    mcnemar.exact(m, conf.level = 0.95)
}

na_pairwise_effect_ci(estau_NA_all, c("None", "StLG"))
na_pairwise_effect_ci(estau_NA_all, c("None", "SeLG"))
na_pairwise_effect_ci(estau_NA_all, c("StLO", "StLG"))
na_pairwise_effect_ci(estau_NA_all, c("StLO", "SeLG"))


## END OF CIs
#################################################


# Combined plot
title <-
  ggdraw() +
  draw_label("Estimate Administrative Unit",
             fontface = "bold.italic",
             size = 15) +
  theme(plot.background = element_rect(fill = "#e6e6e6", color = NA)) 

bottom_row <- plot_grid(pall, tall, pbar, ncol = 3, rel_heights = c(3/9, 3/9, 3/9))
pcombined <- plot_grid(title,
                       bottom_row,
                       nrow = 2,
                       rel_heights = c(0.12, 1))

#saveRDS(pcombined, file = "../rdata/Combined_EstAU.rds")
#ggsave("Combined_EstAU.pdf", pcombined, path = "../plots/", width = 6, height = 4)

###################################
# Summary Data
###################################
summary(estau_all$Normal, na.rm=TRUE)

estau_all %>%
  mutate(id = rep(1:44, 4)) %>%
  pivot_wider(names_from = Treatment, values_from = Normal) %>%
  summary

sum(estau_NA / 176) * 100

estau_NA_all %>%
  group_by(treatment) %>%
  summarise(propor = sum(answer)/44 * 100)

summary(estau_all_time)
  

for (i in 1:4) {
  print(c("Treatment", treatments[i]))
  print(summary(estau_all_time[estau_all_time$Treatment == i, "Time"]))
}

# For None Treatment
summary(estau_all[estau_all$Treatment == "None", ])
mean(abs(estau_all$Normal[estau_all$Treatment == "None"]), na.rm = TRUE)
sd(abs(estau_all$Normal[estau_all$Treatment == "None"]), na.rm = TRUE)
median(abs(estau_all$Normal[estau_all$Treatment == "None"]), na.rm = TRUE)
hist(estau_all$Normal[estau_all$Treatment == "None"],
     xlim = c(-1, 1),
     breaks = 20,
     col = "yellow",
     main = "Histogram for None Treatment",
     xlab = "Normalised Responses")

