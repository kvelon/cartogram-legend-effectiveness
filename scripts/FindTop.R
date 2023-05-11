
# Find Top Analysis
library(tidyverse)
library(ggpubr)
library(cowplot)
library(rstatix)
source("scripts/Util.R")

find_top <- c("FTAU1", "FTAU2",
              "FTAU3", "FTAU4",
              "FTAU1Ti_Page Submit", "FTAU2Ti_Page Submit",
              "FTAU3Ti_Page Submit", "FTAU4Ti_Page Submit")

treatments <- c("None", "StLO", "StLG", "SeLG")

#############################
# Read in data
###################################
gp1 <- read_csv("data/group1.csv") %>%
  slice(3:n()) %>%
  select(find_top) %>%
  mutate(participant_id = row_number())

gp2 <- read_csv("data/group2.csv") %>%
  slice(3:n()) %>%
  select(find_top) %>%
  mutate(participant_id = row_number() + 11)

gp3 <- read_csv("data/group3.csv") %>%
  slice(3:n()) %>%
  select(find_top) %>%
  mutate(participant_id = row_number() + 22)

gp4 <- read_csv("data/group4.csv") %>%
  slice(3:n()) %>%
  select(find_top) %>%
  mutate(participant_id = row_number() + 33)

###################################
# Question 1
# Treament order 3, 4, 2, 1
###################################
ft1_answer <- "Makkah (MK)"

ft1 <- bind_rows(gp3[, c("FTAU1", "participant_id")], 
                 gp4[, c("FTAU1", "participant_id")],
                 gp2[, c("FTAU1", "participant_id")], 
                 gp1[, c("FTAU1", "participant_id")]) %>%
       mutate(answer = FTAU1 == ft1_answer,
              treatment = rep(treatments, each = 11)) 

ft1_incorrect <- count_incorrect(ft1$FTAU1, ft1_answer)
ft1_incorrect_indices <- which(ft1$FTAU1 != ft1_answer)
  
p1 <- get_correct_barplot(ft1_incorrect, "Find Top 1")

ft1_time <- bind_rows(gp3[, "FTAU1Ti_Page Submit"], gp4[, "FTAU1Ti_Page Submit"],
                      gp2[, "FTAU1Ti_Page Submit"], gp1[, "FTAU1Ti_Page Submit"],
                       .id="Treatment") %>%
  rename("TimeString"="FTAU1Ti_Page Submit") %>%
  mutate(Time=as.double(TimeString))

t1 <- get_boxplot_time(ft1_time)


###################################
# Question 2
# Treament order 2, 1, 4, 3
###################################
ft2_answer <- "Almaty (AA)"

ft2 <- bind_rows(gp2[, c("FTAU2", "participant_id")], 
                 gp1[, c("FTAU2", "participant_id")],
                 gp4[, c("FTAU2", "participant_id")], 
                 gp3[, c("FTAU2", "participant_id")]) %>%
  mutate(answer = FTAU2 == ft2_answer,
         treatment = rep(treatments, each = 11)) 

ft2_incorrect <- count_incorrect(ft2$FTAU2, ft2_answer)
ft2_incorrect_indices <- which(ft2$FTAU2 != ft2_answer)

p2 <- get_correct_barplot(ft2_incorrect, "Find Top 2")

ft2_time <- bind_rows(gp2[, "FTAU2Ti_Page Submit"], gp1[, "FTAU2Ti_Page Submit"],
                      gp4[, "FTAU2Ti_Page Submit"], gp3[, "FTAU2Ti_Page Submit"],
                      .id="Treatment") %>%
  rename("TimeString"="FTAU2Ti_Page Submit") %>%
  mutate(Time=as.double(TimeString))

t2 <- get_boxplot_time(ft2_time)

###################################
# Question 3
# Treament order 4, 3, 1, 2
###################################
ft3_answer <- "Zuid-Holland (ZH)"

ft3 <- bind_rows(gp4[, c("FTAU3", "participant_id")], 
                 gp3[, c("FTAU3", "participant_id")],
                 gp1[, c("FTAU3", "participant_id")], 
                 gp2[, c("FTAU3", "participant_id")]) %>%
  mutate(answer = FTAU3 == ft3_answer,
         treatment = rep(treatments, each = 11)) 

ft3_incorrect <- count_incorrect(ft3$FTAU3, ft3_answer)
ft3_incorrect_indices <- which(ft3$FTAU3 != ft3_answer)

p3 <- get_correct_barplot(ft3_incorrect, "Find Top 3")

ft3_time <- bind_rows(gp4[, "FTAU3Ti_Page Submit"], gp3[, "FTAU3Ti_Page Submit"],
                      gp1[, "FTAU3Ti_Page Submit"], gp2[, "FTAU3Ti_Page Submit"],
                      .id="Treatment") %>%
  rename("TimeString"="FTAU3Ti_Page Submit") %>%
  mutate(Time=as.double(TimeString))

t3 <- get_boxplot_time(ft3_time)

###################################
# Question 4
# Treament order 1, 2, 3, 4
###################################
ft4_answer <- "Kano (KN)"

ft4 <- bind_rows(gp1[, c("FTAU4", "participant_id")], 
                 gp2[, c("FTAU4", "participant_id")],
                 gp3[, c("FTAU4", "participant_id")],
                 gp4[, c("FTAU4", "participant_id")]) %>%
  mutate(answer = FTAU4 == ft4_answer,
         treatment = rep(treatments, each = 11)) 

ft4_incorrect <- count_incorrect(ft4$FTAU4, ft4_answer)
ft4_incorrect_indices <- which(ft4$FTAU4 != ft4_answer)

p4 <- get_correct_barplot(ft4_incorrect, "Find Top 4")

ft4_time <- bind_rows(gp1[, "FTAU4Ti_Page Submit"], gp2[, "FTAU4Ti_Page Submit"],
                      gp3[, "FTAU4Ti_Page Submit"], gp4[, "FTAU4Ti_Page Submit"],
                      .id="Treatment") %>%
  rename("TimeString"="FTAU4Ti_Page Submit") %>%
  mutate(Time=as.double(TimeString))

t4 <- get_boxplot_time(ft4_time)

###################################
# Aggregated
###################################

ftall_incorrect <- ft1_incorrect + ft2_incorrect +
  ft3_incorrect + ft4_incorrect

# ftall_correct <- 44 - ftall_incorrect
# 
# chisq_res <- chisq.test(matrix(c(ftall_incorrect, ftall_correct), 
#                         ncol = 2))  # pval = 0.4977

# Cochran Q Test
ftall <- bind_rows(ft1[, -1], ft2[, -1], 
                   ft3[, -1], ft4[, -1])

ftall$answer <- factor(as.character(ftall$answer))

cqtest <- cochran_qtest(ftall, answer ~ treatment | participant_id)
cqtest$p.value <- cqtest$p

pairwise_mcnemar_test(ftall, answer ~ treatment | participant_id)

pall_title <- chi2_and_main_p(cqtest)

pall <- get_correct_barplot(ftall_incorrect, pall_title,
                            44)

# ggsave("Correct_FindTop.pdf", pall, path = "Plots/", width = 5, height = 4)

ftall_time <- bind_rows(ft1_time[-ft1_incorrect_indices, ], 
                        ft2_time[-ft2_incorrect_indices, ],
                        ft3_time[-ft3_incorrect_indices, ], 
                        ft4_time[-ft4_incorrect_indices, ])

kw_time <- get_kruskal_time(ftall_time)  # pval = 0.1265

tall_title <- chi2_and_main_p(kw_time)

tall <- get_aggre_time(ft1_time, ft2_time, ft3_time, ft4_time,
                       ft1_incorrect, ft2_incorrect, ft3_incorrect, ft4_incorrect,
                       tall_title)

###################################
# Combined plot
###################################
title <-
  ggdraw() +
  draw_label("Find Top",
             fontface = "bold.italic",
             size = 15) +
  theme(plot.background = element_rect(fill = "#e6e6e6", color = NA)) 


bottom_row <- plot_grid(NULL, pall, NULL, tall, NULL,
                        nrow = 1, 
                        rel_widths  = c(1/11, 4/11, 1/11, 4/11, 1/11))

pcombined <- plot_grid(title,
                       bottom_row,
                       nrow = 2,
                       rel_heights = c(0.12, 1))

#saveRDS(pcombined, file = "../rdata/Combined_FTAU.rds")
#ggsave("Combined_FTAU.pdf", pcombined, path = "../plots/", width = 6, height = 4)

###################################
# Summary Data
###################################

ftall %>%
  group_by(treatment) %>%
  summarise(propor = sum(answer == FALSE)/44 * 100)

sum(ftall_incorrect / 176) * 100

summary(ftall_time$Time)
  
for (i in 1:4) {
    print(c("Treatment", treatments[i]))
    print(summary(ftall_time[ftall_time$Treatment == i, "Time"]))
  }
