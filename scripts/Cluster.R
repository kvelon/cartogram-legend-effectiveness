
# Cluster Analysis
library(tidyverse)
library(ggpubr)
library(rstatix)
library(cowplot)
source("Util.R")

cluster <- c("CluAU1", "CluAU2",
             "CluAU3", "CluAU4",
             "CluAU1Ti_Page Submit", "CluAU2Ti_Page Submit",
             "CluAU3Ti_Page Submit", "CluAU4Ti_Page Submit") 

treatments <- c("None", "StLO", "StLG", "SeLG")


#############################
# Read in data
#############################
gp1 <- read_csv("../data/group1.csv") %>%
  slice(3:n()) %>%
  select(cluster) %>%
  mutate(participant_id = row_number())

gp2 <- read_csv("../data/group2.csv") %>%
  slice(3:n()) %>%
  select(cluster) %>%
  mutate(participant_id = row_number() + 11)

gp3 <- read_csv("../data/group3.csv") %>%
  slice(3:n()) %>%
  select(cluster) %>%
  mutate(participant_id = row_number() + 22)

gp4 <- read_csv("../data/group4.csv") %>%
  slice(3:n()) %>%
  select(cluster) %>%
  mutate(participant_id = row_number() + 33)

###################################
# Question 1
# Treament order 4, 1, 3, 2
###################################
clu1_answer <- "Province No. 2 (TW)"

clu1 <- bind_rows(gp4[, c("CluAU1", "participant_id")], 
                  gp1[, c("CluAU1", "participant_id")],
                  gp3[, c("CluAU1", "participant_id")], 
                  gp2[, c("CluAU1", "participant_id")]) %>%
  mutate(answer = CluAU1 == clu1_answer,
         treatment = rep(treatments, each = 11)) 

clu1_incorrect <- count_incorrect(clu1$CluAU1, clu1_answer)
clu1_incorrect_indices <- which(clu1$CluAU1 != clu1_answer)

p1 <- get_correct_barplot(clu1_incorrect, "Cluster 1")

clu1_time <- bind_rows(gp4[, "CluAU1Ti_Page Submit"], gp1[, "CluAU1Ti_Page Submit"],
                       gp3[, "CluAU1Ti_Page Submit"], gp2[, "CluAU1Ti_Page Submit"],
                       .id="Treatment") %>%
  rename("TimeString"="CluAU1Ti_Page Submit") %>%
  mutate(Time=as.double(TimeString))

t1 <- get_boxplot_time(clu1_time)

###################################
# Question 2
# Treament order 3, 2, 1, 4
###################################
clu2_answer <- "Alabama (AL)"

clu2 <- bind_rows(gp3[, c("CluAU2", "participant_id")], 
                  gp2[, c("CluAU2", "participant_id")],
                  gp1[, c("CluAU2", "participant_id")], 
                  gp4[, c("CluAU2", "participant_id")]) %>%
  mutate(answer = CluAU2 == clu2_answer,
         treatment = rep(treatments, each = 11)) 

clu2_incorrect <- count_incorrect(clu2$CluAU2, clu2_answer)
clu2_incorrect_indices <- which(clu2$CluAU2 != clu2_answer)

p2 <- get_correct_barplot(clu2_incorrect, "Cluster 2")

clu2_time <- bind_rows(gp3[, "CluAU2Ti_Page Submit"], gp2[, "CluAU2Ti_Page Submit"],
                       gp1[, "CluAU2Ti_Page Submit"], gp4[, "CluAU2Ti_Page Submit"],
                       .id="Treatment") %>%
  rename("TimeString"="CluAU2Ti_Page Submit") %>%
  mutate(Time=as.double(TimeString)) 

t2 <- get_boxplot_time(clu2_time)

###################################
# Question 3
# Treament order 1, 4, 2, 3
###################################
clu3_answer <- "Heves (HE)"

clu3 <- bind_rows(gp1[, c("CluAU3", "participant_id")], 
                  gp4[, c("CluAU3", "participant_id")],
                  gp2[, c("CluAU3", "participant_id")], 
                  gp3[, c("CluAU3", "participant_id")]) %>%
  mutate(answer = CluAU3 == clu3_answer,
         treatment = rep(treatments, each = 11)) 

clu3_incorrect <- count_incorrect(clu3$CluAU3, clu3_answer)
clu3_incorrect_indices <- which(clu3$CluAU3 != clu3_answer)

p3 <- get_correct_barplot(clu3_incorrect, "Cluster 3")


clu3_time <- bind_rows(gp1[, "CluAU3Ti_Page Submit"], gp4[, "CluAU3Ti_Page Submit"],
                       gp2[, "CluAU3Ti_Page Submit"], gp3[, "CluAU3Ti_Page Submit"],
                       .id="Treatment") %>%
  rename("TimeString"="CluAU3Ti_Page Submit") %>%
  mutate(Time=as.double(TimeString))

t3 <- get_boxplot_time(clu3_time)

###################################
# Question 4
# Treament order 2, 3, 4, 1
###################################
clu4_answer <- "Chaco (CC)"

clu4 <- bind_rows(gp2[, c("CluAU4", "participant_id")], 
                  gp3[, c("CluAU4", "participant_id")],
                  gp4[, c("CluAU4", "participant_id")], 
                  gp1[, c("CluAU4", "participant_id")]) %>%
  mutate(answer = CluAU4 == clu4_answer,
         treatment = rep(treatments, each = 11)) 

clu4_incorrect <- count_incorrect(clu4$CluAU4, clu4_answer)
clu4_incorrect_indices <- which(clu4$CluAU4 != clu4_answer)

p4 <- get_correct_barplot(clu4_incorrect, "Cluster 4")


clu4_time <- bind_rows(gp2[, "CluAU4Ti_Page Submit"], gp3[, "CluAU4Ti_Page Submit"],
                       gp4[, "CluAU4Ti_Page Submit"], gp1[, "CluAU4Ti_Page Submit"],
                       .id="Treatment") %>%
  rename("TimeString"="CluAU4Ti_Page Submit") %>%
  mutate(Time=as.double(TimeString))
  # slice(-clu4_incorrect_indices)

t4 <- get_boxplot_time(clu4_time)


###################################
# Aggregated
###################################
cluall_incorrect <- clu1_incorrect + clu2_incorrect +
  clu3_incorrect + clu4_incorrect

# cluall_correct <- 44 - cluall_incorrect
# 
# chisq_res <- chisq.test(matrix(c(cluall_incorrect, cluall_correct), 
#                                ncol = 2))  # pval = 0.4186

# Cochran Q Test
cluall <- bind_rows(clu1[, -1], clu2[, -1], 
                    clu3[, -1], clu4[, -1])

cluall$answer <- factor(as.character(cluall$answer))

cqtest <- cochran_qtest(cluall, answer ~ treatment | participant_id)
cqtest$p.value <- cqtest$p

pairwise_mcnemar_test(cluall, answer ~ treatment | participant_id)

pall_title <- chi2_and_main_p(cqtest)

pall <- get_correct_barplot(cluall_incorrect, pall_title,
                            44)

###################################
# Response Time Aggregated
###################################

cluall_time <- bind_rows(clu1_time[-clu1_incorrect_indices, ],
                         clu2_time[-clu2_incorrect_indices, ],
                         clu3_time[-clu3_incorrect_indices, ], 
                         clu4_time[-clu4_incorrect_indices, ])

kw_time <- get_kruskal_time(cluall_time)  # pval = 0.007

tall_title <- chi2_and_main_p(kw_time)

tall <- get_aggre_time(clu1_time, clu2_time, clu3_time, clu4_time,
                       clu1_incorrect_indices, clu2_incorrect_indices,
                       clu3_incorrect_indices, clu4_incorrect_indices,
                       tall_title)

# Add significant p-values
pairwise.wilcox.test(cluall_time$Time,
                     factor(cluall_time$Treatment),
                     p.adjust.method = "holm",
                     paired = FALSE)

cluall_time$Treatment <- plyr::mapvalues(cluall_time$Treatment,
                                         from = c("1", "2", "3", "4"),
                                         to = c("None", "StLO", "StLG", "SeLG")) %>%
  factor(levels = c("None", "StLO", "StLG", "SeLG"))

cluall_time_pairwise <- pairwise_wilcox_test(cluall_time,
                                             Time ~ Treatment,
                                             p.adjust.method = "holm",
                                             paired = FALSE,
                                             detailed = TRUE)

tall <- tall + stat_pvalue_manual(cluall_time_pairwise,
                                  label = "p.adj.signif",
                                  hide.ns = TRUE,
                                  y.position = c(220, 260))

###################################
# Combined plot
###################################
title <-
  ggdraw() +
  draw_label("Cluster",
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

#saveRDS(pcombined, file = "../rdata/Combined_CluAU.rds")
#ggsave("Combined_CluAU.pdf", pcombined, path = "../plots/", width = 6, height = 4)

###################################
# Summary Data
###################################

cluall %>%
  group_by(treatment) %>%
  summarise(propor = sum(answer == FALSE)/44 * 100)

sum(cluall_incorrect / 176) * 100

summary(cluall_time$Time)

for (i in unique(cluall_time$Treatment)) {
  print(c("Treatment", i))
  print(summary(cluall_time[cluall_time$Treatment == i, "Time"]))
}
