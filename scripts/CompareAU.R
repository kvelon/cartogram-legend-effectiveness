
# Compare AU Analysis
library(tidyverse)
library(ggpubr)
library(rstatix)
library(cowplot)
library(exact2x2)

source("Util.R")

com_au <- c("ComAU1a", "ComAU1b_1",
            "ComAU2a", "ComAU2b_1",
            "ComAU3a", "ComAU3b_1",
            "ComAU4a", "ComAU4b_1",
            "ComAU1aTi_Page Submit","ComAU1bTi_Page Submit",
            "ComAU2aTi_Page Submit","ComAU2bTi_Page Submit",
            "ComAU3aTi_Page Submit","ComAU3bTi_Page Submit",
            "ComAU4aTi_Page Submit","ComAU4bTi_Page Submit")

treatments <- c("None", "StLO", "StLG", "SeLG")

#############################
# Read in data
gp1 <- read_csv("../data/group1.csv") %>%
  slice(3:n()) %>%
  select(com_au) %>%
  mutate(participant_id = row_number())

gp2 <- read_csv("../data/group2.csv") %>%
  slice(3:n()) %>%
  select(com_au) %>%
  mutate(participant_id = row_number() + 11)

gp3 <- read_csv("../data/group3.csv") %>%
  slice(3:n()) %>%
  select(com_au) %>%
  mutate(participant_id = row_number() + 22)

gp4 <- read_csv("../data/group4.csv") %>%
  slice(3:n()) %>%
  select(com_au) %>%
  mutate(participant_id = row_number() + 33)

# 1 -------------------------------
# Treatment order: 2, 3, 4, 1 
comau1_answer <- 240000

comau1 <- bind_rows(gp2[, c("ComAU1a", "ComAU1b_1", "participant_id")], 
                    gp3[, c("ComAU1a", "ComAU1b_1", "participant_id")],
                    gp4[, c("ComAU1a", "ComAU1b_1", "participant_id")], 
                    gp1[, c("ComAU1a", "ComAU1b_1", "participant_id")],
                    .id="Treatment") %>%
  mutate("Clean" = get_double_column(ComAU1b_1),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

comau1_incorrect_count <- count_incorrect(comau1$ComAU1a, "smaller")
comau1_NA_count <- count_NA(comau1$Clean)

comau1_incorrect_indices <- get_incorrect_indices(comau1$ComAU1a, "smaller")
comau1_NA_indices <- get_NA_indices(comau1$Clean)

comau1$Flipped[comau1_incorrect_indices] <- 
  comau1$Flipped[comau1_incorrect_indices] * -1

# Response time
comau1_time <- bind_rows(gp2[,c("ComAU1aTi_Page Submit","ComAU1bTi_Page Submit")],
                         gp3[,c("ComAU1aTi_Page Submit","ComAU1bTi_Page Submit")],
                         gp4[,c("ComAU1aTi_Page Submit","ComAU1bTi_Page Submit")],
                         gp1[,c("ComAU1aTi_Page Submit","ComAU1bTi_Page Submit")],
                         .id="Treatment") %>%
  rename("Time1"="ComAU1aTi_Page Submit",
         "Time2"="ComAU1bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p1a <- get_boxplot_ans(comau1, comau1_NA_indices, comau1_NA_count, comau1_answer,
                       "Compare AU 1 (Incorrect incl)",
                       normalize = FALSE)

p1an <- get_boxplot_ans(comau1, comau1_NA_indices, comau1_NA_count, comau1_answer,
                        "Compare AU 1 (Incorrect incl)",
                        normalize = TRUE)

p1b <- get_boxplot_ans(comau1, comau1_NA_indices, comau1_NA_count, comau1_answer,
                       "Compare AU 1  (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p1bn <- get_boxplot_ans(comau1, comau1_NA_indices, comau1_NA_count, comau1_answer,
                        "Compare AU 1  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t1a <- get_boxplot_time(comau1_time, comau1_NA_indices)


# 2 -------------------------------
# Treatment order: 3, 4, 1, 2 
comau2_answer <- 400000

comau2 <- bind_rows(gp3[, c("ComAU2a", "ComAU2b_1", "participant_id")], 
                    gp4[, c("ComAU2a", "ComAU2b_1", "participant_id")],
                    gp1[, c("ComAU2a", "ComAU2b_1", "participant_id")], 
                    gp2[, c("ComAU2a", "ComAU2b_1", "participant_id")],
                    .id="Treatment") %>%
  mutate("Clean" = get_double_column(ComAU2b_1),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

comau2_incorrect_count <- count_incorrect(comau2$ComAU2a, "smaller")
comau2_NA_count <- count_NA(comau2$Clean)

comau2_incorrect_indices <- get_incorrect_indices(comau2$ComAU2a, "smaller")
comau2_NA_indices <- get_NA_indices(comau2$Clean)

comau2$Flipped[comau2_incorrect_indices] <- 
  comau2$Flipped[comau2_incorrect_indices] * -1

# Response time
comau2_time <- bind_rows(gp3[,c("ComAU2aTi_Page Submit","ComAU2bTi_Page Submit")],
                         gp4[,c("ComAU2aTi_Page Submit","ComAU2bTi_Page Submit")],
                         gp1[,c("ComAU2aTi_Page Submit","ComAU2bTi_Page Submit")],
                         gp2[,c("ComAU2aTi_Page Submit","ComAU2bTi_Page Submit")],
                         .id="Treatment") %>%
  rename("Time1"="ComAU2aTi_Page Submit",
         "Time2"="ComAU2bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p2a <- get_boxplot_ans(comau2, comau2_NA_indices, comau2_NA_count, comau2_answer,
                       "Compare AU 2 (Incorrect incl)",
                       normalize = FALSE)

p2an <- get_boxplot_ans(comau2, comau2_NA_indices, comau2_NA_count, comau2_answer,
                        "Compare AU 2 (Incorrect incl)",
                        normalize = TRUE)

p2b <- get_boxplot_ans(comau2, comau2_NA_indices, comau2_NA_count, comau2_answer,
                       "Compare AU 2 (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p2bn <- get_boxplot_ans(comau2, comau2_NA_indices, comau2_NA_count, comau2_answer,
                        "Compare AU 2  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t2a <- get_boxplot_time(comau2_time, comau2_NA_indices)

# 3 -------------------------------
# Treatment order: 4, 1, 2, 3 
comau3_answer <- 900000

comau3 <- bind_rows(gp4[, c("ComAU3a", "ComAU3b_1", "participant_id")], 
                    gp1[, c("ComAU3a", "ComAU3b_1", "participant_id")],
                    gp2[, c("ComAU3a", "ComAU3b_1", "participant_id")], 
                    gp3[, c("ComAU3a", "ComAU3b_1", "participant_id")],
                    .id="Treatment") %>%
  mutate("Clean" = get_double_column(ComAU3b_1),
         "Flipped" = Clean,
         answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

comau3_incorrect_count <- count_incorrect(comau3$ComAU3a, "larger")
comau3_NA_count <- count_NA(comau3$Clean)

comau3_incorrect_indices <- get_incorrect_indices(comau3$ComAU3a, "larger")
comau3_NA_indices <- get_NA_indices(comau3$Clean)

comau3$Flipped[comau3_incorrect_indices] <- 
  comau3$Flipped[comau3_incorrect_indices] * -1

# Response time
comau3_time <- bind_rows(gp4[,c("ComAU3aTi_Page Submit","ComAU3bTi_Page Submit")],
                         gp1[,c("ComAU3aTi_Page Submit","ComAU3bTi_Page Submit")],
                         gp2[,c("ComAU3aTi_Page Submit","ComAU3bTi_Page Submit")],
                         gp3[,c("ComAU3aTi_Page Submit","ComAU3bTi_Page Submit")],
                         .id="Treatment") %>%
  rename("Time1"="ComAU3aTi_Page Submit",
         "Time2"="ComAU3bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p3a <- get_boxplot_ans(comau3, comau3_NA_indices, comau3_NA_count, comau3_answer,
                       "Compare AU 3 (Incorrect incl)",
                       normalize = FALSE)

p3an <- get_boxplot_ans(comau3, comau3_NA_indices, comau3_NA_count, comau3_answer,
                        "Compare AU 3 (Incorrect incl)",
                        normalize = TRUE)

p3b <- get_boxplot_ans(comau3, comau3_NA_indices, comau3_NA_count, comau3_answer,
                       "Compare AU 3 (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p3bn <- get_boxplot_ans(comau3, comau3_NA_indices, comau3_NA_count, comau3_answer,
                        "Compare AU 3  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t3a <- get_boxplot_time(comau3_time, comau3_NA_indices)

# 4 -------------------------------
# Treatment order: 1, 2, 3, 4 
brz_str_handl <- function(s) {
  
  if (!is.na(s)) {
    s <- str_replace_all(s, "\\s|,", "") %>% # Remove whitespace and commas
      str_to_lower()                      # To lower case   
    
    if (s == "na") {
      s <- NA
    }
    
    s %<>% str_extract("\\d+") %>%
      as.double()
    
    return(s)  
    
  }
    return(NA)
}

comau4_answer <- 450

comau4 <- bind_rows(gp1[, c("ComAU4a", "ComAU4b_1", "participant_id")], 
                    gp2[, c("ComAU4a", "ComAU4b_1", "participant_id")],
                    gp3[, c("ComAU4a", "ComAU4b_1", "participant_id")], 
                    gp4[, c("ComAU4a", "ComAU4b_1", "participant_id")],
                    .id="Treatment")

comau4$Clean <- 0

for (i in 1:length(comau4$Clean)) {
  comau4$Clean[i] <- brz_str_handl(comau4$ComAU4b_1[i])
}

comau4_incorrect_count <- count_incorrect(comau4$ComAU4a, "higher")
comau4_NA_count <- count_NA(comau4$Clean)

comau4_incorrect_indices <- get_incorrect_indices(comau4$ComAU4a, "higher")
comau4_NA_indices <- get_NA_indices(comau4$Clean)
comau4_outliers <- c(comau4_NA_indices)  # outlier whose answer is in wrong magnitude

comau4$Flipped <- comau4$Clean

comau4$Flipped[comau4_incorrect_indices] <- 
  comau4$Flipped[comau4_incorrect_indices] * -1

comau4 <- comau4 %>% 
  mutate(answer = is.na(Flipped),
         treatment = rep(treatments, each = 11))

# Response time
comau4_time <- bind_rows(gp1[,c("ComAU4aTi_Page Submit","ComAU4bTi_Page Submit")],
                         gp2[,c("ComAU4aTi_Page Submit","ComAU4bTi_Page Submit")],
                         gp3[,c("ComAU4aTi_Page Submit","ComAU4bTi_Page Submit")],
                         gp4[,c("ComAU4aTi_Page Submit","ComAU4bTi_Page Submit")],
                         .id="Treatment") %>%
  rename("Time1"="ComAU4aTi_Page Submit",
         "Time2"="ComAU4bTi_Page Submit") %>%
  mutate(Time=as.double(Time1)+as.double(Time2))

# Plots
p4a <- get_boxplot_ans(comau4, comau4_outliers, comau4_NA_count, comau4_answer,
                       "Compare AU 4 (Incorrect incl)",
                       normalize = FALSE)

p4an <- get_boxplot_ans(comau4, comau4_outliers, comau4_NA_count, comau4_answer,
                        "Compare AU 4 (Incorrect incl)",
                        normalize = TRUE)

p4b <- get_boxplot_ans(comau4, comau4_outliers, comau4_NA_count, comau4_answer,
                       "Compare AU 4 (Incorrect Flipped)",
                       normalize = FALSE, "Flipped")

p4bn <- get_boxplot_ans(comau4, comau4_outliers, comau4_NA_count, comau4_answer,
                        "Compare AU 4  (Incorrect Flipped)",
                        normalize = TRUE, "Flipped")

t4a <- get_boxplot_time(comau4_time, comau4_NA_indices)

###################################
#####   Response Aggregated   #####
###################################

comau_all <- get_aggre_df(comau1, comau2, comau3, comau4,
                          comau1_answer, comau2_answer,
                          comau3_answer, comau4_answer,
                          comau1_NA_indices, comau2_NA_indices,
                          comau3_NA_indices, comau4_outliers,
                          "Flipped")

kw_res <- get_kruskal_res(comau_all) # p.value = 0.0005237
pairwise.wilcox.test(comau_all$Normal,
                     factor(comau_all$Treatment),
                     p.adjust.method = "holm",
                     paired = FALSE)  # How to do pairwise when there are repeated values

pall_title <- chi2_and_main_p(kw_res)

pall <- get_aggre_plot(comau_all, pall_title)

pall <- pall + scale_y_continuous(limits = c(-3, 3))

comau_response_pairwise <- pairwise_wilcox_test(comau_all,
                                                Normal ~ Treatment,
                                                p.adjust.method = "holm",
                                                paired = FALSE,
                                                detailed = TRUE)

pall <- pall + stat_pvalue_manual(comau_response_pairwise,
                                  label = "p.adj.signif",
                                  hide.ns = TRUE,
                                  y.position = c(2.4, 2.7),
                                  tip.length = 0.01)

###################################
#####   Time Aggregated      ######
###################################

comau_all_time <- bind_rows(comau1_time[-comau1_NA_indices, ], 
                            comau2_time[-comau2_NA_indices, ],
                            comau3_time[-comau3_NA_indices, ], 
                            comau4_time[-comau4_outliers, ])

comau_all_time$Treatment <- plyr::mapvalues(comau_all_time$Treatment,
                                            from = c("1", "2", "3", "4"),
                                            to = c("None", "StLO", "StLG", "SeLG")) %>%
  factor(levels = c("None", "StLO", "StLG", "SeLG"))

# Statistical Analysis
kw_time <- get_kruskal_time(comau_all_time) # p.value = 0.004511
pairwise.wilcox.test(comau_all_time$Time,
                     factor(comau_all_time$Treatment),
                     p.adjust.method = "holm",
                     paired = FALSE)

tall_title <- chi2_and_main_p(kw_time)

tall <- get_aggre_time(comau1_time, comau2_time,
                       comau3_time, comau4_time,
                       comau1_NA_indices, comau2_NA_indices,
                       comau3_NA_indices, comau4_outliers,
                       tall_title)

# Add significant p-values
comau_time_pairwise <- pairwise_wilcox_test(comau_all_time,
                                            Time ~ Treatment,
                                            p.adjust.method = "holm",
                                            paired = FALSE,
                                            detailed = TRUE)

tall <- tall + stat_pvalue_manual(comau_time_pairwise,
                                  label = "p.adj.signif",
                                  hide.ns = TRUE,
                                  y.position = c(230, 250, 270))

###################################
######      NA Aggregated     #####
###################################
comau_NA_count <- comau1_NA_count + comau2_NA_count + comau3_NA_count + comau4_NA_count

comau_NA_all <- bind_rows(comau1[, c("treatment", "participant_id", "answer")], 
                          comau2[, c("treatment", "participant_id", "answer")], 
                          comau3[, c("treatment", "participant_id", "answer")], 
                          comau4[, c("treatment", "participant_id", "answer")])

comau_NA_all$answer <- factor(as.character(comau_NA_all$answer))
comau_NA_all$treatment <- factor(comau_NA_all$treatment, levels = treatments)

cqtest <- cochran_qtest(comau_NA_all, answer ~ treatment | participant_id)
cqtest$p.value <- cqtest$p

pbar_title <- chi2_and_main_p(cqtest)

pbar <- get_NA_barplot(comau_NA_count, pbar_title)

# Plot significant p-values for NA barplot

comau_pairwise_mcnemar <- pairwise_mcnemar_test(comau_NA_all, 
                                               answer ~ treatment | participant_id,
                                               p.adjust.method = "holm") %>%
  filter(!is.nan(p))

pbar <- pbar + stat_pvalue_manual(comau_pairwise_mcnemar,
                                  label = "p.adj.signif",
                                  hide.ns = TRUE,
                                  y.position = c(77, 82, 88, 93, 99))

# Confidence Interval for NA pairwise results

# Function to get pairwise CIs for NA analysis
na_pairwise_effect_ci <- function(NA_all, two_treatments) {
  
  sbset <- NA_all[NA_all$treatment %in% two_treatments, ]
  sbset$answer <- as.logical(sbset$answer)
  sbset <- pivot_wider(sbset, names_from = "treatment",
                       values_from = "answer")
  
  tble = matrix(0, 2, 2)
  tble[1, 1] <- sum(!sbset[,two_treatments[1]] & !sbset[,two_treatments[2]])
  tble[1, 2] <- sum(!sbset[,two_treatments[1]] & sbset[,two_treatments[2]])
  tble[2, 1] <- sum(sbset[,two_treatments[1]] & !sbset[,two_treatments[2]])
  tble[2, 2] <- sum(sbset[,two_treatments[1]] & sbset[,two_treatments[2]])
  
  mcnemar.exact(tble)
  
}

na_pairwise_effect_ci(comau_NA_all, c("None", "StLO"))
na_pairwise_effect_ci(comau_NA_all, c("None", "StLG"))
na_pairwise_effect_ci(comau_NA_all, c("None", "SeLG"))
na_pairwise_effect_ci(comau_NA_all, c("StLO", "StLG"))
na_pairwise_effect_ci(comau_NA_all, c("StLO", "SeLG"))

###################################
######      Combine plots     #####
###################################

title <-
  ggdraw() +
  draw_label("Compare Administrative Units",
             fontface = "bold.italic",
             size = 15) +
  theme(plot.background = element_rect(fill = "#e6e6e6", color = NA)) 

bottom_row <- plot_grid(pall, tall, pbar, ncol = 3, rel_heights = c(3/10, 4/10, 4/10))
pcombined <- plot_grid(title,
                       bottom_row,
                       nrow = 2,
                       rel_heights = c(0.12, 1))

#saveRDS(pcombined, file = "../rdata/Combined_ComAU.rds")
#ggsave("Combined_ComAU.pdf", pcombined, path = "../plots/", width = 6, height = 4)

###################################
# Summary Data
###################################
summary(comau_all$Normal, na.rm=TRUE)

comau_all %>%
  mutate(id = rep(1:44, 4)) %>%
  pivot_wider(names_from = Treatment, values_from = Normal) %>%
  summary

sum(comau_NA_count / 176) * 100

comau_NA_all %>%
  group_by(treatment) %>%
  summarise(propor = sum(answer == TRUE)/44 * 100)

summary(comau_all_time)
  
for (i in treatments) {
  print(c("Treatment", i))
  print(summary(comau_all_time[comau_all_time$Treatment == i, "Time"]))
}
