
library("cowplot")
library(tidyverse)

# Script to combine plots for report
estau <- readRDS("RData/Combined_EstAU.rds")
comau <- readRDS("RData/Combined_ComAU.rds")
comzo <- readRDS("RData/Combined_ComZo.rds")
dcau <- readRDS("RData/Combined_DCAU.rds")
dczo <- readRDS("RData/Combined_DCZo.rds")

ftau <- readRDS("RData/Combined_FTAU.rds")
cluau <- readRDS("RData/Combined_CluAU.rds")

quant_plotA <- plot_grid(estau, comau, comzo,
                        ncol = 1)

quant_plotB <- plot_grid(dcau, dczo,
                         ncol = 1)

ftau_cluau <- plot_grid(ftau, cluau,
                        ncol = 1)


# ggsave("Combined_Quant_A.pdf", quant_plotA, path = "Plots/", width = 16, height = 24, units = "cm")
# ggsave("Combined_Quant_B.pdf", quant_plotB, path = "Plots/", width = 16, height = 16, units = "cm")
# ggsave("Combined_FT_Clu.pdf", ftau_cluau, path = "Plots/", width = 16, height = 16, units = "cm")

# Script for demographic summaries

bg <- c("BG1", "BG2")

# Read in data
gp1 <- read_csv("Group 1.csv") %>%
  slice(5:n()) %>%
  select(bg)

gp2 <- read_csv("Group 2.csv") %>%
  slice(3:n()) %>%
  select(bg)

gp3 <- read_csv("Group 3.csv") %>%
  slice(3:n()) %>%
  select(bg)

gp4 <- read_csv("Group 4.csv") %>%
  slice(3:n()) %>%
  select(bg)

bg_all <- bind_rows(gp1, gp2, gp3, gp4)

mean_age <- mean(as.integer(bg_all$BG1))
summary(as.integer(bg_all$BG1))
table(bg_all$BG2)
