
library("cowplot")
library(tidyverse)

# Script to combine plots for report
estau <- readRDS("../rdata/Combined_EstAU.rds")
comau <- readRDS("../rdata/Combined_ComAU.rds")
comzo <- readRDS("../rdata/Combined_ComZo.rds")
dcau <- readRDS("../rdata/Combined_DCAU.rds")
dczo <- readRDS("../rdata/Combined_DCZo.rds")

ftau <- readRDS("../rdata/Combined_FTAU.rds")
cluau <- readRDS("../rdata/Combined_CluAU.rds")

quant_plotA <- plot_grid(estau, comau, comzo, dcau,
                        ncol = 1)

quant_plotB <- plot_grid(dczo,
                         ncol = 1)

ftau_cluau <- plot_grid(ftau, NULL, cluau,
                        rel_widths = c(15/31, 1/31, 15/31),
                        ncol = 3)


ggsave("Combined_Quant_A.pdf", quant_plotA, path = "../plots/", width = 20, height = 34, units = "cm")
ggsave("Combined_Quant_B.pdf", quant_plotB, path = "../plots/", width = 20, height = 8.5, units = "cm")
ggsave("Combined_FT_Clu.pdf", ftau_cluau, path = "../plots/", width = 28, height = 8, units = "cm")

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
