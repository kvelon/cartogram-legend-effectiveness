library(tidyverse)

# Function that takes in string and returns number
string_to_double <- function(s) {
  
  if (!is.na(s)) {
    
    s <- str_replace_all(s, "\\s|,", "") %>% # Remove whitespace and commas
      str_to_lower()                      # To lower case   
    
    if (s == "na") {
      s <- NA
    }
    
    else if (str_detect(s, "mil+ion|mil")) {  # Replace million
      s <- str_replace_all(s, "mil+ion|mil", "") %>%
        as.double() %>%
        "*"(1e6)
    }
    
    else if (str_detect(s, "bil+ion")) {  # Replace billion
      s <- str_replace_all(s, "bil+ion", "") %>%
        as.double() %>%
        "*"(1e9)
    }
    
    s <- as.double(s)
  }
  
  return(s)
  
}

# Function that takes in string vector and returns numeric vector
get_double_column <- function(c, fn=string_to_double) {
  
  c_new <- double(length(c))
  
  for (i in 1:length(c)) {
    c_new[i] <- fn(c[i])
  }
  
  return(as.double(c_new))
}

# Function that counts number of NA responses in each treatment
count_NA <- function(v) {

  count <- integer(4)  
  
  for (i in 1:4) {
    idx = 1 + (i-1)*11
    count[i] <- sum(is.na(v[idx:(idx+10)]))
  }
  
  return(count)
}

# Function that counts the number of incorrect responses in each treatment
count_incorrect <- function(v, answer) {
  
  c <- integer(4)
  
  for (i in 1:4) {
    idx = 1 + (i-1)*11
    c[i] <- sum(v[idx:(idx+10)] != answer)
  }
  
  return(c)
}

# Function that returns the indices of NA values in vector
get_NA_indices <- function(v) {
  return(which(is.na(v)))  
}

# Function that returns the indices of Incorrect responses
get_incorrect_indices <- function(v, answer) {
  return(which(v != answer))
}

# Function that flips sign of Incorrect responses
flip_incorrect <- function(v, incorrect_indices) {
  
  v[incorrect_indices] <- v[incorrect_indices] * -1
  
}

# Function that returns ggplot2 boxplot for answers
get_boxplot_ans <- function(df, outliers, legend_count, answer, 
                            plot_title, normalize=FALSE,
                            column="Clean") {
  
  df[outliers, column] <- NA
  
  if (normalize) {
    df[, column] <- (df[, column] - answer) / answer
    # print(summary(df[, column], na.rm=TRUE))
  }
  
  p <- ggplot(df, aes_string(x="Treatment", y=column, fill="Treatment")) +
    geom_boxplot() +
    ggtitle(plot_title) +
    scale_x_discrete(labels=c("1"="None", "2"="StLO",
                              "3"="StLG",
                              "4"="SeLG")) +
    theme(axis.title.y = element_blank()) +
    scale_fill_discrete(name="NAs", labels=legend_count)
  
  if (!normalize) {
    p <- p + geom_hline(yintercept = answer, linetype="dashed", color="red")
  }
  
  return(p)
  
}

# Function that returns ggplot2 boxplot for response time
get_boxplot_time <- function(df, outliers = c(), 
                             plot_title="Response Time (Seconds)") {
  
  df$Time[outliers] <- NA
  
  p <- ggplot(df, aes(x=Treatment, y=Time, fill=Treatment)) +
    geom_boxplot() +
    ggtitle(plot_title) +
    scale_x_discrete(labels=c("1"="None", "2"="LO",
                              "3"="LG",
                              "4"="SLG")) +
    theme(axis.title.y = element_blank(),
          legend.position = "none")
  
  return(p)
  
}

# Function that returns aggregated dataframe
get_aggre_df <- function(df1, df2, df3, df4,
                         a1, a2, a3, a4,
                         o1, o2, o3, o4,
                         column = "Clean") {
  
  df1[o1, column] <- NA
  df2[o2, column] <- NA
  df3[o3, column] <- NA
  df4[o4, column] <- NA
  
  df1[, "Normal"] <- (df1[, column] - a1) / a1
  df2[, "Normal"] <- (df2[, column] - a2) / a2
  df3[, "Normal"] <- (df3[, column] - a3) / a3
  df4[, "Normal"] <- (df4[, column] - a4) / a4

  
  df <- bind_rows(df1[, c("Treatment", "Normal")],
                  df2[, c("Treatment", "Normal")],
                  df3[, c("Treatment", "Normal")],
                  df4[, c("Treatment", "Normal")]) %>%
    arrange(Treatment)
  
  TreatmentNames <- plyr::mapvalues(df$Treatment,
                                    from = c("1", "2", "3", "4"),
                                    to = c("None", "StLO", "StLG", "SeLG")) %>%
    factor(levels = c("None", "StLO", "StLG", "SeLG"))
    
  df <- df %>% mutate(Treatment = TreatmentNames)
  
  return(df)
}

# Function that returns aggregated answer plot
get_aggre_plot <- function(df,  plot_title) {

  p <- ggplot(df, aes(x=Treatment, y=Normal)) +
    # geom_violin(colour = NA,
    #             fill = "#93a1a1",
    #             scale = "width") +
    geom_boxplot(width = 0.6) +
    scale_x_discrete(labels=c("1"="None", "2"="StLO",
                              "3"="StLG",
                              "4"="SeLG")) +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          plot.title = element_text(size=10)) +
    theme(legend.position = "none") +
    ggtitle("Normalized Responses:",
            plot_title)
  
  return(p)
}


# Function that returns aggregated response time plot
get_aggre_time <- function(t1, t2, t3, t4,
                           o1, o2, o3, o4,
                           plot_title) {
  t1$Time[o1] <- NA
  t2$Time[o2] <- NA
  t3$Time[o3] <- NA
  t4$Time[o4] <- NA

  df <- bind_rows(t1, t2, t3, t4)
  
  TreatmentNames <- plyr::mapvalues(df$Treatment,
                                    from = c("1", "2", "3", "4"),
                                    to = c("None", "StLO", "StLG", "SeLG")) %>%
    factor(levels = c("None", "StLO", "StLG", "SeLG"))
  
  df <- df %>% mutate(Treatment = TreatmentNames)

  p <- ggplot(df, aes(x=Treatment, y=Time)) +
    # geom_violin(colour = NA,
    #             fill = "#93a1a1",
    #             scale = "width") +
    geom_boxplot(width = 0.6) +
    ggtitle("Response Time:",
            plot_title) +
    scale_x_discrete(labels=c("1"="None", "2"="StLO",
                              "3"="StLG",
                              "4"="SeLG")) +
    theme_bw() +
    labs(y = "Time (Seconds)") +
    theme(legend.position = "none",
          plot.title = element_text(size=10)) +
    ylim(0, 300)
  
  return(p)
}

# Function that returns grouped boxplot of responses
get_grouped_ans <- function(df1, df2, df3, df4,
                          a1, a2, a3, a4,
                          o1, o2, o3, o4,
                          c1, c2, c3, c4,
                          plot_title, column="Clean") {
  
  df1[o1, column] <- NA
  df2[o2, column] <- NA
  df3[o3, column] <- NA
  df4[o4, column] <- NA

  df1[, "Normal"] <- (df1[, column] - a1) / a1
  df2[, "Normal"] <- (df2[, column] - a2) / a2
  df3[, "Normal"] <- (df3[, column] - a3) / a3
  df4[, "Normal"] <- (df4[, column] - a4) / a4
  
  df <- bind_rows(df1[, c("Treatment", "Normal")],
                  df2[, c("Treatment", "Normal")],
                  df3[, c("Treatment", "Normal")],
                  df4[, c("Treatment", "Normal")],
                  .id="Question") %>%
    arrange(Treatment)

  c_all <- c1 + c2 + c3 + c4
  
  p <- ggplot(df, aes(x=Treatment, y=Normal, fill=Question)) +
    geom_boxplot() +
    ggtitle(plot_title) +
    scale_x_discrete(labels=c("1"="None", "2"="StLO",
                              "3"="StLG",
                              "4"="SeLG")) +
    theme(axis.title.y = element_blank()) +
    scale_fill_discrete(name="Qn", labels=c(1, 2, 3, 4))
  
  return(list(df, p))
}

# Function that returns dataframe with aggregated response time
get_grouped_time <- function(t1, t2, t3, t4,
                             o1, o2, o3, o4,
                             plot_title) {
  t1$Time[o1] <- NA
  t2$Time[o2] <- NA
  t3$Time[o3] <- NA
  t4$Time[o4] <- NA
  
  df <- bind_rows(t1, t2, t3, t4,
                  .id="Question")
  
  p <- ggplot(df, aes(x=Treatment, y=Time, fill=Question)) +
    geom_boxplot() +
    ggtitle(plot_title) +
    scale_x_discrete(labels=c("1"="None", "2"="StLO",
                              "3"="StLG",
                              "4"="SeLG")) +
    theme(axis.title.y = element_blank(),
          legend.position = "none") +
    ylim(0, 300)
  
  return(p)
}

# Function that takes in NA count and returns barplot
get_NA_stacked_barplot <- function(NA_count, graph_title) {
  
  get_NA_datapoints <- function(NA_count) {
    
    res <- rep("Non-NA", 176)
    
    for (i in seq_len(NA_count[1])) {
      res[i] <- "NA"
    }
    
    for (i in seq_len(NA_count[2])) {
      res[44 + i] <- "NA"
    }
    
    for (i in seq_len(NA_count[3])) {
      res[88 + i] <- "NA"
    }
    
    for (i in seq_len(NA_count[4])) {
      res[132 + i] <- "NA"
    }
    
    return(res)
  }
  
  data_points <- get_NA_datapoints(NA_count) 
  
  treatment <- factor(rep(c("None", "Legend", "Grid lines", 
                            "Selectable legend"),each = 44),
                      levels = c("None", "Legend", "Grid lines", 
                                 "Selectable legend"))
  
  df <- data.frame("answer" = data_points,
                   treatment)
  
  p <- 
    ggplot(df, aes(x = treatment, fill = answer)) +
    geom_bar(position = "fill") +
    labs(title = graph_title,
         y = "Proportion",
         x = NULL) +
    scale_x_discrete(labels = c("Selectable legend" = "Selectable\nlegend")) +
    scale_fill_manual(name = NULL,
                      values = c('NA' = '#e41a1c',
                                 'Non-NA' = '#4daf4a'))
  
  return(p)
}

# Function that returns a barplot of percentage of NA reponses
get_NA_barplot <- function(NA_count, graph_title) {
  
  Percentage <- NA_count / 44 * 100
  
  treatment <- factor(rep(c("None", "StLO", "StLG", 
                            "SeLG")),
                      levels = c("None", "StLO", "StLG", 
                                 "SeLG"))
  
  df <- data.frame(treatment, Percentage)
  
  p <- ggplot(df, aes(x = treatment, y = Percentage)) + 
    geom_bar(stat = "identity", fill = "#93a1a1", width = 0.8) +
    ggtitle("No Response:",
            graph_title) +
    labs(x = "Treatment",
         y = "Percentage (%)") +
    theme_bw() +
    theme(plot.title = element_text(size=10)) +
    scale_y_continuous(limits = c(0, 100))

  return(p)
  
}

# Function that takes in participants' answers and returns
# Kruskal-Wallis test results
get_kruskal_res <- function(long_df) {
  
  res <- long_df %>%
    mutate(Treatment = factor(Treatment),
           row = rep(1:44, 4)) %>%
    pivot_wider(names_from = Treatment,
                values_from = Normal) %>%
    select(-row) %>%
    kruskal.test()
  
  return(res)
}

# Function that takes in response time and returns
# Kruskal-Wallis test results
get_kruskal_time <- function(long_df) {
  
  trmt_count <- table(long_df$Treatment)
  
  row <- c()
  
  for (c in trmt_count) {
    row <- c(row, 1:c)
  }
  
  res <- long_df %>%
    select(c(Treatment, Time)) %>%
    arrange(Treatment) %>%
    mutate(Treatment = factor(Treatment),
           row = row) %>%
    pivot_wider(names_from = Treatment,
                values_from = Time) %>%
    select(-row) %>%
    kruskal.test()
  
  return(res)
}

get_wilcox_test <- function(long_df) {
  trmt_count <- table(long_df$Treatment)
  
  row <- c()
  
  for (c in trmt_count) {
    row <- c(row, 1:c)
  }
  
  res <- long_df %>%
    select(c(Treatment, Time)) %>%
    arrange(Treatment) %>%
    mutate(Treatment = factor(Treatment),
           row = row) %>%
    pivot_wider(names_from = Treatment,
                values_from = Time) %>%
    select(-row) %>%
    kruskal.test()
  
  return(res)
}


# Function that takes in incorrect count and returns barplot
get_correct_barplot <- function(incorrect_count, graph_title, total = 11) {
  
  Percentage <- incorrect_count / total * 100
  
  treatment <- factor(rep(c("None", "StLO", "StLG", 
                            "SeLG")),
                      levels = c("None", "StLO", "StLG", 
                                 "SeLG"))

  
  df <- data.frame(treatment, Percentage)
  
  p <- ggplot(df, aes(x = treatment, y = Percentage)) + 
    geom_bar(stat = "identity", fill = "#93a1a1", width = 0.8) +
    ggtitle("Error rate:",
            graph_title) +
    labs(x = "Treatment",
         y = "Incorrect Answers (%)") +
    theme_bw() +
    scale_y_continuous(limits = c(0, 100))
  
  return(p)

}

# Functions to return p-values
get_pairwise_results <- function(NA_count) {
  
  total <- rep(44, 4)
  
  res <- pairwise.prop.test(NA_count, total, p.adjust.method = "holm")
  
  return(res$p.value)
  
}

# Function that takes in vector of NA count and returns chi sq test results
get_chisq <- function(NA_count) {
  
  df <- data.frame("NAs" = NA_count, 
                   "NonNAs" = 44 - NA_count)
  
  Xsq <- chisq.test(df)
  
  return(Xsq)
}

# Function to get plot title
chi2_and_main_p <- function(test) {
  if (test$p.value < 0.001) {
    bquote(chi ^ 2 == phantom(" ") * .(sprintf("%.2f", test$statistic))
           * ", " *  ~ italic(p) < 10 ^ .(ceiling(log(test$p.value, 10))))
  } else if (test$p.value < 0.01) {
    bquote(chi ^ 2 == phantom(" ") * .(sprintf("%.2f", test$statistic))
           * ", " *  ~ italic(p) < 0.01)
  } else {
    bquote(chi ^ 2 == phantom(" ") * .(sprintf("%.2f", test$statistic))
           * ", " *  ~ italic(p) == phantom(" ")
           * .(sprintf("%.2f", test$p.value)))
  }
}
