library(tidyverse)

# Get vector of file names
filenames <- list.files("../map-data")

# Load all files
for(i in filenames){
  path <- file.path("..", "map-data", i)
  name <- substr(i, 1, nchar(i) - 4)
  df <- read_csv(path) %>% select(-Colour) %>% select(-1)
  assign(name, df)
}

remove(path, name, df, i, filenames)

# Put all dataframes in a vector
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

# Summary statistics of administrative units
num_admin_unit <- c()
num_cols <- c()

for (df in dfs){
  num_admin_unit <- c(num_admin_unit, nrow(df))
  num_cols <- c(num_cols, ncol(df))
}

summary(num_admin_unit)   # Statistics of number of administrative units for 28 maps
sd(num_admin_unit)        # Mean: 18.4, Median: 16.00, SD: 10.4

# Initialise dataframe to store data summary statistics
number_datasets = sum(num_cols)

summary_stats <-tibble("mean" = numeric(n),
                       "median" = numeric(number_datasets), 
                       "sd" = numeric(number_datasets))