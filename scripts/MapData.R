library(tidyverse)

rm(list = ls())

# Get vector of file names
filenames <- list.files("map-data")

# Load all files
for(i in filenames){
  path <- file.path("map-data", i)
  name <- substr(i, 1, nchar(i) - 4)
  df <- read_csv(path) %>% select(-Colour) %>% select(-1)
  assign(name, df)
}
remove(path, name, df, i, filenames)

# Put all dataframes in a vector
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

#####################################################
#### Summary statistics of administrative units #####
#####################################################

map_stats <- data.frame(matrix(nrow=28, ncol=4))
colnames(map_stats) <- c("Country", "Num_AU", "Median_Time", "Mean_Time")

num_admin_unit <- c()
num_cols <- c()

for (i in 1:length(dfs)){
  print(i)
  name <- names(dfs)[i]
  tmp <- str_locate_all(name, "_")[[1]]
  
  print(tmp)
  
  final_underscore <- as.vector(tmp)[length(tmp)]
  name <- str_sub(name, 1, final_underscore-1)
  name <- str_replace(name, "_pop", "")
  map_stats$Country[i] <- name
  map_stats$Num_AU[i] <- nrow(dfs[[i]])
  
  num_admin_unit <- c(num_admin_unit, nrow(dfs[[i]]))
  num_cols <- c(num_cols, ncol(dfs[[i]]))
}

remove(i, name, tmp, final_underscore)

summary(num_admin_unit)   # Statistics of number of administrative units for 28 maps
sd(num_admin_unit)        # Mean: 18.4, Median: 16.00, SD: 10.4
 

######################################################################################
# Linear regression of response time as a function of number of administrative units #
######################################################################################

# Read in data
time_cols = c("EstAU1Ti_Page Submit", "EstAu2Ti_Page Submit",
              "EstAu3Ti_Page Submit", "EstAU4Ti_Page Submit",
              "DCAU1aTi_Page Submit","DCAU1bTi_Page Submit",
              "DCAU2aTi_Page Submit","DCAU2bTi_Page Submit",
              "DCAU3aTi_Page Submit","DCAU3bTi_Page Submit",
              "DCAU4aTi_Page Submit","DCAU4bTi_Page Submit",
              "DCZo1aTi_Page Submit","DCZo1bTi_Page Submit",
              "DCZo2aTi_Page Submit","DCZo2bTi_Page Submit",
              "DCZo3aTi_Page Submit","DCZo3bTi_Page Submit",
              "DCZo4aTi_Page Submit","DCZo4bTi_Page Submit",
              "ComAU1aTi_Page Submit","ComAU1bTi_Page Submit",
              "ComAU2aTi_Page Submit","ComAU2bTi_Page Submit",
              "ComAU3aTi_Page Submit","ComAU3bTi_Page Submit",
              "ComAU4aTi_Page Submit","ComAU4bTi_Page Submit",
              "ComZo1aTi_Page Submit","ComZo1bTi_Page Submit",
              "ComZo2aTi_Page Submit","ComZo2bTi_Page Submit",
              "ComZo3aTi_Page Submit","ComZo3bTu_Page Submit",
              "ComZo4aTi_Page Submit","ComZo4bTi_Page Submit",
              "CluAU1Ti_Page Submit", "CluAU2Ti_Page Submit",
              "CluAU3Ti_Page Submit", "CluAU4Ti_Page Submit",
              "FTAU1Ti_Page Submit", "FTAU2Ti_Page Submit",
              "FTAU3Ti_Page Submit", "FTAU4Ti_Page Submit"
              )


gp1 <- read_csv("data/group1.csv") %>%
  slice(3:n()) %>%
  select(all_of(time_cols)) %>%
  mutate(participant_id = row_number())

gp2 <- read_csv("data/group2.csv") %>%
  slice(3:n()) %>%
  select(all_of(time_cols)) %>%
  mutate(participant_id = row_number() + 11)

gp3 <- read_csv("data/group3.csv") %>%
  slice(3:n()) %>%
  select(all_of(time_cols)) %>%
  mutate(participant_id = row_number() + 22)

gp4 <- read_csv("data/group4.csv") %>%
  slice(3:n()) %>%
  select(all_of(time_cols)) %>%
  mutate(participant_id = row_number() + 33)

all_participants <- rbind(gp1, gp2, gp3, gp4)

zone_datasets = c(new_zealand_pop_1991, czech_gdp,
                  germany_covid ,laos_pop_1985,
                  jamaica_pop_2001, belgium_pop,
                  luxembourg_pop, myanmar_pop_1983)

# NEXT STEPS 
# Horizontally stack the groups, 
# get median and mean of each col,
# match question number to country,
# add median time and mean time to map_to_num_au dataframe
estau_maps <- c("denmark", "ukraine", "ireland", "south_korea")
dcau_maps <- c("india", "qatar", "cambodia", "andorra")
dczo_maps <- c("new_zealand", "laos", "jamaica", "myanmar")
comau_maps <- c("sri_lanka", "poland", "france", "brazil")
comzo_maps <- c("czech", "germany", "belgium", "luxembourg") 
cluau_maps <- c("nepal", "usa", "hungary", "argentina")
ftau_maps <- c("saudi", "kazakhstan", "netherlands", "nigeria")

get_median_mean <- function(col_num) {
  if (length(col_num) == 1)
    vec <- as.numeric(all_participants[[col_num]])
  else
    vec <- c(as.numeric(all_participants[[col_num[1]]]),
             as.numeric(all_participants[[col_num[2]]]))
  return(c(median(vec), mean(vec)))
}

for (i in 1:4) {
  print(i)
  print(colnames(all_participants)[i])
  map_stats[which(map_stats$Country == estau_maps[i]), 3:4] <- 
    get_median_mean(i)
}

for (i in 1:4) {
  print(i)
  print(c(colnames(all_participants)[2*i + 3], colnames(all_participants)[2*i + 4]))

  map_stats[which(map_stats$Country == dcau_maps[i]), 3:4] <- 
    get_median_mean(c(2*i + 3, 2*i + 4))
}

for (i in 1:4) {
  print(i)
  print(c(colnames(all_participants)[2*i + 11], colnames(all_participants)[2*i + 12]))

  map_stats[which(map_stats$Country == dczo_maps[i]), 3:4] <- 
    get_median_mean(c(2*i + 11, 2*i + 12))
}

for (i in 1:4) {
  print(i)
  print(c(colnames(all_participants)[2*i + 19], colnames(all_participants)[2*i + 20]))

  map_stats[which(map_stats$Country == comau_maps[i]), 3:4] <- 
    get_median_mean(c(2*i + 19, 2*i + 20))
}

for (i in 1:4) {
  print(i)
  print(c(colnames(all_participants)[2*i + 27], colnames(all_participants)[2*i + 28]))
  
  map_stats[which(map_stats$Country == comzo_maps[i]), 3:4] <- 
    get_median_mean(c(2*i + 27, 2*i + 28))
}

for (i in 1:4) {
  print(i)
  print(colnames(all_participants)[i + 36])

  map_stats[which(map_stats$Country == cluau_maps[i]), 3:4] <- 
    get_median_mean(c(i + 36))
}

for (i in 1:4) {
  print(i)
  print(colnames(all_participants)[i + 40])

  map_stats[which(map_stats$Country == ftau_maps[i]), 3:4] <- 
    get_median_mean(c(i + 40))
}

for (i in 13:20) {
  map_stats[which(map_stats$Country == dcau_maps[i-12]), 3:4] <- 
    get_median_mean(i)
}

#for (i in 21:)