library(jsonlite)
library(tidyverse)
library(lubridate)

source("ABT_add_last_activity.R")
source("ABT_add_post_popularity.R")
source("ABT_add_comment_popularity.R")

path <- "G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/"

data_in <- readRDS(paste0(path, "collected/items_225_245_v2.RDS"))
ABT <- read.csv(paste0(path, "transformed/ABT_base.csv"))

ABT <- ABT %>% select(-c(comment_number, story_number, activity_number))

ABTs <- list()
for(ym in c("2020-07-01", "2020-08-01")){
  ABTs[[ym]] <- add_last_activity(ABT, data_in, ym)
  ABTs[[ym]] <- add_post_popularity(ABTs[[ym]], data_in, ym)
  ABTs[[ym]] <- add_comment_popularity(ABTs[[ym]], data_in, ym)
}

write_rds(ABTs, paste0(path, "transformed/final_ABTs.RDS"))
