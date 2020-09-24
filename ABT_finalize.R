library(jsonlite)
library(tidyverse)
library(lubridate)

source("ABT_add_last_activity.R")
source("ABT_add_post_popularity.R")
source("ABT_add_comment_popularity.R")
source("ABT_add_hist_activity.R")
source("ABT_add_comment_NLP.R")

path <- "G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/"

data_in <- read.csv(paste0(path, "transformed/data_full.csv"))  # , encoding="iso-8859-2")
ABT <- read.csv(paste0(path, "transformed/ABT_base_full.csv"))
sent_in <- readRDS(paste0(path, "transformed/comment_month_user_agg.rds"))

data <- data_in %>% filter(time >= as.Date("2019-08-01"))

ABT <- ABT %>%
  select(-c(comment_number, story_number, activity_number)) %>%
  filter(is_active == 1)

ABTs <- list()
for(ym in c("2020-06-01", "2020-07-01")){
  ABTs[[ym]] <- add_last_activity(ABT, data, ym)
  ABTs[[ym]] <- add_post_popularity(ABTs[[ym]], data, ym)
  ABTs[[ym]] <- add_comment_popularity(ABTs[[ym]], data, ym)
  ABTs[[ym]] <- add_hist_activity(ABTs[[ym]], data, ym)
  ABTs[[ym]] <- add_comment_NLP(ABTs[[ym]], sent_in, ym)
}

saveRDS(ABTs, paste0(path, "transformed/final_ABTs_2.RDS"))
