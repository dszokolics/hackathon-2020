library(jsonlite)
library(tidyverse)
library(lubridate)

# path <- "G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/"

# data_in <- readRDS(paste0(path, "collected/items_225_245_v2.RDS"))
# ABT_in <- read.csv(paste0(path, "transformed/ABT_base.csv"))

# abt_date <- "2020-07-01"

add_post_popularity <- function(ABT_in, data_in, abt_date){
  ABT <- ABT_in %>% filter(year_month == abt_date)
  
  data <- data_in %>%
    mutate(time = as.Date(time)) %>%
    filter(time < as.Date(abt_date),
           time > as.Date(abt_date) - as.difftime(183, unit="days"),
           by %in% ABT$by, type == "story") %>%
    select(by, descendants, score, time) %>%
    replace_na(list("score" = 0, "descendants" = 0))
  
  print(dim(data))
  
  last_post <- data %>%
    group_by(by) %>%
    mutate(max_post = max(time)) %>%
    ungroup() %>%
    filter(max_post == time) %>%
    group_by(by) %>%
    summarise(last_post_score = max(score),
              last_post_desc = max(descendants),
              days_from_last_post = as.Date(abt_date) - max(max_post)) %>%
    ungroup()
  
  ABT <- ABT %>%
    left_join(last_post %>% select(by, last_post_score, last_post_desc, days_from_last_post)) %>%
    replace_na(list("last_post_score" = 0, "last_post_desc" = 0, "days_from_last_post" = 183))
  
  popularity_30 <- data %>%
    filter(as.Date(abt_date) - time < 30) %>%
    group_by(by) %>%
    summarise(max_post_score_30 = max(score),
              max_post_desc_30 = max(descendants),
              mean_post_score_30 = mean(score),
              mean_post_desc_30 = mean(descendants),
              post_count_30 = n()) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  popularity_60 <- data %>%
    filter(as.Date(abt_date) - time < 60) %>%
    group_by(by) %>%
    summarise(max_post_score_60 = max(score),
              max_post_desc_60 = max(descendants),
              mean_post_score_60 = mean(score),
              mean_post_desc_60 = mean(descendants),
              post_count_60 = n()) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  popularity_183 <- data %>%
    group_by(by) %>%
    summarise(max_post_score_183 = max(score),
              max_post_desc_183 = max(descendants),
              mean_post_score_183 = mean(score),
              mean_post_desc_183 = mean(descendants),
              post_count_183 = n()) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  ABT <- ABT %>%
    left_join(popularity_30) %>%
    left_join(popularity_60) %>%
    left_join(popularity_183) %>%
    replace(is.na(.), 0)

  return(ABT)
}
