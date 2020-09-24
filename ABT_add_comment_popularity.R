library(jsonlite)
library(tidyverse)
library(lubridate)

# path <- "G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/"

# data_in <- readRDS(paste0(path, "collected/items_225_245_v2.RDS"))
# ABT_in <- read.csv(paste0(path, "transformed/ABT_base.csv"))

# abt_date <- "2020-07-01"

add_comment_popularity <- function(ABT_in, data_in, abt_date){
  ABT <- ABT_in %>% filter(year_month == abt_date)
  
  data <- data_in %>%
    mutate(time = as.Date(time)) %>%
    filter(time < as.Date(abt_date),
           time > as.Date(abt_date) - as.difftime(183, unit="days"),
           by %in% ABT$by, type == "comment") %>%
    select(by, descendants, score, time) %>%
    replace_na(list("score" = 0, "descendants" = 0))
  
  last_comment <- data %>%
    group_by(by) %>%
    mutate(max_comment = max(time)) %>%
    ungroup() %>%
    filter(max_comment == time) %>%
    group_by(by) %>%
    summarise(last_comment_score = max(score),
              last_comment_desc = max(descendants),
              days_from_last_comment = as.Date(abt_date) - max(max_comment)) %>%
    ungroup()
  
  ABT <- ABT %>%
    left_join(last_comment %>% select(by, last_comment_score, last_comment_desc, days_from_last_comment)) %>%
    replace_na(list("last_comment_score" = 0, "last_comment_desc" = 0, "days_from_last_comment" = 183))
  
  comment_30 <- data %>%
    filter(as.Date(abt_date) - time < 30) %>%
    group_by(by) %>%
    summarise(max_comment_score_30 = max(score),
              max_comment_desc_30 = max(descendants),
              mean_comment_score_30 = mean(score),
              mean_comment_desc_30 = mean(descendants),
              comment_count_30 = n()) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  comment_60 <- data %>%
    filter(as.Date(abt_date) - time < 60) %>%
    group_by(by) %>%
    summarise(max_comment_score_60 = max(score),
              max_comment_desc_60 = max(descendants),
              mean_comment_score_60 = mean(score),
              mean_comment_desc_60 = mean(descendants),
              comment_count_60 = n()) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  comment_183 <- data %>%
    group_by(by) %>%
    summarise(max_comment_score_183 = max(score),
              max_comment_desc_183 = max(descendants),
              mean_comment_score_183 = mean(score),
              mean_comment_desc_183 = mean(descendants),
              comment_count_183 = n()) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  ABT <- ABT %>%
    left_join(comment_30) %>%
    left_join(comment_60) %>%
    left_join(comment_183) %>%
    replace(is.na(.), 0)
  
  return(ABT)
}
