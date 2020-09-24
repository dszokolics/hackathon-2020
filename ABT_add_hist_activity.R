library(jsonlite)
library(tidyverse)
library(lubridate)

# path <- "G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/"

# data_in <- readRDS(paste0(path, "collected/items_225_245_v2.RDS"))
# ABT_in <- read.csv(paste0(path, "transformed/ABT_base.csv"))

# abt_date <- "2020-07-01"

add_hist_activity <- function(ABT_in, data_in, abt_date){
  ABT <- ABT_in %>% filter(year_month == abt_date)
  
  data <- data_in %>%
    mutate(time = as.Date(time)) %>%
    filter(time < as.Date(abt_date),
           time > as.Date(abt_date) - as.difftime(183, unit="days"),
           by %in% ABT$by) %>%
    select(by, descendants, score, time) %>%
    replace_na(list("score" = 0, "descendants" = 0))
  
  activity_30_1 <- data %>%
    filter(as.Date(abt_date) - time < 30) %>%
    group_by(by) %>%
    summarise(max_activity_score_30 = max(score),
              max_activity_desc_30 = max(descendants),
              mean_activity_score_30 = mean(score),
              mean_activity_desc_30 = mean(descendants),
              activity_count_30 = n()) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  activity_30_2 <- data %>%
    filter(as.Date(abt_date) - time < 60,
           as.Date(abt_date) - time >= 30) %>%
    group_by(by) %>%
    summarise(max_activity_score_30_2 = max(score),
              max_activity_desc_30_2 = max(descendants),
              mean_activity_score_30_2 = mean(score),
              mean_activity_desc_30_2 = mean(descendants),
              activity_count_30_2 = n()) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  activity_60 <- data %>%
    filter(as.Date(abt_date) - time < 60) %>%
    group_by(by) %>%
    summarise(max_activity_score_60 = max(score),
              max_activity_desc_60 = max(descendants),
              mean_activity_score_60 = mean(score),
              mean_activity_desc_60 = mean(descendants),
              activity_count_60 = n()) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  activity_60_2 <- data %>%
    filter(as.Date(abt_date) - time < 120,
           as.Date(abt_date) - time >= 60) %>%
    group_by(by) %>%
    summarise(max_activity_score_60_2 = max(score),
              max_activity_desc_60_2 = max(descendants),
              mean_activity_score_60_2 = mean(score),
              mean_activity_desc_60_2 = mean(descendants),
              activity_count_60_2 = n()) %>%
    ungroup() %>%
    replace(is.na(.), 0)
  
  activity_trend_30 <- activity_30_1 %>%
    left_join(activity_30_2) %>%
    replace(is.na(.), 0) %>%
    mutate(max_activity_score_t_30 = max_activity_score_30 - max_activity_score_30_2,
           max_activity_desc_t_30 = max_activity_desc_30 - max_activity_desc_30_2,
           mean_activity_score_t_30 = mean_activity_score_30 - mean_activity_score_30_2,
           mean_activity_desc_t_30 = mean_activity_desc_30 - mean_activity_desc_30_2,
           activity_trend_30 = activity_count_30 - activity_count_30_2)
  
  activity_trend_60 <- activity_60 %>%
    left_join(activity_60_2) %>%
    replace(is.na(.), 0) %>%
    mutate(max_activity_score_t_60 = max_activity_score_60 - max_activity_score_60_2,
           max_activity_desc_t_60 = max_activity_desc_60 - max_activity_desc_60_2,
           mean_activity_score_t_60 = mean_activity_score_60 - mean_activity_score_60_2,
           mean_activity_desc_t_60 = mean_activity_desc_60 - mean_activity_desc_60_2,
           activity_trend_60 = activity_count_60 - activity_count_60_2)
  
  ABT <- ABT %>%
    left_join(activity_trend_30) %>%
    left_join(activity_trend_60) %>%
    replace(is.na(.), 0)
  
  return(ABT)
}
