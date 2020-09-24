library(jsonlite)
library(tidyverse)
library(lubridate)


#path <- "G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/"

#sent_in <- readRDS(paste0(path, "transformed/comment_month_user_agg.rds"))
#ABT_in <- read.csv(paste0(path, "transformed/ABT_base.csv"))

#abt_date <- "2020-08-01"

add_comment_NLP <- function(ABT_in, sent_in, abt_date){
  sent <- sent_in %>%
    filter(time < as.Date(abt_date),
           time > as.Date(abt_date) - as.difftime(183, unit="days"))
  
  ABT <- ABT_in %>%
    left_join(sent %>% filter(time > as.Date(abt_date) - as.difftime(35, unit="days"))) %>%
    left_join(sent %>%
                filter(time > as.Date(abt_date) - as.difftime(70, unit="days")) %>%
                group_by(by) %>%
                summarise(sum_qout_2 = sum(sum_qout),
                          sum_exclam_2 = sum(sum_exclam),
                          sum_link_2 = sum(sum_link),
                          cnt_comment_2 = sum(cnt_comment),
                          avg_token_len_2 = mean(avg_token_len))) %>%
    left_join(sent %>%
                filter(time > as.Date(abt_date) - as.difftime(200, unit="days")) %>%
                group_by(by) %>%
                summarise(sum_qout_6 = sum(sum_qout),
                          sum_exclam_6 = sum(sum_exclam),
                          sum_link_6 = sum(sum_link),
                          cnt_comment_6 = sum(cnt_comment),
                          avg_token_len_6 = mean(avg_token_len)))
  
  return(ABT)
}