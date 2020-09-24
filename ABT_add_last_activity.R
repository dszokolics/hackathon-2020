library(jsonlite)
library(tidyverse)
library(lubridate)


# abt_date <- "2020-08-01"
# path <- "G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/"

# data_in <- readRDS(paste0(path, "collected/items_225_245_v2.RDS"))
# ABT_in <- read.csv(paste0(path, "transformed/ABT_base.csv"))

add_last_activity <- function(ABT_in, data_in, abt_date) {
  ABT <- ABT_in %>% filter(year_month == abt_date)
  
  data <- data_in %>%
    mutate(time = as.Date(time)) %>%
    filter(time < abt_date, by %in% ABT$by) %>%
    group_by(by) %>%
    summarise(last_activity = max(time)) %>%
    ungroup()
  
  ABT <- ABT %>%
    left_join(data) %>%
    mutate(days_from_last_activity = as.Date(year_month)-last_activity)
  
  return(ABT)
}
