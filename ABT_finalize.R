library(jsonlite)
library(tidyverse)
library(lubridate)

source("ABT_add_last_activity.R")

path <- "G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/"

data <- readRDS(paste0(path, "collected/items_225_245_v2.RDS"))
ABT <- read.csv(paste0(path, "transformed/ABT_base.csv"))

ABTs <- list()
for(ym in c("2020-07-01", "2020-08-01")){
  ABTs[[ym]] <- add_last_activity(ABT, data, ym)
}

write_rds(ABTs, paste0(path, "transformed/final_ABTs.RDS"))
