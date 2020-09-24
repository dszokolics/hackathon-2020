library(jsonlite)
library(tidyverse)
library(lubridate)

path <- "G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/"

rng <- "185_195"
data <- readRDS(paste0(path, "collected/items_", rng, ".RDS")) %>% select(-c(kids, parts))
for(rng in c("195_205", "205_215", "215_225")){
  print(rng)
  data <- data %>%
    bind_rows(readRDS(paste0(path, "collected/items_", rng, ".RDS")) %>%
                select(-c(kids, parts)))
}

# data <- fromJSON(readLines(paste0(path, "collected/items_225_245.JSON")))
d <- fromJSON(readLines(paste0(path, "collected/items_225_245.JSON")))
d <- d %>% select(-c(kids, parts))

data <- bind_rows(data, d)

set.seed(10)
users_sample <- data %>% select(by) %>% unique() %>% sample_n(50000)
sample <- data %>% filter(by %in% users_sample$by)
sample <- data
# sample <- data[sample(nrow(data), 50000), ] 
# write(toJSON(sample), "G:/.shortcut-targets-by-id/0B23Ot7AW9q8dTHJCSU1MT1A2SzQ/Common/HomokozÃ³/nlp-hackathon/data/collected/sample.json")

sample <- sample %>% filter(!is.na(by))

str(sample)
#table(sample$parts)

sample$deleted <- NULL
sample$poll <- NULL 
sample <- sample %>% filter(type %in% c('comment', 'story')) # 6 jobs, 4 pollopts out of 50k datapoints
sample$time <- as.Date(as.POSIXct(sample$time, origin="1970-01-01"))
sample$parts <- NULL
sample$by <- as.factor(sample$by)

### day diff exploration

# user_count <- sample %>% group_by(by, time) %>% 
#   summarize(count = n()) %>% ungroup() %>% 
#   arrange(desc(by), time) %>% group_by(by) %>% 
#   mutate(diff = as.numeric(difftime(time, lag(time), units = 'days'))) 
# 
# user_count %>% filter() %>% 
#   ggplot(aes(diff)) +
#   geom_histogram(binwidth = 1, color = 'darkblue', fill = 'blue') + 
#   scale_x_continuous(breaks = seq(0, 200, 10)) + 
#   theme_bw()
# 
# user_count %>% filter(diff < 15) %>% 
#   ggplot(aes(diff)) +
#   geom_histogram(binwidth = 1, color = 'darkblue', fill = 'blue') + 
#   scale_x_continuous(breaks = seq(0, 15, 1)) + 
#   theme_bw()

### year-month values

sample$year_month <- floor_date(as_date(sample$time), "month")


### ABT

ABT <- sample %>% group_by(by, year_month) %>% 
  summarize(comment_number = sum(type == 'comment'),
            story_number = sum(type == 'story'),
            activity_number = n())

ABT_base <- ABT
ABT <- ABT %>% transform(is_active = activity_number > 0) %>%
  select(by, year_month, is_active) %>%
  pivot_wider(names_from = year_month, values_from = is_active, values_fill = 0) %>%
  pivot_longer(cols = -by, names_to = "year_month") %>%
  mutate(year_month = as.Date(year_month)) %>%
  left_join(ABT_base) %>%
  replace(is.na(.), 0) %>%
  rename(c("is_active" = "value")) %>%
  mutate(active_prev_month = lag(is_active, order_by = year_month)) %>%
  filter(active_prev_month == TRUE) %>%
  mutate(churn = !is_active) %>%
  select(-is_active, -active_prev_month)


write.csv(ABT, file = paste0(path, "transformed/ABT_base_full.csv"))

# write_rds(data, paste0(path, "collected/items_185_245.RDS"))
# write_rds(d, paste0(path, "collected/items_225_245_v2.RDS"))

# users_to_keep <- ABT %>% group_by(by) %>% summarize(count = n()) %>% filter(count >= 4) %>% select(by) %>% as.list()
# ABT <- ABT %>% filter(by %in% users_to_keep$by)


ABT %>% ggplot(aes(year_month, activity_number, color = by)) +
  geom_line(show.legend = F) +
  theme_bw()


ABT %>% group_by(year_month) %>% summarize(total_activity = sum(activity_number)) %>% ungroup() %>% 
  ggplot(aes(year_month, total_activity)) + 
  geom_col() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  theme_bw()


ABT %>% group_by(year_month) %>% summarize(unique_users_count = n_distinct(by)) %>% ungroup() %>% 
  ggplot(aes(year_month, unique_users_count)) + 
  geom_col() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  theme_bw()
