library(tidyverse)
library(lubridate)

setwd("G:/.shortcut-targets-by-id/0B23Ot7AW9q8dTHJCSU1MT1A2SzQ/Common/Homokozó/nlp-hackathon/data/collected")


data <- read_csv("user_registration_dates.csv")
data$year_month <- floor_date(as_date(data$created), "month")
data$X1 <- NULL

data <- data[!duplicated(data), ]
data %>% group_by(year_month) %>% summarize(count = n()) %>% arrange(desc(count)) %>% View()
View(data)

data %>% group_by(year_month) %>% summarize(count = n()) %>% ungroup() %>% 
  ggplot(aes(year_month, count)) + 
  geom_point() + 
  geom_line() + 
  theme_bw()


data %>% group_by(year_month) %>% summarize(count = n()) %>% ungroup() %>% arrange(desc(count))


# 2019 sept registered users for cohort

cohort_users <- data %>% filter(year_month == "2019-02-01") %>% select(id) %>% as.list()
ABT <- read_csv("../transformed/ABT_base_full.csv")
ABT <- ABT %>% filter(!is.na(by))
ABT$X1 <- NULL
ABT <- ABT %>% filter(is_active == 1)

ABT %>% group_by(year_month) %>% summarize(count= n())

cohort_data <- ABT %>% filter(by %in% cohort_users$id)
cohort_data <- cohort_data %>% filter(year_month >= "2019-02-01")





cohort_data %>% 
  filter(activity_number < 25, year_month != '2020-09-01') %>% 
  ggplot(aes(factor(year_month), activity_number)) +
  geom_boxplot(outlier.size = 1/2, outlier.color = 'blue', notch = T, notchwidth = 0.75, fill = 'lightblue', alpha = 0.5) +
  #scale_x_date(limits = as.Date(c('2019-06-01', '2020-08-01'))) +
  labs(title = 'Monthly distribution of activities seem constant',
       subtitle = 'Chosen user cohort: registration month is February, 2019',
       x =  NULL,
       y = 'Activity count (stories & comments)') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



cohort_data %>% group_by(year_month) %>% summarize(yes = sum(churn) / n(),
                                                   no = 1 - yes) %>% ungroup() %>% 
  filter(year_month != "2020-09-01") %>% 
  gather(churn, ratio, yes, no) %>% 
  filter(churn == 'yes') %>% 
  ggplot(aes(factor(year_month), ratio)) + 
  geom_col(fill = 'lightblue') +
  geom_text(aes(label = scales::percent(ratio, accuracy = 1)), vjust = -0.5, size = 4) +
  theme_bw() +
  labs(title = 'Our definition results in 20% monthly average churn ratios',
       subtitle = 'Chosen user cohort: registration month is February, 2019',
       x = NULL,
       y = "Churners %") +
  scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 0.05), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.position="bottom",
        legend.title = element_text(size=8),
        legend.text = element_text(size=8)) +
  theme(axis.ticks.y=element_blank())

ratios <- cohort_data %>% group_by(year_month) %>% summarize(yes = sum(churn) / n(),
                                                   no = 1 - yes) %>% ungroup() %>% 
  filter(year_month != "2020-09-01") %>% 
  gather(churn, ratio, yes, no) %>% 
  filter(churn == 'yes') %>% select(ratio)

mean(ratios$ratio)
