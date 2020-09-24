library(tidyverse)
library(jsonlite)
library(data.table)
library(modiscloud)


setwd("G:/.shortcut-targets-by-id/0B23Ot7AW9q8dTHJCSU1MT1A2SzQ/Common/Homokozó/nlp-hackathon/data")

### COMMENTS

comments <- fromJSON(readLines("comments_recent.json"))
comments <- comments[complete.cases(comments), ]

str(comments)
comments$time <- as.POSIXct(comments$time)
comments$type <- NULL
comments$by <- as.factor(comments$by)

comments %>% group_by(by) %>% dplyr::summarize(count = n()) %>% ungroup() %>% arrange(desc(count)) %>% top_n(20) %>% 
  ggplot(aes(reorder(by, -count),  count)) +
  geom_col()


### ARTICLES

articles <- fromJSON(readLines("articles.json"))
articles$kids <- NULL
articles$url <- NULL
articles <- unlist_df(articles)


articles$time <- as.POSIXct(articles$time)
articles$type <- NULL
articles$by <- as.factor(articles$by)

articles <- data.frame(articles)

articles %>% group_by(by) %>% dplyr::summarize(count = n()) %>% ungroup() %>% arrange(desc(count)) %>% top_n(20) %>% 
  ggplot(aes(reorder(by, -count),  count)) +
  geom_col()

# relations
# article # vs comment #

most_articles <- articles %>% group_by(by) %>% dplyr::summarize(art_num = n()) %>% ungroup() %>% arrange(desc(art_num))
most_comments <- comments %>% group_by(by) %>% dplyr::summarize(com_num = n()) %>% ungroup() %>% arrange(desc(com_num))

joined <- inner_join(most_articles, most_comments)
joined <- joined %>% filter(art_num > 5, com_num > 5)

joined %>% ggplot(aes(art_num, com_num)) + 
  geom_point()
