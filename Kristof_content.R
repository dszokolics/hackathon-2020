library(readr)
library(tidyverse)
library(tidytext)
library(topicmodels)

content <- read_csv('../../../Downloads/content.csv')

content[3] <- NULL
content[4] <- NULL

train_on <- data.frame(content)

#train_on$text


train_on$text <- gsub("\\s*\\([^\\)]+\\)", " ", train_on$text)
train_on$text <- gsub("\\{.*?\\}", " ", train_on$text)
train_on$text <- gsub("\\(.*?\\)", " ", train_on$text)
train_on$text <- gsub("\\[.*?\\]", " ", train_on$text)
train_on$text <- gsub('[0-9]+', '', train_on$text)
train_on$text <- gsub("\\s+", " ", train_on$text)
train_on$text <- gsub("'.*? ", ' ', train_on$text)
train_on$text <- gsub('[[:punct:]]+', ' ', train_on$text)
train_on$text <- gsub("\\s+", " ", train_on$text)
train_on$text <- tolower(train_on$text)


count_by_url <- train_on %>% 
  unnest_tokens(word, text) %>% 
  anti_join(bind_rows(data.frame(word = stop_words$word), 
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = stopwords::stopwords(source = 'marimo')),
                      data.frame(word = stopwords::stopwords(source = 'snowball')),
                      data.frame(word = stopwords::stopwords(source = 'nltk')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey'))))  %>% 
  count(url, word, sort = T) %>% 
  ungroup()


write.csv(count_by_url, "G:/.shortcut-targets-by-id/0B23Ot7AW9q8dTHJCSU1MT1A2SzQ/Common/Homokozó/nlp-hackathon/data/collected/content_unnested.csv")


dtm <- count_by_url %>% cast_dtm(url, word, n)
lda <- dtm %>% LDA(k = 3, control = list(seed = 123))

# betas

lda_betas <- tidy(lda, matrix = 'beta')
lda_betas %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>% 
  mutate(term = as.factor(term),
         term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = F, width = 2/3) +
  facet_wrap(~topic, scales = 'free') + 
  coord_flip() + 
  scale_x_reordered() +
  theme_bw()

# gammas

lda_gammas <- tidy(lda, matrix = 'gamma')
lda_gammas %>%  
  rename('name' = 'document') %>% 
  mutate(topic = as.factor(topic),
         name = as.factor(name)) %>% 
  ggplot(aes(topic, gamma, fill = name)) + 
  geom_point(show.legend = F, color = 'black', shape = 8) +
  facet_wrap(~name, scales = 'free') + 
  theme_bw()

























  # group_by(url) %>% 
  # summarize(text = str_c(word, collapse = " ")) %>%
  # ungroup()


