library(jsonlite)


setwd('G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/collected')



comments_recent_2 <- fromJSON(readLines('items_20.json'))
str(comments_recent_2)

###########################
# cleaning comment field  #
###########################

comments_recent <- comments_recent[!is.na(comments_recent$text), ]
comments_recent$text <- gsub("&#x27;", "'", comments_recent$text)

# count of href than regex it
comments_recent$cnt_links <- str_count(comments_recent$text, pattern = "<a.*?/a>")
comments_recent$text <- gsub("<a.*?/a>", "", comments_recent$text)

# regex HTML tags like <p></p>
comments_recent$text <- gsub("<.*?>", "", comments_recent$text)
comments_recent$text <- gsub("\n", "'", comments_recent$text)

# count of qoutes than regex it
comments_recent$cnt_qout <- str_count(comments_recent$text, pattern = "&quot;")
comments_recent$text <- gsub("&quot;", "", comments_recent$text)

# regex other spams
comments_recent$text <- gsub("&gt;", "'", comments_recent$text)
comments_recent$text <- gsub("&#x2F;", " ", comments_recent$text)

# count of ! and ?
comments_recent$cnt_exclam <- str_count(comments_recent$text, pattern = "!")
comments_recent$cnt_ques <- str_count(comments_recent$text, pattern = "?")


# lowercasing the text:
comments_recent$text <- tolower(comments_recent$text)


# drop character after '
comments_recent$text <- gsub("'.*? ", " ", comments_recent$text)


# drop non alphanumerical characters
comments_recent$text <- gsub("[^[:alnum:] ]", " ", comments_recent$text)

# drop numerical characters
comments_recent$text <-gsub("[0-9]", " ", comments_recent$text)

# filter empty texts after regex
comments_recent <- comments_recent[comments_recent$text != "", ]

# length(comments_recent$text[78]) < 10

head(comments_recent)

#################
# Starting NLP  #
#################

library(dplyr)
library(tidytext)
#install.packages('stopwords')
library(stopwords)
library(ggplot2)

install.packages('textdata')
library(textdata)


# unnest_tokens
# stopwords remove
comments_tokenized <- comments_recent  %>%
                        unnest_tokens(word, text) %>%
                        anti_join(bind_rows(data.frame(word = stop_words$word),
                                            data.frame(word = stopwords::stopwords(source = 'smart')),
                                            data.frame(word = stopwords::stopwords(source = 'marimo')),
                                            data.frame(word = stopwords::stopwords(source = 'snowball')),
                                            data.frame(word = stopwords::stopwords(source = 'nltk')),
                                            data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')))) %>%
                        group_by(id) %>%
                        summarize(text = str_c(word, collapse = " ")) %>%
                        ungroup() 

comments_tokenized <- comments_tokenized %>%
  left_join(select(comments_recent, by, id, parent, type, time, cnt_links, cnt_qout, cnt_exclam), by='id')


#################################
# Further feature engineering   #
#################################

# length of comments:
len <- c()
for (i in 1:nrow(comments_tokenized)){
  len <- c(len, length(unlist(str_split(comments_tokenized$text[i], ' '))))
}

comments_tokenized$len <- len

ggplot(comments_tokenized, aes(x=len)) + geom_histogram()


# create sophisticated index:
sop <- c()
for (i in 1:nrow(comments_tokenized)){
  sop <- c(sop, length(unique(unlist(str_split(comments_tokenized$text[i], ' ')))) / length(unlist(str_split(comments_tokenized$text[i], ' '))))
}

comments_tokenized$sophis_index <- sop

ggplot(comments_tokenized, aes(x=len)) + geom_histogram()


# sentiment analysis
str(comments_tokenized)

comments_tokenized %>%
  inner_join(get_sentiments("afinn"), by = "text") %>%
  group_by(text) %>%
  summarize(value = sum(value * n) / sum(n))


comments_recent  %>%
  unnest_tokens(word, text) %>%
  anti_join(bind_rows(data.frame(word = stop_words$word),
                      data.frame(word = stopwords::stopwords(source = 'smart')),
                      data.frame(word = stopwords::stopwords(source = 'marimo')),
                      data.frame(word = stopwords::stopwords(source = 'snowball')),
                      data.frame(word = stopwords::stopwords(source = 'nltk')),
                      data.frame(word = c('yeah', 'gonna', 'uh', 'alright', 'um', 'lot', 'hey')))) %>%
  count(id, word, sort=T) %>%
  ungroup() %>%
  left_join(get_sentiments("afinn"), by = "word") %>%
  mutate(sentiment_value = n * value) %>%
  group_by(id) %>%
  summarize(text = str_c(word, collapse = " "), contribution = sum(sentiment_value)) %>%
  ungroup() 

comments_tokenized$text[1]




