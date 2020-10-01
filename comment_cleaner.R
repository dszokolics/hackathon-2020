library(jsonlite)
library(sentimentr)
library(dplyr)
library(tidytext)
library(stopwords)
library(ggplot2)
library(stringr)
library(textdata)
library(topicmodels)
library(lubridate)


rm(list = ls())
memory.limit(size=56000)


###############
# Read Data   #
###############

setwd('G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/collected')

comments_all <- readRDS('G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/collected/items_185_245.RDS')
str(comments_all)
# length: 5996184

abt <- read.csv("G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/ABT_base_full.csv", sep=',', header=T)
str(abt)

# select comments based on ABT's ID-s
comments_recent <- comments_all[comments_all$by %in% unique(abt$by), ]
comments_recent <- comments_all[comments_all$type == "comment", ]

###################
# Data Cleanings  #
###################

# create date field
comments_recent$time <- as.Date(as.POSIXct(comments_recent$time, origin="1970-01-01"))
comments_recent$time <- floor_date(as_date(comments_recent$time), "month")

comments_recent <- comments_recent[comments_recent$time > "2019-08-01", ]
str(comments_recent)
# length: 2909608


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


comments_recent$text <- gsub("nâ\200\2", "e", comments_recent$text) 

# regex other spams
comments_recent$text <- gsub("&gt;", "'", comments_recent$text)
comments_recent$text <- gsub("&#x2F;", " ", comments_recent$text)

# count of ! and ?
comments_recent$cnt_exclam <- str_count(comments_recent$text, pattern = "!")
comments_recent$cnt_ques <- str_count(comments_recent$text, pattern = '\\?')


# lowercasing the text:
comments_recent$text <- tolower(comments_recent$text)


# drop non alphanumerical characters
comments_recent$text <- gsub("'.*? ", " ", comments_recent$text)
comments_recent$text <- gsub("[^[:alnum:] ]", " ", comments_recent$text)

# drop numerical characters
comments_recent$text <- gsub("[0-9]", " ", comments_recent$text)

comments_recent$text <- gsub("â€™", "'", comments_recent$text)

# filter empty texts after regex
comments_recent <- comments_recent[comments_recent$text != "", ]


###########################################
# Sentiment analysis on the whole comment #
###########################################

# drop character after '
# before drop it run sentiment R sentiment analysis,
# on sentences:
comments1$sentiment <- sentiment(comments1$text)$sentiment

comments_recent$text <- gsub("'.*? ", " ", comments_recent$text)


#################
# Starting NLP  #
#################

# tokenization
# stopword removal
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

comments_tokenized <- comments_tokenized[!duplicated(comments_tokenized), ]

#################################
# Further feature engineering   #
#################################

# length of a comment (just tokens without stopwords)
comments_tokenized$token_cnt <- str_count(comments_tokenized$text, '\\w+')


# create sophisticated index:
sop <- c()
for (i in 1:nrow(comments_tokenized)){
  sop <- c(sop, length(unique(unlist(str_split(comments_tokenized$text[i], ' ')))) / length(unlist(str_split(comments_tokenized$text[i], ' '))))
}

comments_tokenized$sophis_index <- sop


###################################
# Group the data like ABT format  #
###################################

ABT_comment <- comments_tokenized %>% 
  group_by(by, time) %>% 
  summarize(sum_qout = sum(cnt_qout),
            sum_exclam = sum(cnt_exclam),
            sum_link = sum(cnt_links),
            cnt_comment = n(),
            avg_token_len = mean(token_cnt))

str(ABT_comment)

saveRDS(ABT_comment, 'G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comment_month_user_agg.rds')



###############################################################
# Remaining data process when runnig this on the whole ABT    #
###############################################################

"""
comments_recent1 <- comments_recent[1:1000000, ]
comments_recent2 <- comments_recent[1000001:2000000, ]
comments_recent3 <- comments_recent[2000001:2824962, ]

saveRDS(comments_recent1, file='G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comments_recent1.rds')
saveRDS(comments_recent2, file='G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comments_recent2.rds')
saveRDS(comments_recent3, file='G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comments_recent3.rds')


comments_recent1 <- readRDS('G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comments_recent1.rds')
comments_recent2 <- readRDS('G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comments_recent2.rds')
comments_recent3 <- readRDS('G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comments_recent3.rds')


#comment_tokenized1 <- comments_tokenized
#saveRDS(comment_tokenized1, file='G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comment_tokenized1.rds')
#str(comment_tokenized1)

#comment_tokenized2 <- comments_tokenized
#saveRDS(comment_tokenized2, file='G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comment_tokenized2.rds')
#str(comment_tokenized2)


comment_tokenized3 <- comments_tokenized
saveRDS(comment_tokenized3, file='G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comment_tokenized3.rds')
str(comment_tokenized3)




comment_tokenized1 <- readRDS('G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comment_tokenized1.rds')
comment_tokenized2 <- readRDS('G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comment_tokenized2.rds')
comment_tokenized3 <- readRDS('G:/Saját meghajtó/HiFly/Common/Homokozó/nlp-hackathon/data/transformed/comment_tokenized3.rds')

df <- rbind(comment_tokenized1, comment_tokenized2, comment_tokenized3)
"""

###################################
#               LDA               #
# (not used because the huge ABT) #
###################################


# LDA on tokenized tibble:
# include only words that occur at least 50 times
set.seed(123)
word_sci_comment <- comments_tokenized %>%
  group_by(id, time, word) %>%
  summarize(word_total = n()) %>%
  group_by(id, time) %>%
  mutate(n = sum(word_total)) %>%
  ungroup() %>%
  filter(n > 20) %>%
  sample_n(5000)

ggplot(word_sci_comment, aes(x=n)) + geom_histogram(binwidth = 5)



# convert into a document-term matrix
dtm <- word_sci_comment %>%
  cast_dtm(id, word, word_total)

lda <- LDA(dtm, k = 7, control = list(seed= 1234))

str(lda)

lda@gamma

# add LDA flag to tibble
chapter_topics <- tidy(lda, matrix = "gamma")

chapter_topics %>% 
  arrange(document, gamma) %>% View()


mean(chapter_topics$gamma)


comments_recent <- comments_recent %>% 
  left_join(chapter_classifications, by=c('id'='document'))



# visualize LDA
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()



