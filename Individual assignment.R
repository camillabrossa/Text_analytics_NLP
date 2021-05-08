# importing libraries
library(dplyr)
library(stringr)
library(tidytext)
library(tidyr)
library(tm)
library(pdftools)
library(ggplot2)

setwd("/Users/camillabrossa/Desktop/HULT Business School/MBAN/TEXT ANALYTICS & NLP/BUSINESS REPORT/2019")
nm1 <- list.files(path="/Users/camillabrossa/Desktop/HULT Business School/MBAN/TEXT ANALYTICS & NLP/BUSINESS REPORT/2019")
articles_2019 <- do.call(rbind, lapply(nm1, function(x) paste(pdf_text(x), collapse = " ")))

setwd("/Users/camillabrossa/Desktop/HULT Business School/MBAN/TEXT ANALYTICS & NLP/BUSINESS REPORT/2020")
nm2 <- list.files(path="/Users/camillabrossa/Desktop/HULT Business School/MBAN/TEXT ANALYTICS & NLP/BUSINESS REPORT/2020")
articles_2020 <- do.call(rbind, lapply(nm2, function(x) paste(pdf_text(x), collapse = " ")))

setwd("/Users/camillabrossa/Desktop/HULT Business School/MBAN/TEXT ANALYTICS & NLP/BUSINESS REPORT/2021")
nm3 <- list.files(path="/Users/camillabrossa/Desktop/HULT Business School/MBAN/TEXT ANALYTICS & NLP/BUSINESS REPORT/2021")
articles_2021 <- do.call(rbind, lapply(nm3, function(x) paste(pdf_text(x), collapse = " ")))


## FROM UNSTRUCTURED TO STRUCTURED DATA
# Tokenization

# 2019

colnames(articles_2019) <- c("text")

mydf19 <- data.frame(line=1:2, text = articles_2019[,1])

token_list19 <-  mydf19 %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

print(token_list19)

# 2020

colnames(articles_2020) <- c("text")

mydf20 <- data.frame(line=1:2, text = articles_2020[,1])

token_list20 <-  mydf20 %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

print(token_list20)

# 2021

colnames(articles_2021) <- c("text")

mydf21 <- data.frame(line=1:2, text = articles_2021[,1])

token_list21 <-  mydf21 %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE)

print(token_list21)



## SENTIMENT ANALYSIS
# get the sentiment with BING (binary: positive/negative) 
bing_2019 <- token_list19 %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bing_2019


bing_2020 <- token_list20 %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bing_2020 

bing_2021 <- token_list21 %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
bing_2021

# get a deeper view of sentiment with NRC
nrc_2019 <- token_list19 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
nrc_2019

nrc_2020 <- token_list20 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
nrc_2020

nrc_2021 <- token_list21 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
nrc_2021

# QUANTIFYING SENTIMENT
# counting bing words 2019
bing_words_19 <- token_list19 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_words_19

# counting bing words 2020
bing_words_20 <- token_list20 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


# counting bing words 2021
bing_words_21 <- token_list21 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()



########################

# counting nrc words 2019
nrc_words_19 <- token_list19 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_words_19

# counting nrc words 2020
nrc_words_20 <- token_list20 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_words_20

# counting nrc words 2021
nrc_words_21 <- token_list21 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_words_21

# VISUALIZATION: CONTRIBUTION TO SENTIMENT
# 2019 - Bing
bing_words_19 %>%
  group_by(sentiment) %>%
  top_n(3) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

# 2020 - Bing
bing_words_20 %>%
  group_by(sentiment) %>%
  top_n(3) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

# 2021 - Bing
bing_words_21 %>%
  group_by(sentiment) %>%
  top_n(2) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

###############
# 2019 - Nrc
nrc_words_19 %>%
  group_by(sentiment) %>%
  top_n(3) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()


# 2020 - Nrc
nrc_words_20 %>%
  group_by(sentiment) %>%
  top_n(3) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

# 2021 - Nrc
nrc_words_21 %>%
  group_by(sentiment) %>%
  top_n(3) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

####################

## TF_IDF 
global_idf <- bind_rows(mutate(token_list19, period="Before pandemic"),
                        mutate(token_list20, period="During pandemic"),
                        mutate(token_list21, period= "After pandemic"))

token_tf_idf <- global_idf %>%
  bind_tf_idf(word, period, n)
token_tf_idf

token_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  filter(n<10)

token_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(period) %>%
  filter(n<5) %>%
  top_n(7) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill=period))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~period, ncol=2, scales="free")+
  coord_flip()




