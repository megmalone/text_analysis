# packages ----
library(tidyverse)
library(tidytext)
library(ggplot2)
library(devtools)
require(devtools)
install_github("lchiffon/wordcloud2")
library(wordcloud2)
library(ggthemes)

# twilight ----
twilight <- readLines("twilight2.txt")
twilight_df <- data.frame(text = twilight)
twilight_dl <- unnest_tokens(twilight_df, 
                             input = text, 
                             output = line, 
                             token = "sentences", 
                             to_lower = F)
twilight_dl2 <- twilight_dl %>%
  unnest_tokens(output = word, input = line, token = "words")
twilight_dl3 <- twilight_dl2 %>%
  anti_join(stop_words, by = c("word" = "word"))
twilight_dl4 <- twilight_dl3 %>%
   anti_join(names, by = c("word" = "word"))
twilight_words <- twilight_dl4 %>% 
  count(word, sort = TRUE)

twilight_words %>%
  top_n(20) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(stat = "identity") + coord_flip() +
  theme_tufte(ticks = FALSE) +
  ggtitle("Twilight") +
  labs(y = "Word Count", x = "")

twi_words_senti <- twilight_words %>%
  inner_join(bing, by = "word")
twi_words_senti %>%
  top_n(20)



twi_words_senti %>%
  top_n(20) %>%
  ggplot(aes(word, n, fill(sentiment))) +
  geom_bar(stat = "identity") + coord_flip() +
  theme_tufte(ticks = FALSE) +
  ggtitle("Twilight") +
  labs(y = "Word Count", x = "")


bing <- get_sentiments("bing")
twilight_senti <- twilight_dl4 %>%
  inner_join(bing)
twilight_senti_count <- twilight_senti %>% 
  count(sentiment, sort = TRUE)
ggplot(twilight_senti_count, aes(reorder(sentiment, n), n)) +
  geom_bar(stat = "identity") +
  theme_tufte(ticks = FALSE) +
  ggtitle("Twilight") +
  labs(y = "Word Count", x = "Word Sentiment")

# playing with afinn ----
afinn <- get_sentiments("afinn")
twilight_senti2 <- twilight_words %>%
  inner_join(afinn)
ggplot(twilight_senti2, aes(score, n)) +
  geom_point()

twilight_senti_count2 <- twilight_senti2 %>% 
  count(score)
ggplot(twilight_senti_count2, aes(score, n)) +
  geom_histogram(stat = "identity")
# new moon ----
new_moon <- readLines("new_moon2.txt")
new_moon_df <- data.frame(text = new_moon)
new_moon_dl <- unnest_tokens(new_moon_df, 
                             input = text, 
                             output = line, 
                             token = "sentences", 
                             to_lower = F)
new_moon_dl2 <- new_moon_dl %>%
  unnest_tokens(output = word, input = line, token = "words")
new_moon_dl3 <- new_moon_dl2 %>%
  anti_join(stop_words, by = c("word" = "word"))
new_moon_dl4 <- new_moon_dl3 %>%
  anti_join(names, by = c("word" = "word"))
new_moon_words <- new_moon_dl4 %>% 
  count(word, sort = TRUE)

new_moon_words %>%
  top_n(20) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(stat = "identity") + coord_flip() +
  theme_tufte(ticks = FALSE) +
  ggtitle("New Moon") +
  labs(y = "Word Count", x = "")

nm_words_senti <- new_moon_words %>%
  inner_join(bing, by = "word")
nm_words_senti20 <- nm_words_senti %>%
  top_n(20, "word")

new_moon_senti <- new_moon_dl3 %>%
  inner_join(bing)
new_moon_senti_count <- new_moon_senti %>% 
  count(sentiment, sort = TRUE)
ggplot(new_moon_senti_count, aes(reorder(sentiment, n), n)) +
  geom_bar(stat = "identity") +
  theme_tufte(ticks = FALSE) +
  ggtitle("New Moon") +
  labs(y = "Word Count", x = "Word Sentiment")

# eclipse ----
eclipse <- readLines("eclipse2.txt")
eclipse_df <- data.frame(text = eclipse)
eclipse_dl <- unnest_tokens(eclipse_df, 
                             input = text, 
                             output = line, 
                             token = "sentences", 
                             to_lower = F)
eclipse_dl2 <- eclipse_dl %>%
  unnest_tokens(output = word, input = line, token = "words")
eclipse_dl3 <- eclipse_dl2 %>%
  anti_join(stop_words, by = c("word" = "word"))
eclipse_dl4 <- eclipse_dl3 %>%
  anti_join(names, by = c("word" = "word"))
eclipse_words <- eclipse_dl4 %>% 
  count(word, sort = TRUE)

eclipse_words %>%
  top_n(20) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(stat = "identity") + coord_flip() +
  theme_tufte(ticks = FALSE) +
  ggtitle("Eclipse") +
  labs(y = "Word Count", x = "")

ec_words_senti <- eclipse_words %>%
  inner_join(bing, by = "word")
ec_words_senti20 <- ec_words_senti %>%
  top_n(20, "word")

eclipse_senti <- eclipse_dl3 %>%
  inner_join(bing)
eclipse_senti_count <- eclipse_senti %>% 
  count(sentiment, sort = TRUE)
ggplot(eclipse_senti_count, aes(reorder(sentiment, n), n)) +
  geom_bar(stat = "identity") +
  theme_tufte(ticks = FALSE) +
  ggtitle("Eclipse") +
  labs(y = "Word Count", x = "Word Sentiment")

# breaking dawn ----
breaking_dawn <- readLines("breaking_dawn2.txt")
breaking_dawn_df <- data.frame(text = breaking_dawn)
breaking_dawn_dl <- unnest_tokens(breaking_dawn_df, 
                           input = text, 
                             output = line, 
                             token = "sentences", 
                             to_lower = F)
breaking_dawn_dl2 <- breaking_dawn_dl %>%
  unnest_tokens(output = word, input = line, token = "words")
breaking_dawn_dl3 <- breaking_dawn_dl2 %>%
  anti_join(stop_words, by = c("word" = "word"))
breaking_dawn_dl4 <- breaking_dawn_dl3 %>%
  anti_join(names, by = c("word" = "word"))

breaking_dawn_words <- breaking_dawn_dl4 %>% 
  count(word, sort = TRUE)

breaking_dawn_words %>%
  top_n(20) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(stat = "identity") + coord_flip() +
  theme_tufte(ticks = FALSE) +
  ggtitle("Breaking Dawn") +
  labs(y = "Word Count", x = "")

bd_words_senti <- breaking_dawn_words %>%
  inner_join(bing, by = "word")
bd_words_senti20 <- bd_words_senti %>%
  top_n(20, "word")

breaking_dawn_senti <- breaking_dawn_dl3 %>%
  inner_join(bing)
breaking_dawn_senti_count <- breaking_dawn_senti %>% 
  count(sentiment, sort = TRUE)
ggplot(breaking_dawn_senti_count, aes(reorder(sentiment, n), n)) +
  geom_bar(stat = "identity") +
  theme_tufte(ticks = FALSE) +
  ggtitle("Breaking Dawn") +
  labs(y = "Word Count", x = "Word Sentiment")


#breaking dawn by book ----


# names ----
names <- read_excel("names.xls")
# wordclouds ----
wordcloud2(twilight_words %>% top_n(100), color = "darkred")
wordcloud2(new_moon_words %>% top_n(100), color = "darkred")
wordcloud2(eclipse_words %>% top_n(100), color = "darkred")
wordcloud2(breaking_dawn_words %>% top_n(100), color = "darkred")
