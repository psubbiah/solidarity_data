##Bigram and Sentiment Analysis on HOV Articles using NRC Lexicon
##Code developed by Parvathi Subbiah
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(wordcloud2)
library(tm)

#ggplot theme
theme_hov <- function(aticks = element_blank(),
                      pgminor = element_blank(),
                      lt = element_blank(),
                      lp = "none")
  
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks = aticks,
        panel.grid.minor = pgminor,
        legend.title = lt,
        legend.position = lp)
}


#read clean csv with HOV Articles and metadata
input <- read_csv('/Users/pas89/R_files/hov_csv2.csv', trim_ws = TRUE)

#clean Title, Author for white space
input$Title <- stripWhitespace(input$Title)
input$Author <- stripWhitespace(input$Author)

#Add year column
input <- input %>%
  mutate(year = lubridate::year(input$Date)) 

#Create tidy text format: Unnested, remove PS and empty images column from scraping
hov_clean <- input %>%
  select(-images, -PS) %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words)

#join to sentiments lexicon NRC
hov_nrc <- hov_clean %>%
  inner_join(get_sentiments("nrc"))

#graph for words that contribute the most to each sentiment for all years
hov_nrc_plot <- hov_nrc %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(8)) %>% 
  ungroup()

hov_nrc_plot %>%
  #set `y = 1` to just plot one variable and use word as label
  ggplot(aes(word, 1, label = word, fill = sentiment)) + 
  #words not points. 
  geom_point(color = "transparent") + 
  #make sure labels don't overlap
  geom_label_repel(force = 3,
                   direction = "y",
                   segment.color = "transparent",
                   size = 3) + 
  facet_grid(~sentiment) + 
  theme_hov() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9)) +
  xlab(NULL) + ylab(NULL) + 
  ggtitle("NRC Sentiment of HOV Articles gathered from 2003-2019")
  coord_flip()

#graph that shows words that contribute most to each sentiment for 2017-2019  

hov_plot2017 <- hov_nrc %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  filter(year %in% c("2017", "2018", "2019")) %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(8)) %>% 
  ungroup()
  
hov_plot2017 %>%    
    #set `y = 1` to just plot one variable and use word as label
    ggplot(aes(word, 1, label = word, fill = sentiment)) + 
    #words not points. 
    geom_point(color = "transparent") + 
    #make sure labels don't overlap
    geom_label_repel(force = 6,
                     direction = "y",
                     segment.color = "transparent",
                     fontface = 'bold', color = "white",
                     size = 2) + 
    facet_grid(~sentiment) + 
    theme_hov() +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(), 
          axis.title.x = element_text(size = 6),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    xlab(NULL) + ylab(NULL) + 
    ggtitle("NRC Sentiment of HOV Articles gathered from 2017-2019")
  coord_flip()
  
#word freq analysis
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")  

word_hov <- hov_clean %>%
  group_by(word) %>%
  count(sort = TRUE)

wordcloud2(word_hov[0:100, ], fontFamily = "Didot")

#bigram analysis
bigram_hov <- input %>%
  select(-images, -PS) %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2)

#bigram analysis with stopword extraction
bigram_hov_separated <- bigram_hov %>%
  separate(bigram, c('word1', 'word2'), sep = " ")

#list of words that don't contribute to meaning
hov_stopwords <- tibble(word = c("venezuela", "venezuelan", "de", "la", "111", "ef", "var", "email",
                                 "address", "innerhtml", "javascript"))

bigrams_hov_filtered <- bigram_hov_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_hov_counts <- bigrams_hov_filtered %>%
  count(word1, word2, sort = TRUE) %>%
  head(n = 20)

#filter for superfluos words, spanish stopwords, html terms
hov_stopwords <- tibble(word = c("venezuela", "venezuelan", "de", "la", "111", "ef", "var", "email",
                                 "address", "innerhtml", "javascript", "path", "hr", "hov"))

bigrams_hov_filtered %>%
  filter(!word1 %in% hov_stopwords$word) %>%
  filter(!word2 %in% hov_stopwords$word) %>%
  count(word1, word2, sort = TRUE) %>%
  head(n = 20)


