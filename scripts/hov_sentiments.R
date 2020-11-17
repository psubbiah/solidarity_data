# Sentiment Analysis, Word Frequency and Bigram network Analysis on HOV Articles using NRC Lexicon
# Code developed by Parvathi Subbiah 2019-2020

# Rscript hov_sentiments.R input_path output_path

#### Dependencies #####
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(wordcloud)
library(tm)
library(webshot)
library(htmlwidgets)
library(ggraph)
library(igraph)
library(textdata)
library(readr)
library(viridis)
library(RColorBrewer)
library(widyr)

#### Arguments ####

args <- commandArgs(trailingOnly=TRUE)

input_path <- '/Users/ale/solidarity_data/data/hov_csv2.csv'
#input_path <- as.character(args[1])
output_path <- '/Users/ale/RStudio/'
#output_path <- as.character(args[2])

#### ggplot theme and configs ####
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

my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")  

# Read clean csv with HOV Articles and metadata
input <- read_csv(input_path, trim_ws = TRUE)
print('Reading csv file...')

# Clean Title, Author for white space
input$Title <- stripWhitespace(input$Title)
input$Author <- stripWhitespace(input$Author)

print('Tidying dataset')

# Add year column
input <- input %>%
  mutate(year = lubridate::year(input$Date)) 

# Create tidy text format: Unnested, remove PS and empty images column from scraping
hov_clean <- input %>%
  select(-images, -PS) %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words)

# Join to sentiments lexicon NRC
hov_nrc <- hov_clean %>%
  inner_join(get_sentiments("nrc"))
print('Starting Sentiment Analysis...')

# Graph for words that contribute the most to each sentiment for all years
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
  
ggsave(paste0(output_path, '/sentiments_hov.pdf'), width=8, height=4)
print('Saved Sentiment Analysis Plot successfully.')

# graph that shows words that contribute most to each sentiment for 2017-2019  
# 
# hov_plot2017 <- hov_nrc %>%
#   filter(!sentiment %in% c("positive", "negative")) %>%
#   filter(year %in% c("2017", "2018", "2019")) %>%
#   group_by(sentiment) %>%
#   count(word, sort = TRUE) %>%
#   arrange(desc(n)) %>%
#   slice(seq_len(8)) %>% 
#   ungroup() + 
#   
# hov_plot2017 %>%    
#     #set `y = 1` to just plot one variable and use word as label
#     ggplot(aes(word, 1, label = word, fill = sentiment)) + 
#     #words not points. 
#     geom_point(color = "transparent") + 
#     #make sure labels don't overlap
#     geom_label_repel(force = 6,
#                      direction = "y",
#                      segment.color = "transparent",
#                      fontface = 'bold', color = "white",
#                      size = 2) + 
#     facet_grid(~sentiment) + 
#     theme_hov() +
#     theme(axis.text.y = element_blank(), axis.text.x = element_blank(), 
#           axis.title.x = element_text(size = 6),
#           panel.grid = element_blank(), panel.background = element_blank(),
#           panel.border = element_rect("lightgray", fill = NA),
#           strip.text.x = element_text(size = 9)) +
#     xlab(NULL) + ylab(NULL) + 
#     ggtitle("NRC Sentiment of HOV Articles gathered from 2017-2019")
#   coord_flip() 
# ggsave(path = output_path, filename = "sentiments_hov2017-2019.png")

#### wordcloud freq analysis ####

hov_stopwords <- tibble(word = c("venezuela", "venezuelan", "de", "la", "111", "ef", "var", "email",
                                 "address", "innerhtml", "javascript", "path", "hr", "hov", "108", 
                                 "116", "109",  "46", "101", "105", "64", "97", "nds", "natfhe", "acod", 
                                 "onderwijs", "pcs", "document.getelementbyid", "los", "prefix", "spambots", "ffv", "nds", "qui", "par", 
                                 "bbtk", "delgado", "fiom", "cgil", "milano", "shopsteward",
                                 "cc.oo", "gpa", "ugt", "e.c", "bda", "dwp", "sbq", "del", "por", 
                                 "fue", "las", "delegado", "delegada", "astilleros", "sevilla", "navantia",
                                 "ferrol", "u003d", "0cm", "vvn", "ate", "vez", "öfinger", "gerd",
                                 "hans", "avec", "pour", "dans", "rg"))



word_hov <- hov_clean %>%
  filter(!word %in% hov_stopwords$word) %>%

  group_by(word) %>%
  count(sort = TRUE)
 
webshot::install_phantomjs()

wordcloud <- wordcloud2(word_hov[0:100, ])
saveWidget(wordcloud, "tmp.html", selfcontained = F)
webshot::webshot("tmp.html", "wordcloud.pdf", delay = 10, vwidth = 2000, vheight = 500)

# Bigram analysis 
bigram_hov <- input %>%
  select(-images, -PS, URL) %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2)

# Bigram analysis with stopword extraction
bigram_hov_separated <- bigram_hov %>%
  separate(bigram, c('word1', 'word2'), sep = " ")

# List of words/terms that don't contribute to meaning (this needs to be added to args)
bigram_hov_filtered <- bigram_hov_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_hov_counts <- bigram_hov_filtered %>%
  count(word1, word2, sort = TRUE) 

# Filter for superfluos words, spanish stopwords, html terms
bigram_hov_filtered <- bigram_hov_counts %>%
  filter(!word1 %in% hov_stopwords$word) %>%
  filter(!word2 %in% hov_stopwords$word) 

bigrams_united <- bigram_hov_filtered %>%
  unite(bigram, word1, word2, sep = " ")


# Build bigram graph

print('Building bigram, i.e. visualization of Markov chain ...')

# Filter for relatively common combinations
bigram_graph <- bigram_hov_filtered %>%
  filter(n > 40) %>%
  graph_from_data_frame()
  
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.08, "inches"))

bigram_network <- ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.02, 'inches')) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, check_overlap = TRUE) +
  theme_void()

ggsave(paste0(output_path, '/bigram.pdf'), width = 12, height = 6, dpi = 200)

print('Bigram network analysis saved')

# Examine Pair-wide correlation, i.e how often they appear together 
# relative to how often they appear separately
# Calculate Phi Coefficient (equivalent to the Pearson correlation)

# word_cors <- hov_clean %>%
#   filter(!word %in% hov_stopwords$word) %>%
#   group_by(word) %>%
#   filter(n() >= 20) %>%
#   pairwise_cor(word, Title, sort = TRUE) %>%
#   filter(correlation < .95)


