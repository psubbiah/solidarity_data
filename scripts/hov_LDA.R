# LDA Topic Modelling on HOV Articles

# Code developed by Parvathi Subbiah 2019-2020
# Rscript hov_sentiments.R input_path output_path

# Dependencies
library(cleanNLP)
library(dplyr)
library(readr)
library(magrittr)
library(ggplot2)
library(stringr)
library(rvest)
library(tidytext)
library(tm)
library(udpipe)
library(lattice)
library(topicmodels)
library(ggplot2)
library(ggrepel)
library(viridis)
library(forcats)
library(purrr)

# Arguments
args <- commandArgs(trailingOnly=TRUE)

#input_path <- '/Users/ale/solidarity_data/data/hov_csv2.csv'
input_path <- as.character(args[1])
output_path <- '/Users/ale/test_folder/'
output_path <- as.character(args[2])
#set_k <- as.double(args[3])

my_colours <- c('#481567FF', '#39568CFF', '#238A8DFF', '#95D840FF', '#FDE725FF')

# Read hov_csv file
input <- read_csv(input_path)

# Clean Title, Author for white space
input$Title <- stripWhitespace(input$Title)
input$Author <- stripWhitespace(input$Author)

# download and load 'english' udpipe model
print('Downloading UDpipe English model...')

model <- udpipe_download_model(language = "english")
model$file_model
ud_english <- udpipe_load_model(model$file_model)

# annotate input: (parts of speech et. al)
print('Annotating words in the corpus...')

s <- udpipe_annotate(ud_english, input$Text)
x <- data.frame(s)

# most occurring nouns
print('Graphing most occurring nouns...')

stats <- subset(x, upos %in% c("NOUN"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))

stats_topn <- stats %>%
  filter(freq > 370)

ggplot(data = stats_topn, aes(x = key, y = freq)) + 
  geom_col(fill = my_colours[1]) + 
  coord_flip() + 
  labs(title = "Most Ocurring Nouns",
       x = "Word", 
       y = "Frequency") 

ggsave(paste0(output_path, '/noun_frequency.pdf'), width = 12, height = 6, dpi = 200)
print('Plot Saved successfully.')

# barchart(key ~ freq, data = head(stats, 20), col = "cadetblue",
#          mail = "Most occurring nouns", xlab = "Freq")

#most occurring adjectives

print('Graphing most occurring adjectives...')

stats_adj <- subset(x, upos %in% c("ADJ"))
stats_adj <- txt_freq(stats_adj$token)
stats_adj$key <- factor(stats_adj$key, levels = rev(stats_adj$key))

# Remove superfluous terms and filter for > 200 freq
superfluous <- tibble(key = c("other", "var", "more", "many",
                              "new", "first", "last", "such", "main",
                              "same", "own", "few", "clear", "only"))

stats_adj_filtered <- stats_adj %>%
  filter(!key %in% superfluous$key)

stats_topadj <- stats_adj_filtered %>%
  top_n(n = 20)

ggplot(data = stats_topadj, aes(x = key, y = freq)) + 
  geom_col(fill = my_colours[2]) + 
  coord_flip() + 
  labs(title = "Most Ocurring Adjectives",
       x = "Word", 
       y = "Frequency") 

ggsave(paste0(output_path, '/adj_frequency.pdf'), width = 6, height = 6, dpi = 200)
print('Plot Saved successfully.')

# barchart(key ~ freq, data = head(stats_adj, 30, col = "purple", 
#                                  main = "Most occurring adjectives", xlab = "Freq"))

# Barchart for most occuring verbs
print('Graphing most occurring verbs...')

stats_ve <- subset(x, upos %in% c("VERB"))
stats_ve <- txt_freq(stats_ve$token)
stats_ve$key <- factor(stats_ve$key, levels = unique(rev(stats_ve$key)))

# Remove superfluous verbs

superfluous_ve <- tibble(key = c("is", "said", "explained", "have", "working",
                                "a'", "going", "has", "had", "was", "var", "prefix"))

stats_ve_filtered <- stats_ve %>%
  filter(!key %in% superfluous_ve$key)

stats_topve <- stats_ve_filtered %>%
  top_n(n = 20)

ggplot(data = stats_topve, aes(x = key, y = freq)) + 
  geom_col(fill = my_colours[3]) + 
  coord_flip() + 
  labs(title = "Most Ocurring Verbs",
       x = "Word", 
       y = "Frequency") 

ggsave(paste0(output_path, '/verb_frequency.pdf'), width = 6, height = 6, dpi = 200)
print('Plot Saved successfully.')


# barchart(key ~ freq, data = head(stats_ve, 30, col = "purple", 
#                                  main = "Most occurring adjectives", xlab = "Freq"))



#Automated Keywords Extraction with RAKE - Machine Learning unsupervised algorithm
# Starting RAKE Extraction

print('Starting RAKE algorithm NOUN-PHRASE extraction...')

# stats_rake <- keywords_rake(x = x, term = "lemma", group = "doc_id",
#                             relevant = x$upos %in% c("NOUN", "ADJ"))
# stats_rake$key <- factor(stats_rake$keyword, levels = rev(stats_rake$keyword))

####barchart(key ~ rake, data = head(subset(stats_rake, freq > 3.4 & freq > 2), 20), col = "red", 
####        main = "Keywords identified by RAKE", 
####          xlab = "Rake")


# ggplot(data = stats_topve, aes(x = key, y = freq)) +
#   geom_col(fill = my_colors[1]) +
#   coord_flip() +
#   labs(title = "Most Ocurring Verbs",
#        x = "Word",
#        y = "Frequency")

#Automated NOUN-VERB pairs

x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)

stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

stats_topnp <- stats %>%
  top_n(n = 20)

ggplot(data = stats_topnp, aes(x = fct_reorder(keyword, freq), y = freq)) +
  geom_col(fill = my_colours[4]) +
  coord_flip() +
  labs(title = "Most Ocurring Noun Phrases per RAKE algorithm",
       x = "Word",
       y = "Frequency")

ggsave(paste0(output_path, '/noun_phrases.pdf'), width = 6, height = 6, dpi = 200)
print('Plot Saved successfully.')


# barchart(key ~ freq, data = head(stats, 20), col = "magenta",
#          main = "Keywords - simple noun phrases", xlab = "Freq")

#Set variables using NLP
print('Starting LDA Topic Modelling...')

source_tidy <- x %>%
  select(doc_id, token, lemma, upos) %>%
  filter(upos == "NOUN")

source_dtm <- source_tidy %>%
  count(doc_id, token, sort = TRUE) %>%
  ungroup() %>%
  cast_dtm(doc_id, token, n)
print('Document Term Matrix sourced successfully.')

#function for word visualisation 
word_chart <- function(data, input, title) {
  data %>%
    #set y = 1 to just plot one variable and use word as the label
    ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
    #you want the words, not the points
    geom_point(color = "transparent") +
    #make sure the labels don't overlap
    geom_label_repel(nudge_x = .2,  
                     direction = "y",
                     box.padding = 0.1,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~topic) +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          #axis.title.x = element_text(size = 9),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    labs(x = NULL, y = NULL, title = title) +
    #xlab(NULL) + ylab(NULL) +
    #ggtitle(title) +
    coord_flip()
}

#LDA TopicModeling
seed = 4321
lda <- LDA(source_dtm, k = 4, method = "GIBBS", control = list(seed = seed))
lda2 <- LDA(source_dtm, k = 4, control = list(seed = seed))

lda3 <- LDA(source_dtm, k = 10, method = "GIBBS", control = list(seed = seed))
lda4 <- LDA(source_dtm, k = 4, control = list(seed = seed))

num_words <- 10 #number of words we want to see per topic

#create function that accepts lda model and num word to display


top_terms_per_topic <- function(lda_model, num_words, k) {
  #tidy LDA object to get work, topic and probability (beta)
  topics_tidy <- tidy(lda_model, matrix = "beta")
  
  top_terms <- topics_tidy %>%
  group_by(topic) %>%
  arrange(topic, desc(beta)) %>%
    #get the top num_workds PER topic
  slice(seq_len(num_words)) %>%
  arrange(topic, beta) %>% 
    #row is required for the word_chart() function
  mutate(row = row_number()) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic, sep = " "))
    #create a title to pass to word_chart
  title <- paste("LDA Top Terms for", k, "Topics")
    #call the word_chart function built
  word_chart(top_terms, top_terms$term, title)
}

#display top_terms found per topic by the LDA algorithm
print('Building Top Terms found per Topic, k = 4...')
top_terms_per_topic(lda, num_words, 4)

ggsave(paste0(output_path, '/LDA_top_4terms.pdf'), width = 8, height = 8, dpi = 200)
print('Plot Saved successfully.')

print('Building Top Terms found per Topic, k = 10...')
top_terms_per_topic(lda3, num_words, 10)

ggsave(paste0(output_path, '/LDA_top_10terms.pdf'), width = 12, height = 10, dpi = 200)
print('Plot Saved successfully.')
print('End of script.')





