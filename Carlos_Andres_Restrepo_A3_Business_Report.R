# libraries used 
#install.packages("tibble")
library(pdftools)
library(NLP)
library(tm)
library(dplyr)
library(tidytext)
library(tidyverse)
library(tibble)
library(ggplot2)
library(plotly)
library(tidyr)
library(igraph)
library(ggraph)

################################################################################
# importing the pdf files
setwd("/Users/USER/OneDrive - Hult Students/Documents/Hult - Courses/7 - Text Analytics and NLP/A3 - Business Insight Report/documents")
files <- list.files(path="/Users/USER/OneDrive - Hult Students/Documents/Hult - Courses/7 - Text Analytics and NLP/A3 - Business Insight Report/documents") 
fs_documents <- lapply(files, pdf_text)
fs_doc <- do.call(rbind, lapply(files, function(x) pdf_text(x)))
fs_tibble <- tibble(fs_doc)

my_stop_words <- c("v3.0", "Wheel.pdf", "Impact.pdf", "Cards.pdf", "Modeling.pdf",
                   "Analysis.pdf", "Games.pdf", "Forecasting.pdf")

################################################################################
# creating the objects for text analytics
# creating the corpus
Rpdf <- readPDF(control = list(text = "-layout"))
fs_corpus <- Corpus(URISource(files), 
                    readerControl = list(reader = Rpdf)) 

#fs_corpus[[2]]$meta$author

# creating the DTM 
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
fs_dtm <- DocumentTermMatrix(fs_corpus, control = list(tokenize="words", 
                                                       removePunctuation = TRUE, 
                                                       stopwords = c(stopwords("english"),my_stop_words), 
                                                       stemming = TRUE))



inspect(fs_dtm)

# creating the tidy object
fs_tidy <- tidy(fs_dtm) 

# calculating the td-idf
fs_tidy_idf <- fs_tidy %>% 
  bind_tf_idf(term, document, count) %>% 
  arrange(desc(tf_idf))

fs_tidy_idf

# calculating the total of words per document
total_words <- fs_tidy_idf %>%
  group_by(document) %>%
  summarize(total=sum(count))

# joining tidy object and count of words
fs_tokens <- left_join(fs_tidy_idf, total_words)


# calculating the frequency by rank
freq_by_rank <- fs_tokens %>%
  group_by(document) %>%
  mutate(rank = row_number(),
         `term frequency` = count/total)
freq_by_rank



################################################################################
# Plotings based on tokens

# ploting the frequency of words
ggplot(fs_tokens, aes(count/total, fill = document))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~document, ncol=2, scales="free_y")

# ploting the Zip's law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=document))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10()+
  scale_y_log10()


# ploting the most frequent words
fs_tidy_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(term, levels=rev(unique(term)))) %>%
  group_by(document) %>%
  top_n(20) %>%
  ungroup %>%
  ggplot(aes(x=word, y=tf_idf, fill=document))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~document, ncol=2, scales="free")+
  coord_flip()



################################################################################
# N-grams

my_stop_words <- c("v3.0", ".pdf")

# creating the bigrams removing the stopwords
fs_ngrams <- tidy(fs_corpus) %>% 
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word1 %in% my_stop_words) %>%
  filter(!word2 %in% my_stop_words) %>%
  filter(!word3 %in% my_stop_words) %>%
  unite(trigram, word1, word2, word3, sep=" ")

# priotitizing the bigrams usinf tf_idf
fs_ngrams_tf_idf <- fs_ngrams %>%
  count(id, trigram) %>%
  bind_tf_idf(trigram, id, n) %>%
  arrange(desc(tf_idf))

# priotitizing the ngrams usinf tf_idf
fs_ngrams_tf_idf <- fs_ngrams %>%
  count(id, trigram) %>%
  bind_tf_idf(trigram, id, n) %>%
  arrange(desc(tf_idf))

fs_ngrams_tf_idf


# visualizing the bigram
# preparing for chart
fs_ngrams_graph <- fs_ngrams_tf_idf %>%
  filter(n>20) %>%
  graph_from_data_frame()
fs_ngrams_graph

# charting the n-gram networkd
ggraph(fs_ngrams_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


