# Load required packages
library(tm)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(RColorBrewer)

# Step 1: Load and prepare data
articles <- read.csv("E:/10TH SEMESTER (15)/INTRODUCTION TO DATA SCIENCE [D]/FINAL/22-46775-1.csv", 
                     stringsAsFactors = FALSE)
articles$ProcessedText <- as.character(articles$ProcessedText)

# Step 2: Create Document-Term Matrix
corpus <- Corpus(VectorSource(articles$ProcessedText))
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, sparse = 0.95)

# Step 3: Run LDA model
set.seed(123)
k <- 5  # Number of topics
lda_model <- LDA(dtm, k = k, control = list(seed = 123))

# Step 4: Extract and visualize topics

# 4.1 Get top terms
top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# 4.2 Top terms plot
top_terms_plot <- top_terms %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(reorder(term, beta), beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(title = "Top Terms by Topic Probability", 
       x = NULL, y = "Beta") +
  theme_minimal()

print(top_terms_plot)

# 4.3 Document-topic assignments
doc_topics <- tidy(lda_model, matrix = "gamma") %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  mutate(document = as.numeric(document))

# 4.4 Topic distribution plot
topic_dist <- doc_topics %>%
  count(topic) %>%
  ggplot(aes(x = factor(topic), y = n, fill = factor(topic))) +
  geom_col() +
  labs(title = "Document Distribution Across Topics",
       x = "Topic", y = "Number of Documents") +
  theme_minimal()

print(topic_dist)

# Step 5: Word Clouds

# 5.1 Overall word cloud
all_terms <- tidy(lda_model, matrix = "beta")
term_freq <- all_terms %>% 
  group_by(term) %>% 
  summarize(freq = mean(beta))

set.seed(123)
wordcloud(words = term_freq$term, 
          freq = term_freq$freq,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))




