library(rvest)
library(dplyr)
library(tm)         
library(ggplot2)

link <- "https://www.amazon.com/MSI-Katana-Gaming-Laptop-B13VFK-835US/product-reviews/B0CG7NVGQ2/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
page <- read_html(link)

title <- page %>% html_nodes(".review-title") %>% html_text()

# Cleaning the titles
cleaned_titles <- gsub("^[0-9]+\\.?[0-9]* out of 5 stars\\s*", "", title)
cleaned_titles <- gsub("\\s+", " ", cleaned_titles)

# Creating a corpus
corpus <- Corpus(VectorSource(cleaned_titles))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Calculating word frequency
tdm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(tdm)
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)

ggplot(word_freqs_df[1:10,], aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  xlab("Word") +
  ylab("Frequency") +
  ggtitle("Top 10 Words in Product Reviews") +
  coord_flip()

