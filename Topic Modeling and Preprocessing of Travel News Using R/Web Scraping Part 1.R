
library(rvest)       # For web scraping HTML content
library(tm)          # For text mining (stopwords, cleaning)
library(SnowballC)   # For stemming words
library(textclean)   # For contractions, emojis, emoticons handling
library(hunspell)    # For spell checking
library(textstem)    # For lemmatization

# Your URLs to scrape
urls <- c(
  "https://www.bbc.com/travel/article/20250415-why-all-inclusive-resorts-are-sending-guests-off-site",
  "https://www.thedailystar.net/news/bangladesh/news/travel-ban-st-martins-feb-businesses-seek-extension-3811336",
  "https://www.bbc.com/travel/article/20250520-the-us-travellers-afraid-to-travel-abroad-right-now",
  "https://www.bbc.com/travel/article/20250508-australias-mysterious-glowing-big-four",
  "https://www.bbc.com/travel/article/20250416-inside-the-fortress-that-inspired-hamlet",
  "https://www.bbc.com/travel/article/20240330-a-local-experts-guide-to-seeing-the-most-beautiful-tulips-in-the-netherlands",
  "https://www.bbc.com/travel/article/20250505-where-to-get-new-york-citys-best-chinese-food",
  "https://www.thedailystar.net/environment/news/70-turtles-found-dead-coxs-bazar-beach-3809851"
  
)

# Function to scrape and process one article URL
scrape_process_article <- function(url) {
  webpage <- read_html(url)
  
  # Extract headline
  headline <- webpage %>%
    html_node("h1") %>%
    html_text(trim = TRUE)
  
  # Extract summary from meta or fallback paragraph
  summary <- webpage %>%
    html_node("meta[name='description']") %>%
    html_attr("content")
  if(is.na(summary) || summary == "") {
    summary <- webpage %>%
      html_node("p") %>%
      html_text(trim = TRUE)
  }
  
  # Extract publication date from time or meta tag
  date <- webpage %>%
    html_node("time") %>%
    html_attr("datetime")
  if(is.na(date) || date == "") {
    date <- webpage %>%
      html_node("meta[property='article:published_time']") %>%
      html_attr("content")
  }
  
  # Extract author from meta or fallback to author class selector
  author <- webpage %>%
    html_node("meta[name='author']") %>%
    html_attr("content")
  if(is.na(author) || author == "") {
    author <- webpage %>%
      html_node(".byline__name") %>%
      html_text(trim = TRUE)
  }
  
  # Extract headings and paragraphs text (adjust selectors if needed)
  headings <- html_nodes(webpage, ".dfvxux, .dfvxux,.article-title ")
  heading_texts <- html_text(headings)
  paragraphs <- html_nodes(webpage, "p")
  ptext <- html_text(paragraphs)
  print(heading_texts)
  
  full_text <- paste(paste(heading_texts, collapse = " "), paste(ptext, collapse = " "))
  
  
  # --- Text Preprocessing ---
  
  ## Text Cleaning
  clean_text <- full_text %>%
    tolower() %>%                        # convert to lowercase
    replace_contraction() %>%            # expand contractions (e.g., "don't" → "do not")
    replace_emoji() %>%                  # replace emojis with text (e.g., 😊 → smile)
    replace_emoticon() %>%               # replace emoticons with text (e.g., :) → smile)
    gsub("<.*?>", " ", .) %>%            # remove HTML tags (if any remain)
    removeNumbers() %>%                  # remove digits
    removePunctuation() %>%              # remove punctuation marks
    stripWhitespace() %>%                # remove extra whitespace
    trimws()                             # trim leading and trailing whitespace
  
  
  
  # Spell checking (can be slow on large text)
  words <- unlist(strsplit(clean_text, "\\s+"))
  correct_spelling <- function(word) {
    if (!hunspell_check(word)) {
      suggestions <- hunspell_suggest(word)
      if (length(suggestions[[1]]) > 0) suggestions[[1]][1] else word
    } else {
      word
    }
  }
  corrected_words <- sapply(words, correct_spelling)
  clean_text <- paste(corrected_words, collapse = " ")
  
  # Tokenize and remove stopwords
  tokens <- unlist(strsplit(clean_text, "\\s+"))
  clean_tokens <- tokens[!tokens %in% stopwords("en")]
  
  # Stemming
  stemmed_tokens <- wordStem(clean_tokens)
  
  # ✅ Lemmatization using `textstem`
  lemmatized_tokens <- lemmatize_words(clean_tokens)
  lemmatized_tokens <- tolower(lemmatized_tokens)  # Ensure all lowercase
  
  # Final processed text
  final_text <- paste(lemmatized_tokens, collapse = " ")
  
  # Return a dataframe row for this article
  data.frame(
    Headline = headline,
    Link = url,
    Summary = summary,
    Date = date,
    Author = author,
    FullText = full_text,
    ProcessedText = final_text,
    stringsAsFactors = FALSE
  )
}

# Process all URLs, combine into one dataframe
all_articles <- do.call(rbind, lapply(urls, scrape_process_article))

# Save to CSV
write.csv(all_articles, "E:/10TH SEMESTER (15)/INTRODUCTION TO DATA SCIENCE [D]/FINAL/22-46775-1.csv", row.names = FALSE)

print(all_articles)
