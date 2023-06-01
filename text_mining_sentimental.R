# Load required libraries
library(tidyverse)
library(tidytext)
library(wordcloud)
library(reshape2)

# Define data extraction function
extract_data <- function(df, rows) {
  return(df[rows, 4])
}

# Extract data from multiple dataframes
HKT <- extract_data(Hickman, c(131:151, 267:278))
MDT <- extract_data(Mead, c(118:127, 239:248))
YDT <- extract_data(York, c(40:45,99:106))
LKT <- extract_data(Lincoln, c(25:27, 67:68))
NFT <- extract_data(Norflok, c(59:83, 136:146, 210:216, 262:275))

# Combine data and convert to a tidy format
TM <- c(HKT, MDT, YDT, LKT, NFT) %>%
  as_tibble() %>% 
  rename(text = value)

# Remove duplicates
TM_unique <- TM %>% distinct(text, .keep_all = TRUE)

# Unnest tokens
tidy_text <- TM_unique %>% unnest_tokens(word, text)

# Define sentiments
nrc_joy <- get_sentiments("nrc") %>% filter(sentiment == "joy")
bing_sentiments <- get_sentiments("bing")

# Perform text sentiment analysis
joy_word_counts <- tidy_text %>% inner_join(nrc_joy) %>% count(word, sort = TRUE)
bing_word_counts <- tidy_text %>%
  inner_join(bing_sentiments) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Define custom stop words
custom_stop_words <- bind_rows(tibble(word = c("erosion"), lexicon = c("custom")), stop_words)

# Generate wordcloud
tidy_text %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Perform sentiment comparison and generate comparison cloud
tidy_text %>% 
  inner_join(bing_sentiments) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("black", "blue"), max.words = 100)
