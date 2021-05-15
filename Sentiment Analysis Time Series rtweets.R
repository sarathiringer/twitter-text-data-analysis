# Packages needed
library(rtweet)
library(tidyverse)
library(tidytext)
library(stopwords)

# Get Twitter data on Black Lives Matter hashtag
blm <- search_tweets("#blm", n = 100000, include_rts = FALSE)

# Function for cleaning tweets
clean_tweets <- function(x) {
  x %>%
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    str_replace_all("&amp;", "and") %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("^RT:? ") %>%
    str_remove_all('@\\w+ *') %>% 
    #str_remove_all("@[[:alnum:]]+") %>%
    str_remove_all("#[[:alnum:]]+") %>%
    str_replace_all("\\\n", " ") %>%
    str_to_lower() %>%
    str_trim("both")
}

# Tidy format
tidy_blm <- blm %>% 
  select(user_id, text) %>%
  mutate(tweet_nr = row_number()) %>%
  mutate(text = clean_tweets(text)) %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  mutate(word = str_extract(word, "[a-z']+"))

# Check 'anger' sentiment
nrc_anger <- get_sentiments("nrc") %>% 
  filter(sentiment == "anger")

tidy_blm %>%
  inner_join(nrc_anger) %>%
  count(word, sort = TRUE)

# Check positive (note 'police', should probably be changed)
tidy_blm %>%
  inner_join(filter(get_sentiments("nrc"), sentiment == "positive")) %>%
  count(word, sort = TRUE)

# Sentiment analysis
blm_sentiment_tweet <- tidy_blm %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(tweet_nr) %>% 
  summarize(sentiment = sum(value)) %>% 
  left_join(distinct(select(tidy_blm, user_id, tweet_nr)))

# blm_sentiment_tweet %>% 
#   group_by(user_id) %>% 
#   summarise(mean_sentiment = mean(sentiment), .groups = 'keep') %>% 
#   ggplot(aes(x = user_id, y = mean_sentiment)) +
#     geom_col()

# By date, average per day
# Tidy format
tidy_blm_ts <- blm %>% 
  select(created_at, text) %>%
  mutate(tweet_nr = row_number(), date = as.Date(created_at)) %>%
  select(-created_at) %>% 
  mutate(text = clean_tweets(text)) %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  mutate(word = str_extract(word, "[a-z']+"))

sentiment_ts <- tidy_blm_ts %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(date) %>% 
  summarize(avg_sentiment = mean(value))

sentiment_ts %>% 
  ggplot(aes(x = date, y = avg_sentiment)) +
  geom_point() +
  geom_line()

# Function to plot time series

plot_sentiment_ts <- function(tweetdata, language = 'en', lexicon = get_sentiments("afinn")) {
  stop_words <- tibble(word = stopwords::data_stopwords_nltk[[language]])
  
  tidy_data_ts <- tweetdata %>% 
    select(created_at, text) %>%
    mutate(tweet_nr = row_number(), date = as.Date(created_at)) %>%
    select(-created_at) %>% 
    mutate(text = clean_tweets(text)) %>% 
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    mutate(word = str_extract(word, "[a-z']+"))
  
  sentiment_ts <- tidy_data_ts %>%
    inner_join(lexicon) %>% 
    group_by(date) %>% 
    summarize(avg_sentiment = mean(value))
  
  sentiment_ts %>% 
    ggplot(aes(x = date, y = avg_sentiment)) +
    geom_point() +
    geom_line()
}

# Other hashtags
gaza <- search_tweets("#gaza", n = 50000, include_rts = FALSE)
plot_sentiment_ts(gaza)

# Frequency ts
ts_plot(blm)

jkrow <- search_tweets("#jkrowling", n = 50000, include_rts = FALSE)
plot_sentiment_ts(jkrow)

# Swedish
lexicon_swe <- read.csv('sentimentlex_swe.csv', sep =',') %>% 
  filter(length(strength) > 3) %>% 
  mutate(value = as.numeric(strength)) %>% 
  select(word, value) %>% 
  tibble() # Warning can safely be ignored (I think)
swe_tweets <- search_tweets("#svpol", n = 25000, include_rts = FALSE)
plot_sentiment_ts(swe_tweets, language = 'sv', lexicon = lexicon_swe)
