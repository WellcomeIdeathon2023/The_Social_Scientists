"""The aim of this file is to create functions that can
track negative sentiment over time.
"""

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(stringr)
library(purrr)
library(tidyr)

# Specify the file path of your CSV file
csv_file <- "data/vax_tweets_with_sentiment_entities_3.csv"

# Read the CSV file
data <- read_csv(csv_file)

# Define the function that sums up
# Positive sentiment over time
compute_sums_every_five_days <- function(df) {
  df %>%
    mutate(date = as.Date(date),  # Convert 'date' to Date object
           date_interval = floor_date(date, "3 days")) %>%  # Create 'date_interval' column
    group_by(date_interval) %>%
    summarise(sum_favourites_pos = sum(user_favourites[polarity_text > 0.1], na.rm = TRUE), # Compute sum of 'user_favourites' for negative tweets
              sum_favourites_neg = sum(user_favourites[polarity_text < -0.1], na.rm = TRUE), # Compute sum of 'user_favourites' for negative tweets
              sum_favourites_all = sum(user_favourites, na.rm = TRUE)) %>%  # Compute total sum of 'user_favourites'
    mutate(normalized_pos = sum_favourites_pos / sum_favourites_all)  %>%
    mutate(normalized_neg = sum_favourites_neg / sum_favourites_all)
}
# Try it out
fiveday_sums <- compute_sums_every_five_days(data)

# Define the function
label_high_periods <- function(df) {
    df %<>%
        arrange(date_interval) %>%  # Ensure data is ordered by date
        mutate(rolling_mean_pos = rollapply(normalized_pos, width = 10, FUN = mean, align = "right", fill = NA),  # Calculate rolling mean of past 3 periods for positive tweets
            rolling_mean_neg = rollapply(normalized_neg, width = 10, FUN = mean, align = "right", fill = NA),  # Calculate rolling mean of past 3 periods for negative tweets
            high_pos = ifelse(normalized_pos > 2 * lag(rolling_mean_pos), TRUE, FALSE),  # Identify high periods for positive tweets
            high_neg = ifelse(normalized_neg > 2 * lag(rolling_mean_neg), TRUE, FALSE))  # Identify high periods for negative tweets
    df <- replace(df, is.na(df), 0)
    return(df)
}

# Use the function
fiveday_sums <- label_high_periods(fiveday_sums)


### Plot
# Create a new data frame for high_neg == TRUE
high_neg_dates <- fiveday_sums %>%
  filter(high_neg == TRUE) %>%
  select(date_interval)

# Then, in the geom_vline() function, you use the new data frame
ggplot(fiveday_sums, aes(x = date_interval)) +
  geom_line(aes(y = normalized_pos), color = 'blue') +
  geom_line(aes(y = normalized_neg), color = 'red') +
  geom_vline(data = high_neg_dates, aes(xintercept = as.numeric(date_interval)),
             linetype = "dashed",
             color = "black",
             size = 0.5) +
  labs(title = "Normalized Sentiment Over Time",
       x = "Date Interval",
       y = "Normalized Sentiment Score",
       color = "Legend") +
  scale_color_manual(values = c("blue", "red"), labels = c("Positive", "Negative")) +
  theme_minimal()

# For any datetime, we want to look at the hashtags most correlated
# with negative polarity
# Function to get most common hashtags associated with negative sentiment
get_negative_hashtags <- function(df, timeframe_start, timeframe_end) {
    df<-data
    timeframe_start <- "2020-01-01"
    timeframe_end <- "2021-06-30"
  # Convert date to Date object
  df$date <- as.Date(df$date)
  
  # Select rows within timeframe
  df <- df %>%
    filter(date >= as.Date(timeframe_start) & date <= as.Date(timeframe_end))
  
  # Parse the 'hashtag' column as a list and unnest
  df <- df %>%
    mutate(hashtags = str_extract_all(hashtags, "\\w+")) %>%
    unnest(hashtags)
  
  # Filter for negative sentiment
  df <- df %>%
    filter(polarity_text < -0.1)
  
  # Count frequency of each hashtag
  df <- df %>%
    count(hashtags, sort = TRUE)
  
  return(df)
}

# Function to identify hashtags with the highest negative correlation
get_correlated_negative_hashtags <- function(df, timeframe_start, timeframe_end) {
  # Convert date to Date object
  df$date <- as.Date(df$date)
  
  # Select rows within timeframe
  df <- df %>%
    filter(date >= as.Date(timeframe_start) & date <= as.Date(timeframe_end))
  
  # Parse the 'hashtag' column as a list and unnest
  df <- df %>%
    mutate(hashtags = str_extract_all(hashtags, "\\w+")) %>%
    unnest(hashtags) 
  
  # Create dummy variables for each hashtag
  df <- df %>%
    mutate(hashtags = as.factor(hashtags)) %>%
    pivot_wider(names_from = hashtags, values_from = hashtags,
              names_prefix = "hashtags_",
              values_fill = 0,
              values_fn = function(x) 1)
  
  # Calculate correlation between each hashtag and sentiment
  cor_df <- sapply(df[, grepl("^hashtags_", names(df))], 
                  function(hashtags_col) cor(df$polarity_text, hashtags_col, use = "pairwise.complete.obs"))
  
  # Convert to data frame
  cor_df <- data.frame(hashtags = names(cor_df), correlation = cor_df)
  
  # Filter for negative correlations
  cor_df <- cor_df %>%
    filter(correlation < 0) %>%
    arrange(correlation)  # arrange in ascending order so most negative come first
  
  cor_df$hashtags <- sub("^hashtags_", "", cor_df$hashtags)

  return(cor_df)
}

# Example usage
correlated_negative_hashtags <- get_correlated_negative_hashtags(data, "2020-01-01", "2020-06-30")
View(correlated_negative_hashtags)
