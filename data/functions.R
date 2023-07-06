# Helper function to process the data and generate the category_counts
processEntitiesData <- function(selected_month) {
  selected_date <- parse_date_time(selected_month, "b-y")
  file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
  data <- get(file_name)
  
  # Create an empty dataframe
  variable_sums <- colSums(data[, 17:34])
  
  category_counts <- data.frame(Category = names(variable_sums), Count = variable_sums)
  
  return(category_counts)
}

# Define the function that sums up positive sentiment over time
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

# Function to get most common hashtags associated with negative sentiment
get_negative_hashtags <- function(df, timeframe_start, timeframe_end) {
  df$date <- as.Date(df$date)
  
  df <- df %>%
    filter(date >= as.Date(timeframe_start) & date <= as.Date(timeframe_end))
  
  df <- df %>%
    mutate(hashtags = str_extract_all(hashtags, "\\w+")) %>%
    unnest(hashtags)
  
  df <- df %>%
    filter(polarity_text < -0.1)
  
  df <- df %>%
    count(hashtags, sort = TRUE)
  return(df)
}

# Function to identify hashtags with the highest negative correlation
get_correlated_negative_hashtags <- function(df, timeframe_start, timeframe_end) {
  df$date <- as.Date(df$date)
  
  df <- df %>%
    filter(date >= as.Date(timeframe_start) & date <= as.Date(timeframe_end))
  
  df <- df %>%
    mutate(hashtags = str_extract_all(hashtags, "\\w+")) %>%
    unnest(hashtags) 
  
  df <- df %>%
    mutate(hashtags = as.factor(hashtags)) %>%
    pivot_wider(names_from = hashtags, values_from = hashtags,
                names_prefix = "hashtags_",
                values_fill = 0,
                values_fn = function(x) 1)
  
  cor_df <- sapply(df[, grepl("^hashtags_", names(df))], 
                   function(hashtags_col) cor(df$polarity_text, hashtags_col, use = "pairwise.complete.obs"))
  
  cor_df <- data.frame(hashtags = names(cor_df), correlation = cor_df)
  
  cor_df <- cor_df %>%
    filter(correlation < 0) %>%
    arrange(correlation)
  
  cor_df$hashtags <- sub("^hashtags_", "", cor_df$hashtags)
  
  return(cor_df)
}
