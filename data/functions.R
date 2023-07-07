library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(stringr)
library(purrr)
library(tidyr)

# Helper function to process the data and generate the category_counts
processEntitiesData <- function(selected_month) {
  selected_date <- parse_date_time(selected_month, "b-y")
  file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
  data <- get(file_name)
  
  # Create an empty dataframe
  variable_sums <- colSums(data[, 37:54])
  
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


hashtag_cooccurrence = function(data){
  require(tidyverse)
  # Extract individual hashtags
  all_hashtags <- unlist(str_extract_all(data$hashtags, "\\w+"))
  
  # Find the two most common hashtags
  top_hashtags <- head(sort(table(all_hashtags), decreasing = TRUE), 20)
  most_common_hashtags <- names(top_hashtags)
  
  # Create new variables for the most common hashtags
  for (hashtag in most_common_hashtags) {
    hashtag_column <- paste0("hashtag_", hashtag)  # Add prefix "hashtag_" to the column name
    data[[hashtag_column]] <- ifelse(grepl(hashtag, data$hashtags), 1, 0)
  }
  
  
  
  # Install and load the igraph and Matrix packages if needed
  if (!requireNamespace("igraph", quietly = TRUE)) {
    install.packages("igraph")
  }
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    install.packages("Matrix")
  }
  library(igraph)
  library(Matrix)
  
  # Extract the hashtag columns for co-occurrence graph
  co_occurrence_data <- data[, grepl("^hashtag_", names(data))]
  
  # Create an empty co-occurrence matrix
  co_occurrence_matrix <- sparseMatrix(
    i = integer(0),
    j = integer(0),
    dims = c(ncol(co_occurrence_data), ncol(co_occurrence_data)),
    dimnames = list(colnames(co_occurrence_data), colnames(co_occurrence_data)),
    giveCsparse = TRUE
  )
  
  # Compute the co-occurrence matrix
  for (i in 1:(ncol(co_occurrence_data) - 1)) {
    for (j in (i + 1):ncol(co_occurrence_data)) {
      intersection <- co_occurrence_data[, i] & co_occurrence_data[, j]
      if (sum(intersection) > 0) {
        co_occurrence_matrix[i, j] <- sum(intersection)
        co_occurrence_matrix[j, i] <- sum(intersection)
      }
    }
  }
  hashtag_freq <- colSums(co_occurrence_data)
  
  # Calculate the maximum co-occurrence count for edge width normalization
  max_co_occurrence <- max(co_occurrence_matrix)
  
  # Create an empty graph object
  graph <- graph.empty(n = ncol(co_occurrence_matrix), directed = FALSE)
  
  # Add nodes to the graph with varying size based on frequency
  V(graph)$name <- gsub("hashtag_", "", colnames(co_occurrence_data))
  V(graph)$size <- sqrt(hashtag_freq) * 0.5  # Adjust the scaling factor as needed
  
  # Set the scaling factor to adjust edge width
  edge_scaling_factor <- 1
  
  # Add edges to the graph based on co-occurrence with varying width
  for (i in 1:(ncol(co_occurrence_matrix) - 1)) {
    for (j in (i + 1):ncol(co_occurrence_matrix)) {
      if (co_occurrence_matrix[i, j] > 0) {
        edge_width <- edge_scaling_factor * (co_occurrence_matrix[i, j] / max_co_occurrence)
        graph <- add_edges(graph, c(i, j))
        E(graph)$width[ecount(graph)] <- edge_width
      }
    }
  }
  
  # Plot the co-occurrence graph
  graph1 = plot(graph, vertex.label = V(graph)$name, vertex.size = V(graph)$size, edge.width = E(graph)$width, layout = layout.fruchterman.reingold)
  return(graph1)
}