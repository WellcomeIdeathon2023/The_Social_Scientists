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
compute_sums_every_one_day <- function(df) {
  df %>%
    mutate(date = as.Date(date),  # Convert 'date' to Date object
           date_interval = floor_date(date, "1 days")) %>%  # Create 'date_interval' column
    group_by(date_interval) %>%
    summarise(sum_favourites_pos = sum(user_favourites[polarity_text > 0.1], na.rm = TRUE), # Compute sum of 'user_favourites' for negative tweets
              sum_favourites_neg = sum(user_favourites[polarity_text < -0.1], na.rm = TRUE), # Compute sum of 'user_favourites' for negative tweets
              sum_favourites_all = sum(user_favourites, na.rm = TRUE)) %>%  # Compute total sum of 'user_favourites'
    mutate(normalized_pos = sum_favourites_pos / sum_favourites_all)  %>%
    mutate(normalized_neg = sum_favourites_neg / sum_favourites_all)
}

# Define the function that sums up positive sentiment over time
compute_sums_every_thirty_days <- function(df) {
  df %>%
    mutate(date = as.Date(date),  # Convert 'date' to Date object
           date_interval = floor_date(date, "30 days")) %>%  # Create 'date_interval' column
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
    mutate(change_neg = normalized_neg - lag(normalized_neg, default = first(normalized_neg)), # Calculate the rate of increase
           high_neg = ifelse(change_neg > 0.1, TRUE, FALSE))  # Identify peaks where negative sentiment increased more than 0.1
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


# Plots the number of tweets and sentiments for a hashtag in specific
analyze_hashtag <- function(df, column_name, hashtag){
  # Remove the square brackets and split by comma
  df <- df %>%
    mutate(!!rlang::sym(column_name) := str_replace_all(!!rlang::sym(column_name), "\\[|\\]", "")) %>%
    mutate(!!rlang::sym(column_name) := strsplit(!!rlang::sym(column_name), ", "))
  
  # "Explode" the list into multiple rows
  df <- df %>%
    unnest(!!rlang::sym(column_name))
  
  # Filter rows with the specified hashtag
  df <- df %>%
    filter(!!rlang::sym(column_name) == hashtag)
  
  # Extract year and month from date
  df <- df %>%
    mutate(date = as.Date(date),  # Convert 'date' to Date object
           date_interval = floor_date(date, "30 days")) %>%  # Create 'date_interval' column
    group_by(date_interval) %>%
    summarise(sum_favourites_pos = sum(user_favourites[polarity_text > 0.1], na.rm = TRUE), # Compute sum of 'user_favourites' for negative tweets
              sum_favourites_neg = sum(user_favourites[polarity_text < -0.1], na.rm = TRUE), # Compute sum of 'user_favourites' for negative tweets
              sum_favourites_all = sum(user_favourites, na.rm = TRUE)) %>%  # Compute total sum of 'user_favourites'
    mutate(normalized_pos = sum_favourites_pos / sum_favourites_all)  %>%
    mutate(normalized_neg = sum_favourites_neg / sum_favourites_all)
  
  # Return the modified dataframe
  return(df)
}

misinformation = function(final_data){
  
  require(ggplot2)
  # Step 2: Sort the data frame by date
  library(dplyr)
  final_data <- final_data %>%
    arrange(date)
  
  # Step 3: Calculate the 10-day rolling average of 'misinformation' by date
  library(zoo)
  final_data <- final_data %>%
    group_by(date) %>%
    summarise(misinformation_avg = mean(misinformation)) %>%
    mutate(misinformation_avg_1 = rollmean(misinformation_avg, k = 10, fill = NA, align = "right", by = "1 day"))
  
  # Step 4: Create a line plot of the 1-day average over time
  graph <- ggplot(final_data, aes(x = date, y = misinformation_avg_1)) +
    geom_line() +
    labs(x = "Date", y = "10-day Average of Misinformation",
         title = "10-day Average of Misinformation Over Time")
  
  return(graph)
  
  
}


misinformation_month <- function(final_data) {
  require(ggplot2)
  # Step 2: Sort the data frame by date
  library(dplyr)
  final_data <- final_data %>%
    arrange(date)
  
  # Step 3: Calculate the 1-day rolling average of 'misinformation' by date
  library(zoo)
  final_data <- final_data %>%
    group_by(date) %>%
    summarise(misinformation_avg = mean(misinformation)) %>%
    mutate(misinformation_avg_1 = rollmean(misinformation_avg, k = 1, fill = NA, align = "right", by = "1 day"))
  
  # Step 4: Create a line plot of the 1-day average over time
  graph <- ggplot(final_data, aes(x = date, y = misinformation_avg_1)) +
    geom_line() +
    labs(x = "Date", y = "1-day Average of Misinformation",
         title = "1-day Average of Misinformation Over Time")
  
  return(graph)
}

favourites_month <- function(final_data) {
  require(ggplot2)
  # Step 2: Sort the data frame by date
  library(dplyr)
  final_data <- final_data %>%
    arrange(date)
  
  # Step 3: Calculate the 1-day rolling average of 'misinformation' by date
  library(zoo)
  final_data <- final_data %>%
    group_by(date) %>%
    summarise(favourites_avg = mean(user_favourites)) %>%
    mutate(favourites_avg_1 = rollmean(favourites_avg, k = 1, fill = NA, align = "right", by = "1 day"))
  
  # Step 4: Create a line plot of the 1-day average over time
  graph <- ggplot(final_data, aes(x = date, y = favourites_avg_1)) +
    geom_line() +
    labs(x = "Date", y = "1-day Average of Favourites",
         title = "1-day Average of Favourites Over Time")
  
  return(graph)
}

favourites <- function(final_data) {
  require(ggplot2)
  # Step 2: Sort the data frame by date
  library(dplyr)
  final_data <- final_data %>%
    arrange(date)
  
  # Step 3: Calculate the 10-day rolling average of 'misinformation' by date
  library(zoo)
  final_data <- final_data %>%
    group_by(date) %>%
    summarise(favourites_avg = mean(user_favourites)) %>%
    mutate(favourites_avg_10 = rollmean(favourites_avg, k = 10, fill = NA, align = "right", by = "1 day"))
  
  # Step 4: Create a line plot of the 10-day average over time
  graph <- ggplot(final_data, aes(x = date, y = favourites_avg_10)) +
    geom_line() +
    labs(x = "Date", y = "10-day Average of Favourites",
         title = "10-day Average of Favourites Over Time")
  
  return(graph)
}

ClusterAnalysis = function(data){
  require(stringi)
  require(dplyr)
  require(tm)
  data %>% dplyr::select(cleaned_text, polarity_text) %>% 
    mutate(WordCount = stri_count_words(cleaned_text))%>% 
    select(WordCount,polarity_text)
  
  negative_sentiments <- filter(data, polarity_text <0.1)
  positive_sentiments <- filter(data, polarity_text >0.1)
  
  negative_dtm <- DocumentTermMatrix(Corpus(VectorSource(negative_sentiments[,c("cleaned_text")])))
  negative_dtm <- removeSparseTerms(negative_dtm, 0.98)
  negative_clusters <- as.data.frame(as.matrix(negative_dtm))
  
  positive_dtm <- DocumentTermMatrix(Corpus(VectorSource(positive_sentiments[,c("cleaned_text")])))
  positive_dtm <- removeSparseTerms(positive_dtm, 0.98)
  positive_clusters <- as.data.frame(as.matrix(positive_dtm))
  
  cluster_list = list(negative_clusters,positive_clusters)
  
  return(cluster_list)
}

clusters_plot <- function(data) {
  require(EGAnet)
  plot = EGA(data, cor = "spearman")
  return(plot)
}
