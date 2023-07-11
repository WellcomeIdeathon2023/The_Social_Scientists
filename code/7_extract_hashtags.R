# Get the directory of the R script
set.seed(42)
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory
setwd(file.path(script_dir, "../data"))

data = read.csv("vax_tweets_4.csv")


library(tidyverse)
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

write.csv(data, "vax_tweets_5.csv", row.names = FALSE)

