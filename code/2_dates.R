# Get the directory of the R script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory
setwd(file.path(script_dir, "../data"))

data = read.csv("vax_tweets_cleaned_with_sentiment.csv")

library(lubridate)

data$date = gsub("\\ .*", "", data$date)

data$date = as.Date(data$date)

write.csv(data, file = "vax_tweets_cleaned_with_sentiment_2.csv", row.names = F)

