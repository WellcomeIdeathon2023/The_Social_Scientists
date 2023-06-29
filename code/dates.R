data = read.csv("vax_tweets_cleaned_with_sentiment.csv")

library(lubridate)

data$date = gsub("\\ .*", "", data$date)

data$date = ymd(data$date)

write.csv(data, file = "vax_tweets_cleaned_with_sentiment_2.csv", row.names = F)

