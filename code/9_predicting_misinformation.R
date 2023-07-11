# Get the directory of the R script
library(dplyr)
set.seed(42)
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory
setwd(file.path(script_dir, "../data"))

data1 = read.csv("classified.csv")
data4 = read.csv("classified_1001_1500.csv")
data3 = read.csv("classified_750_1001.csv")
data3 = data3[,-1]
data2 = read.csv("classified_501_750.csv")

topics = read.csv("vax_tweets_topic_distribution_6.csv")
class(topics$date)
topics$date = as.Date(topics$date,format = "%d/%m/%y")

#data  goes from 1 - 500
#seems that data2 goes from 500 - 749 - so 500 repeated in data and data4
#data 3 goes from 750 - 1000
#data 4 goes from 1001 - 1500

data4 = data4[,c(1,3)]
data2 = data2[,c(1,3)]
data2 = data2[-1,]

data2 = data2[1:249,]

colnames(data2) = c("X", "misinformation")
colnames(data3) = c("X", "misinformation")
colnames(data4) = c("X", "misinformation")

data2$misinformation <- ifelse(data2$misinformation == 1, "True", "False")
data4$misinformation <- ifelse(data4$misinformation == 2, "True", "False")
tweets = read.csv("vax_tweets_5.csv")




topics_subset = topics[,c("X","dominant_topic")]

tweets = merge(topics_subset, tweets, by="X")
tweets$dominant_topic = as.factor(tweets$dominant_topic)
tweets$date = as.Date(tweets$date)

library(lubridate)
tweets <- tweets %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         day_of_week = wday(date, label = TRUE))

data = as.data.frame(rbind(data1, data2, data3, data4))

# Check for missing numbers in the "X" column
missing_numbers <- setdiff(1:100000, unique(tweets$X))


merged_data <- merge(data, tweets, by = "X")



merged_data$misinformation = as.factor(merged_data$misinformation)

# Assuming you have a merged dataset called merged_data with columns "misinformation" and other variables

# Set a random seed for reproducibility
set.seed(42)

# Split the data into training and testing sets using an 80:20 ratio
train_indices <- sample(nrow(merged_data), 0.8 * nrow(merged_data))
train_data <- merged_data[train_indices, ]
test_data <- merged_data[-train_indices, ]


# Create the formula for random forest, including the hashtag columns
formula <- as.formula(paste("misinformation ~ polarity_text + subjectivity_text + 
                        subjectivity_description + polarity_description + 
                        Locations + Symptoms + COVID +
                        Conspiracy +  Masks + origin + day + month + year"))


formula
# Fit a random forest model
model <- glm(formula, data = train_data, family = binomial)

# Make predictions on the test data
predictions <- predict(model, newdata = test_data, type = "response")

library(pROC)

# Compute the ROC curve
roc_obj <- roc(test_data$misinformation, predictions)

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve", print.auc = TRUE)

# Add labels and legend
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj), 2)), cex = 0.8)



# Find the observations in 'tweets' that do not have matching values in 'data$X'
validation <- anti_join(tweets, data, by = "X")

pred = predict(model, validation, type = "response")

validation$misinformation = pred

pred1 = predict(model, merged_data, type = "response")

merged_data$misinformation = pred1

final_data = rbind(merged_data, validation)
write.csv(final_data, "vax_tweets_6.csv")

# Add misinformation to the monthly datasets
library(dplyr)
final_data <- final_data %>%
  select(-starts_with("hashtag_"))
class(final_data$date)
final_data$date = as.Date(final_data$date)
min_date <- min(final_data$date)
max_date <- max(final_data$date)


# Generate monthly final_datasets and write to CSV
date_range <- seq(min_date, max_date, by = "month")
for (i in 1:(length(date_range) - 1)) {
  start_date <- date_range[i]
  end_date <- date_range[i+1] - 1
  subset_name <- paste0("data_", format(start_date, "%b"), "_", format(start_date, "%Y"))
  subset_final_data <- final_data[final_data$date >= start_date & final_data$date <= end_date, ]
  assign(subset_name, subset_final_data)
  write.csv(subset_final_data, file = paste0(subset_name, ".csv"), row.names = FALSE)
}
