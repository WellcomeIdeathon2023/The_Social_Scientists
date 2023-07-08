# Get the directory of the R script
library(dplyr)
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory
setwd(file.path(script_dir, "../data"))

data = read.csv("classified.csv")
tweets = read.csv("vax_tweets_5.csv")


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

# Load the randomForest package
library(randomForest)

# Get the column names that start with "hashtag_"
hashtag_columns <- grep("^hashtag_", colnames(tweets), value = TRUE)

# Create the formula for random forest, including the hashtag columns
formula <- as.formula(paste("misinformation ~ polarity_text + subjectivity_text + subjectivity_description + polarity_description + Organizations + Locations + Symptoms + COVID + Vaccination + Politics +
                        Conspiracy + Slurs + Masks + Miscellaneous + origin + 
                            vaccine_conspiracy + government + pharma + Five_G + 
                            gates + nwo + media +", paste(hashtag_columns, collapse = " + "), collapse = " + "))


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