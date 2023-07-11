# Load the dataset
set.seed(42)
# Get the directory of the R script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory
setwd(file.path(script_dir, "../data"))

data <- read.csv("vax_tweets_4.csv")
data$date <- as.Date(data$date)

# Find the minimum and maximum dates
min_date <- min(data$date)
max_date <- max(data$date)

# Generate monthly datasets and write to CSV
date_range <- seq(min_date, max_date, by = "month")
for (i in 1:(length(date_range) - 1)) {
  start_date <- date_range[i]
  end_date <- date_range[i+1] - 1
  subset_name <- paste0("data_", format(start_date, "%b"), "_", format(start_date, "%Y"))
  subset_data <- data[data$date >= start_date & data$date <= end_date, ]
  assign(subset_name, subset_data)
  write.csv(subset_data, file = paste0(subset_name, ".csv"), row.names = FALSE)
}