library(tidyverse)
library(igraph)
library(Matrix)

# Get the directory of the R script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory
setwd(file.path(script_dir, "../data"))

source("functions.R")

# List all CSV files in the working directory
csv_files <- list.files(pattern = "*.csv")

# Load the CSV datasets into separate objects
for (file in csv_files) {
  # Extract the month and year from the file name
  month_year <- gsub("data_|.csv", "", file)
  
  # Read the CSV file and assign it to a named object using the month-year as the name
  assign(paste0("data_", month_year), read.csv(file))
}


# Load the lubridate package for date manipulation
library(lubridate)

# Define the start and end dates
start_date <- ymd("2020-01-01")
end_date <- ymd("2022-11-01")

# List to store the dataset objects
datasets <- list()

# Generate the dataset objects
current_date <- start_date
while (current_date <= end_date) {
  dataset_name <- paste0("data_", format(current_date, "%b_%Y"))
  dataset_object <- get(dataset_name)  # Assuming dataset objects are stored with the same name
  datasets[[dataset_name]] <- dataset_object
  current_date <- current_date + months(1)
}


# Iterate through the datasets
for (dataset_name in names(datasets)) {
  dataset_object <- datasets[[dataset_name]]
  
  # Generate the graph using hashtag_cooccurrence()
  hashtag_cooccurrence(dataset_object)
  
}
