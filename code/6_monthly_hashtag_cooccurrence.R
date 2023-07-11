#This code produces the hashtag co-occurrence images in the results file
#Have to save them manually


library(tidyverse)
library(igraph)
library(Matrix)
set.seed(42)



# Get the directory of the R script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory
setwd(file.path(script_dir, "../data"))

source(file.path(script_dir, "functions.R"))

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
start_date <- ymd("2020-08-09")
end_date <- ymd("2022-09-14")

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


#For some reason cannot save graphs using function
#So have to save each one manually using export button!
# Iterate through the datasets
for (dataset_name in names(datasets)) {
  dataset_object <- datasets[[dataset_name]]
  
  # Generate the graph using hashtag_cooccurrence()
  hashtag_cooccurrence(dataset_object)
  print(dataset_name)
  
}
