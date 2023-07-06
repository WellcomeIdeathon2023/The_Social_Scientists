library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

setwd("C:/Users/s1723280/Documents/GitHub/The_Social_Scientists/The_Social_Scientists/data")

# Load the categories list from the RDS file
categories <- readRDS("categories (3).rds")

# List all CSV files in the working directory
csv_files <- list.files(pattern = "*.csv")

# Load the CSV datasets into separate objects
for (file in csv_files) {
  # Extract the month and year from the file name
  month_year <- gsub("data_|.csv", "", file)
  
  # Read the CSV file and assign it to a named object using the month-year as the name
  assign(paste0("data_", month_year), read.csv(file))
}

# Define UI
ui <- fluidPage(
  titlePanel("Monthly Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_month",
        label = "Select a month:",
        choices = format(seq(as.Date("2020-01-01"), as.Date("2022-11-01"), by = "month"), format = "%b-%Y"),
        selected = format(as.Date("2020-01-01"), format = "%b-%Y")
      )
    ),
    mainPanel(
      plotOutput("barplot_categories")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Generate the plots
  output$barplot_categories <- renderPlot({
    category_counts <- processEntitiesData(input$selected_month)
    
    ggplot(category_counts, aes(x = reorder(Category, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Category", y = "Count") +  # Add axis labels
      theme_minimal() +  # Use a minimal theme
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
  })
  
  # Helper function to process the data and generate the entity_category_df
  processEntitiesData <- function(selected_month) {
    selected_date <- parse_date_time(selected_month, "b-y")
    file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
    data <- get(file_name)
    
    # Create an empty dataframe
    variable_sums <- colSums(data[,17:34])
    
    category_counts <- data.frame(Category = names(variable_sums), Count = variable_sums)
    
    return(category_counts)
  }
}

# Run the app
shinyApp(ui = ui, server = server)
