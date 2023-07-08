library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(igraph)
library(Matrix)
library(tidyverse)
library(networkD3)
library(htmlwidgets)
library(visNetwork)
library(here)




# Get the directory of the R script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory
setwd(file.path(script_dir, "../data"))

# setwd("D:/Programming/Projects/The_Social_Scientists/The_Social_Scientists/data")


#setwd("D:/Programming/Projects/The_Social_Scientists/The_Social_Scientists/data")

# Source the "functions.R" script
source("functions.R")

# Load the categories list from the RDS file
categories <- readRDS("categories.rds")

# List all CSV files in the working directory
csv_files <- list.files(pattern = "*.csv")

# Load the CSV datasets into separate objects
for (file in csv_files) {
  # Extract the month and year from the file name
  month_year <- gsub("data_|.csv", "", file)
  
  # Read the CSV file and assign it to a named object using the month-year as the name
  assign(paste0("data_", month_year), read.csv(file))
}

# Get global dataset to use for global trends tab
global_trends <- read.csv("vax_tweets_6.csv")
global_trends$date = as.Date(global_trends$date)

# Apply the function to the dataframe
gt2 <- analyze_hashtag(global_trends, "hashtags", "'vaccine'")

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        .custom-image {
          width: 600px;
          height: 400px;
        }
      ")
    )
  ),
  titlePanel("Monthly Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_month",
        label = "Select a month:",
        choices = format(seq(as.Date("2020-01-01"), as.Date("2022-11-01"), by = "month"), format = "%b-%Y"),
        selected = format(as.Date("2021-06-01"), format = "%b-%Y")
      ),
      checkboxInput(
        inputId = "pharma_checkbox",
        label = "Pharma",
        value = FALSE
      ),
      checkboxInput(
        inputId = "politics_checkbox",
        label = "Politics",
        value = FALSE
      ),
      verbatimTextOutput("warning") 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Categories",
          plotOutput("barplot_categories")
        ),
        tabPanel(
          "Sentiment and Misinformation - Selected Month",
          plotOutput("sentiment_plot"),
          plotOutput("misinformation_month")
        ),
        tabPanel(
          "Sentiment and Misinformation - All time",
          plotOutput("alltime_trends"),
          plotOutput("misinformation"),
          plotOutput("boxplots")
        ),
        tabPanel(
          "Hashtags",
          imageOutput("co_occurrence_plot"),
          textInput(inputId = "input_hashtag", label = "Enter a hashtag:", value = ""),
          plotOutput("hashtag_plot"),
        ),
      )
    )
  )
)



server <- function(input, output) { 
  
  # Create reactive values for storing the selected month and mean misinformation
  current_month <- reactiveVal(0)
  previous_month <- reactiveVal(0)
  selected_month <- reactiveVal(format(as.Date("2021-06-01"), format = "%b-%Y"))  # Initial selected month
  
  observeEvent(input$selected_month, {
    # Update the selected month value
    selected_month(input$selected_month)
    
    # Store the current month value
    selected_date <- parse_date_time(input$selected_month, "b-%Y")
    current_data <- global_trends[format(global_trends$date, "%b-%Y") == input$selected_month, ]
    current_mean <- mean(current_data$misinformation)
    current_month(current_mean)
    
    # Calculate the previous month value
    previous_date <- selected_date %m-% months(1)
    previous_month_data <- global_trends[format(global_trends$date, "%b-%Y") == format(previous_date, "%b-%Y"), ]
    previous_mean <- mean(previous_month_data$misinformation)
    previous_month(previous_mean)
    
    # Calculate confidence interval for the previous month mean
    previous_ci <- t.test(previous_month_data$misinformation)$conf.int
    
    # Compare current month mean with confidence interval for previous month mean and display warning
    output$warning <- renderText({
      if (current_month() > previous_ci[2]) {
        "Misinformation high!"
      } else {
        "Misinformation unchanged"
      }
    })
  })
    
  output$boxplots <- renderPlot({
    # Convert Month-Year labels to Date class and define custom order
    global_trends$MonthYear <- as.Date(paste0("01-", format(global_trends$date, "%b-%Y")), format = "%d-%b-%Y")
    custom_order <- format(seq(as.Date("2020-01-01"), as.Date("2022-11-01"), by = "month"), format = "%b-%Y")
    
    # Create boxplots for each month of misinformation values
    p <- ggplot(global_trends, aes(x = factor(format(date, "%b-%Y"), levels = custom_order, ordered = TRUE), y = misinformation)) +
      geom_boxplot(fill = ifelse(format(date, "%b-%Y") == selected_month() && current_month() > previous_month(), "red", "gray")) +
      labs(x = "Month-Year", y = "Misinformation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    p
  })
  
  
  
  # Generate the plots
  output$barplot_categories <- renderPlot({
    category_counts <- processEntitiesData(input$selected_month)
    
    ggplot(category_counts, aes(x = reorder(Category, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "Category", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$misinformation_month <- renderPlot({
    selected_date <- parse_date_time(input$selected_month, "b-%Y")
    selected_data <- global_trends[format(global_trends$date, "%b_%Y") == format(selected_date, "%b_%Y"), ]
    
    misinfo_graph <- misinformation_month(selected_data)
    misinfo_graph
  })
  
  output$sentiment_plot <- renderPlot({
    # Load the necessary data (replace with your own data loading code)
    selected_date <- parse_date_time(input$selected_month, "b-y")
    file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
    data <- get(file_name)
    
    # Compute the sums every five days
    fiveday_sums <- compute_sums_every_one_day(data)
    
    # Label high periods
    fiveday_sums <- label_high_periods(fiveday_sums)
    
    # Create a new data frame for high_neg == TRUE
    high_neg_dates <- fiveday_sums %>%
      filter(high_neg == TRUE) %>%
      select(date_interval)
    
    # Plot the sentiment over time
    ggplot(fiveday_sums, aes(x = date_interval)) +
      geom_line(aes(y = normalized_pos), color = 'blue') +
      geom_line(aes(y = normalized_neg), color = 'red') +
      geom_vline(
        data = high_neg_dates,
        aes(xintercept = as.numeric(date_interval)),
        linetype = "dashed",
        color = "black",
        size = 0.5
      ) +
      labs(
        title = "Normalized Sentiment Over Time",
        x = "Date Interval",
        y = "Normalized Sentiment Score",
        color = "Legend"
      ) +
      scale_color_manual(
        values = c("blue", "red"),
        labels = c("Positive", "Negative")
      ) +
      theme_minimal()
  })
  
  
  
  output$co_occurrence_plot <- renderImage({
    # Construct the file path for the image
    selected_date <- parse_date_time(input$selected_month, "b-y")
    image_file <- paste0(format(selected_date, format = "%b_%Y"), ".png")
    image_path <- file.path(getwd(), image_file)
    
    # Return a list with the src and alt attributes
    list(src = image_path, alt = "Co-occurrence Plot", class = "custom-image")
  }, deleteFile = FALSE)
  
  
  output$misinformation = renderPlot({
    misinfo_graph = misinformation(global_trends)
    misinfo_graph
  })
  
  output$negative_hashtags_table <- renderTable({
    # Load the necessary data (replace with your own data loading code)
    selected_date <- parse_date_time(input$selected_month, "b-y")
    file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
    data <- get(file_name)
    
    # Get the most common hashtags associated with negative sentiment
    timeframe_start <- "2020-01-01"
    timeframe_end <- "2020-06-30"
    get_negative_hashtags(data, timeframe_start, timeframe_end)
  })
  
  output$correlated_negative_hashtags_table <- renderTable({
    # Load the necessary data (replace with your own data loading code)
    selected_date <- parse_date_time(input$selected_month, "b-y")
    file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
    data <- get(file_name)
    
    # Get hashtags with the highest negative correlation
    timeframe_start <- "2020-01-01"
    timeframe_end <- "2020-06-30"
    get_correlated_negative_hashtags(data, timeframe_start, timeframe_end)
  })
  
  output$alltime_trends <- renderPlot({
    # Load the necessary data (replace with your own data loading code)
    data <- global_trends
    
    # Compute the sums every five days
    fiveday_sums <- compute_sums_every_thirty_days(data)
    
    # Label high periods
    fiveday_sums <- label_high_periods(fiveday_sums)
    
    # Create a new data frame for high_neg == TRUE
    high_neg_dates <- fiveday_sums %>%
      filter(high_neg == TRUE) %>%
      select(date_interval,normalized_neg,sum_favourites_neg)
    
    # Plot the sentiment over time
    ggplot(fiveday_sums, aes(x = date_interval)) +
      geom_line(aes(y = normalized_pos), color = 'blue') +
      geom_line(aes(y = normalized_neg), color = 'red') +
      geom_point(
        data = high_neg_dates,
        aes(x = date_interval, y = normalized_neg),
        color = "black",
        size = 3
      ) +
      geom_text(
        data = high_neg_dates,
        aes(x = date_interval, y = normalized_neg),
        label = "Negative Peak",
        vjust = -1,
        hjust = 0.2,
        size = 3,
        color = "black"
      ) +
      labs(
        title = "Normalized Sentiment Over Time",
        x = "Date Interval",
        y = "Normalized Sentiment Score",
        color = "Legend"
      ) +
      scale_x_date(date_breaks = "3 months", date_labels = "%,b %Y") +
      scale_color_manual(
        values = c("blue", "red"),
        labels = c("Positive", "Negative","All")
      ) +
      theme_minimal()
  })
  
  
  output$hashtag_plot <- renderPlot({
    if (input$input_hashtag != "") {
      hashtag_data <- analyze_hashtag(global_trends, "hashtags", paste0("'",input$input_hashtag,"'"))
      # Plot the sentiment over time
      ggplot(hashtag_data, aes(x = date_interval)) +
        geom_line(aes(y = sum_favourites_pos), color = 'blue') +
        geom_line(aes(y = sum_favourites_neg), color = 'red') +
        geom_line(aes(y = sum_favourites_all), color = 'black') +
        labs(
          title = "Normalized Sentiment Over Time",
          x = "Date Interval",
          y = "Number of Tweets",
          color = "Legend"
        ) +
        scale_x_date(date_breaks = "3 months", date_labels = "%,b %Y") +
        scale_color_manual(
          values = c("blue", "red","black"),
          labels = c("Positive", "Negative","All")
        ) +
        theme_minimal()
    }
  })
  
  # Helper function to process the data and generate the category_counts
  processEntitiesData <- function(selected_month) {
    selected_date <- parse_date_time(selected_month, "b-y")
    file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
    data <- get(file_name)
    
    # Create an empty dataframe
    variable_sums <- colSums(data[, 17:34])
    
    category_counts <- data.frame(Category = names(variable_sums), Count = variable_sums)
    
    return(category_counts)
  }
}


# Run the app
shinyApp(ui = ui, server = server)

