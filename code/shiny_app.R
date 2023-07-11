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
library(stringi)
library(tm)
library(EGAnet)
library(shinythemes)
library(sna)
library(RColorBrewer)

set.seed(42)

# Get the directory of the R script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory
setwd(file.path(script_dir, "../data"))

# Source the "functions.R" script
source(file.path(script_dir, "functions.R"))

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
global_trends = global_trends[,-1]

global_trends$dominant_topic = as.factor(global_trends$dominant_topic)


simulated = read.csv("simulated data.csv")

simulated$date = as.Date(simulated$date, format = "%d/%m/%Y")
simulated = simulated[-(36:39),]
simulated = simulated[,-(4:6)]

# Sort the dataframe by date in ascending order
simulated <- simulated[order(simulated$date), ]

# Calculate the change in antivax count
simulated$antivax_change <- c(diff(simulated$antivaccine), NA)

#Get data with sentiments for clustering
sentiment_clusters <- read.csv("vax_tweets_with_sentiment_entities_3.csv")
sentiment_clusters$date <- as.Date(sentiment_clusters$date)

# Apply the function to the dataframe
gt2 <- analyze_hashtag(global_trends, "hashtags", "'vaccine'")

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
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
  titlePanel("Vaccine Misinformation Detector"),
  sidebarLayout(
    sidebarPanel(
      actionButton("stopBtn", "Stop App"),
      selectInput(
        inputId = "selected_month",
        label = "Select a month:",
        choices = format(seq(as.Date("2020-08-09"), as.Date("2022-09-14"), by = "month"), format = "%b-%Y"),
        selected = format(as.Date("2021-06-01"), format = "%b-%Y")
      ),
      tags$p("Misinformation Alert", style = "font-weight: bold;"),
      verbatimTextOutput("warning"),
      sliderInput("misinfoSlider", "Misinformation Proportion:",
                  min = 0, max = 1, step = 0.01, value = 0),
      tags$p("Choose Category", style = "font-weight: bold;"),
      checkboxInput(
        inputId = "organisations_checkbox",
        label = "Organisations",
        value = FALSE
      ),
      checkboxInput(
        inputId = "locations_checkbox",
        label = "Locations",
        value = FALSE
      ),
      checkboxInput(
        inputId = "symptoms_checkbox",
        label = "Symptoms",
        value = FALSE
      ),
      checkboxInput(
        inputId = "covid_checkbox",
        label = "COVID",
        value = FALSE
      ),
      checkboxInput(
        inputId = "vaccination_checkbox",
        label = "Vaccination",
        value = FALSE
      ),
      checkboxInput(
        inputId = "politics_checkbox",
        label = "Politics",
        value = FALSE
      ),
      checkboxInput(
        inputId = "conspiracy_checkbox",
        label = "Conspiracy",
        value = FALSE
      ),
      checkboxInput(
        inputId = "slurs_checkbox",
        label = "Slurs",
        value = FALSE
      ),
      checkboxInput(
        inputId = "masks_checkbox",
        label = "Masks",
        value = FALSE
      ),
      checkboxInput(
        inputId = "origin_checkbox",
        label = "Origin",
        value = FALSE
      ),
      checkboxInput(
        inputId = "vaccine_conspiracy_checkbox",
        label = "Vaccine Conspiracy",
        value = FALSE
      ),
      checkboxInput(
        inputId = "government_checkbox",
        label = "Government",
        value = FALSE
      ),
      checkboxInput(
        inputId = "pharma_checkbox",
        label = "Pharma",
        value = FALSE
      ),
      checkboxInput(
        inputId = "five_g_checkbox",
        label = "Five_G",
        value = FALSE
      ),
      checkboxInput(
        inputId = "gates_checkbox",
        label = "Gates",
        value = FALSE
      ),
      checkboxInput(
        inputId = "nwo_checkbox",
        label = "NWO",
        value = FALSE
      ),
      checkboxInput(
        inputId = "media_checkbox",
        label = "Media",
        value = FALSE
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions",
                 # Use tags to include formatted text in the tab
                 tags$div(
                   tags$h3("Welcome to the Misinformation Detector"),
                   tags$p("This app allows you to analyze and visualize data related COVID19 vaccine related information and misinformation"),
                   tags$p("To use the app, follow these steps:"),
                   tags$ol(
                     tags$li("Step 1: Select the desired month using the dropdown menu."),
                     tags$li("Step 2: Subset the data using the misinformation predicted probability"),
                     tags$li("Step 3: Filter by specified categories using the checkbox."),
                   ),
                   tags$p("The app will output a warning if misinformation is significantly higher than the previous month"),
                   tags$p("We also offer visualisation related to hashtags, including co-occurrences"),
                 ),
                 tags$div(
                   tags$h3("How it works:"),
                   tags$p("Our app uses Machine Learning to predict the probability that any given tweet is misinformation"),
                   tags$p("We aggregregate various visualisations related to our results to offer advice as to where intereventions will be effective"),
                   tags$p("We predict the anti-vax sentiment for the next month based on the current misinformation levels")
                 ),
                 tags$div(
                   tags$h3("What is misinformation probability?"),
                   tags$p("Each tweet has been identified by out model as having a certain probability (0 to 1) of being misinformation. By adjusting the slider, you can subset the data to only include tweets with a misinformation probability ABOVE the chosen amount"),
                 ),
                 tags$div(
                   tags$h3("The Alert Sytem"),
                   tags$p("On the left, you will see a misinformation alert for the chosen month"),
                   tags$p("If misinformation is HIGHER than the previous month (to a statistically significant level), then you will be alerted to this"),
                 )
        ),
        tabPanel(
          "Misinformation",
          tags$h3("Average probability of misinformation for the selected month"),
          plotOutput("misinformation_month"),
          tags$h3("Average probability of misinformation"),
          plotOutput("misinformation"),
          tags$h3("Density plot of misinformation probabiltiies per month"),
          plotOutput("density_plot"),
          tags$h3("Distribution of probability of misinformation per month"),
          plotOutput("boxplots")
        ),
        tabPanel(
          "Categories",
          tags$p("We conducted named entity recognition to discover the named entities in each tweet. We categorised them according to a dictionary, and display the counts for the current settings. If the proportion of a category is higher than the previous month, then it displays in RED."),
          tags$h3("Most common categorised entities during the selected month"),
          plotOutput("barplot_categories"),
          htmlOutput("category_increase_text"),
          htmlOutput("hashtags_intervene")
        ),
        tabPanel(
          "Sentiment",
          tags$p("We conducted sentiment analysis for each tweet. For example: If a tweet is 'I am VERY happy about the Spurs score!', then it would be a positive sentiment (even if slightly unrealistic!)"),
          tags$h3("All time sentiment changes"),
          tags$p("Blue = positive, Red = negative"),
          plotOutput("alltime_trends"),
          tags$h3("Sentiment change during the selected month"),
          tags$p("Blue = positive, Red = negative"),
          plotOutput("sentiment_plot"),
        ),
        tabPanel(
          "Topics",
          tags$p("We conducted topic modelling (which we interpreted), and assigned to each tweet the most dominant topic. We display these topic counts over time for the given settings."),
          plotOutput("topics_all")
        ),
        # tabPanel(
        #   "Word Clusters",
        #   tags$h3("All time negative sentiment clusters"),
        #   plotOutput("sentiment_neg_clusters"),
        #   
        #   tags$h3("Monthly negative sentiment clusters"),
        #   plotOutput("sentiment_neg_clusters_monthly"),
        #   
        #   tags$h3("All time positive sentiment clusters"),
        #   plotOutput("sentiment_pos_clusters"),
        #   
        #   tags$h3("Monthly positive sentiment clusters"),
        #   plotOutput("sentiment_pos_clusters_monthly"),
        # ),
        tabPanel(
          "Hashtags",
          tags$h3("Hashtag counts for the given settings"),
          plotOutput("hashtags_by_category"),
          plotOutput("hashtags_by_category_all"),
          tags$h3("Co-occurrence of the most common hashtags during the selected month"),
          imageOutput("co_occurrence_plot"),
          tags$h3("Sentiment by hashtag"),
          tags$p("For a chosen hashtag, we will show you the sentiments of tweets with that hashtag over time."),
          textInput(inputId = "input_hashtag", label = "Enter a hashtag:", value = ""),
          plotOutput("hashtag_plot"),
          tags$p("Blue = positive, Red = negative, Black = all"),
        ),
        tabPanel(
          "Favourites",
          tags$h3("Average number of favourites for the given settings and selected month"),
          plotOutput("favourites_month"),
          tags$h3("Average number of favourites for the given settings"),
          plotOutput("favourites"),
        ),
        tabPanel(
          "Vaccine Sentiment",
          tags$h3("Surveyed vaccine sentiment and misinformation level"),
          tags$p("Here we show the surveyed anti-vaccination sentiment against the current misinformation level"),
          tags$p("A linear regression model predicts the change in the anti-vaccination sentiment for the upcoming month"),
          plotOutput("survey"),
          htmlOutput("predict_antivax")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Reactive value to track the state of the app
  appRunning <- reactiveVal(TRUE)
  
  # Function to stop the app
  stopAppFunc <- function() {
    appRunning(FALSE)
    stopApp()
  }
  
  # Handler for the stop button
  observeEvent(input$stopBtn, {
    stopAppFunc()
  })
  
  # Run the app until the appRunning value is FALSE
  observe({
    if (!appRunning()) {
      stopAppFunc()
    }
    
    # Create reactive values for storing the selected month and mean misinformation
    current_month <- reactiveVal(0)
    previous_month <- reactiveVal(0)
    selected_month <- reactiveVal(format(as.Date("2020-06-01"), format = "%b-%Y"))  # Initial selected month
    
    # Load the ggplot2 library
    library(ggplot2)
    
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
      
      output$topics_all = renderPlot({
        
        data = global_trends
        
        data <- data[data$misinformation >= input$misinfoSlider,]
        
        # Check if "Organisations" checkbox is checked
        if (input$organisations_checkbox) {
          data <- data[data$Organizations > 0, ]
        }
        
        # Check if "Locations" checkbox is checked
        if (input$locations_checkbox) {
          data <- data[data$Locations > 0, ]
        }
        
        # Check if "Symptoms" checkbox is checked
        if (input$symptoms_checkbox) {
          data <- data[data$Symptoms > 0, ]
        }
        
        # Check if "COVID" checkbox is checked
        if (input$covid_checkbox) {
          data <- data[data$COVID > 0, ]
        }
        
        # Check if "Vaccination" checkbox is checked
        if (input$vaccination_checkbox) {
          data <- data[data$Vaccination > 0, ]
        }
        
        # Check if "Politics" checkbox is checked
        if (input$politics_checkbox) {
          data <- data[data$Politics > 0, ]
        }
        
        # Check if "Conspiracy" checkbox is checked
        if (input$conspiracy_checkbox) {
          data <- data[data$Conspiracy > 0, ]
        }
        
        # Check if "Slurs" checkbox is checked
        if (input$slurs_checkbox) {
          data <- data[data$Slurs > 0, ]
        }
        
        # Check if "Masks" checkbox is checked
        if (input$masks_checkbox) {
          data <- data[data$Masks > 0, ]
        }
        
        # Check if "Origin" checkbox is checked
        if (input$origin_checkbox) {
          data <- data[data$origin > 0, ]
        }
        
        # Check if "Vaccine Conspiracy" checkbox is checked
        if (input$vaccine_conspiracy_checkbox) {
          data <- data[data$vaccine_conspiracy > 0, ]
        }
        
        # Check if "Government" checkbox is checked
        if (input$government_checkbox) {
          data <- data[data$government > 0, ]
        }
        
        # Check if "Pharma" checkbox is checked
        if (input$pharma_checkbox) {
          data <- data[data$pharma > 0, ]
        }
        
        # Check if "Five_G" checkbox is checked
        if (input$five_g_checkbox) {
          data <- data[data$Five_G > 0, ]
        }
        
        # Check if "Gates" checkbox is checked
        if (input$gates_checkbox) {
          data <- data[data$gates > 0, ]
        }
        
        # Check if "NWO" checkbox is checked
        if (input$nwo_checkbox) {
          data <- data[data$nwo > 0, ]
        }
        
        # Check if "Media" checkbox is checked
        if (input$media_checkbox) {
          data <- data[data$media > 0, ]
        }
        # Create a new variable for the month and year
        data$month_year <- format(data$date, "%Y-%m")
        
        # Count the occurrences of each factor level in dominant_topic for each month
        topic_counts <- data %>%
          group_by(month_year, dominant_topic) %>%
          count() %>%
          ungroup()
        
        # Generate a color palette with 11 distinct colors
        my_colors <- rainbow(length(unique(topic_counts$dominant_topic)))
        
        # Plotting
        ggplot(topic_counts, aes(x = month_year, y = n, fill = dominant_topic)) +
          geom_bar(stat = "identity") +
          labs(x = "Month", y = "Count", title = "Monthly Count of Dominant Topics") +
          scale_fill_manual(values = my_colors, labels = levels(topic_counts$dominant_topic)) +
          theme_minimal() +
          theme(legend.title = element_blank(),
                axis.text.x = element_text(angle = 90, hjust = 1))
        
      })
      
      output$survey = renderPlot({
        ggplot(simulated, aes(x = date)) +
          geom_line(aes(y = antivaccine, color = "Antivaccine")) +
          geom_line(aes(y = misinformation, color = "Misinformation")) +
          scale_color_manual(values = c("blue", "red"), 
                             labels = c("Against Vaccination", "Misinformation percentage")) +
          labs(x = "Date", y = "Percentage (%)") +
          ggtitle("Anti-vaccine and Misinformation Trends") +
          theme_minimal() +
          theme(legend.position = "top", 
                legend.justification = "right")
      })
      
      output$predict_antivax = renderText({
        
        
        model = lm(antivax_change ~ misinformation ,data = simulated)
        
        selected_month <- input$selected_month
        selected_date <- as.Date(paste0("01-", selected_month), format = "%d-%b-%Y")
        
        # Create a new dataframe with the selected month's data
        selected_data <- subset(simulated, format(date, "%b-%Y") == format(selected_date, "%b-%Y"))
        
        # Check if data for the selected month exists
        if (nrow(selected_data) > 0) {
          # Format the predicted change as a percentage
          predicted_change <- predict(model, newdata = selected_data)
          predicted_change_percentage <- predicted_change * 100
          
          output_text <- paste("Predicted change in anti-vaccination sentiment for ", selected_month, ": ", 
                               ifelse(predicted_change_percentage >= 0, "+", "-"), 
                               "<span style='color: red;'>", abs(round(predicted_change_percentage, 2)), "%</span>", sep = "")
          
          
          
          # Return the output text with HTML tags
          HTML(output_text)
        } else {
          # Output a message if data for the selected month is not available
          output_text <- paste("Data not available for", selected_month)
          
          # Return the output text with HTML tags
          HTML(output_text)
        }
      })
      
      # Create a density plot comparing the current and previous month
      output$density_plot <- renderPlot({
        # Filter selected data based on the checkboxes
        if (input$organisations_checkbox) {
          current_data <- current_data[current_data$Organisations > 0, ]
        }
        if (input$locations_checkbox) {
          current_data <- current_data[current_data$Locations > 0, ]
        }
        if (input$symptoms_checkbox) {
          current_data <- current_data[current_data$Symptoms > 0, ]
        }
        if (input$covid_checkbox) {
          current_data <- current_data[current_data$COVID > 0, ]
        }
        if (input$vaccination_checkbox) {
          current_data <- current_data[current_data$Vaccination > 0, ]
        }
        if (input$politics_checkbox) {
          current_data <- current_data[current_data$Politics > 0, ]
        }
        if (input$conspiracy_checkbox) {
          current_data <- current_data[current_data$Conspiracy > 0, ]
        }
        if (input$slurs_checkbox) {
          current_data <- current_data[current_data$Slurs > 0, ]
        }
        if (input$masks_checkbox) {
          current_data <- current_data[current_data$Masks > 0, ]
        }
        if (input$origin_checkbox) {
          current_data <- current_data[current_data$origin > 0, ]
        }
        if (input$vaccine_conspiracy_checkbox) {
          current_data <- current_data[current_data$vaccine_conspiracy > 0, ]
        }
        if (input$government_checkbox) {
          current_data <- current_data[current_data$government > 0, ]
        }
        if (input$pharma_checkbox) {
          current_data <- current_data[current_data$pharma > 0, ]
        }
        if (input$five_g_checkbox) {
          current_data <- current_data[current_data$Five_G > 0, ]
        }
        if (input$gates_checkbox) {
          current_data <- current_data[current_data$gates > 0, ]
        }
        if (input$nwo_checkbox) {
          current_data <- current_data[current_data$nwo > 0, ]
        }
        if (input$media_checkbox) {
          current_data <- current_data[current_data$media > 0, ]
        }
        
        
        # Plot the density of misinformation for the selected data
        plot1 <- ggplot(current_data, aes(x = misinformation)) +
          geom_density(fill = "blue", alpha = 0.5) +
          scale_x_continuous(limits = c(0, 1)) +
          geom_vline(aes(xintercept = current_mean, color = "Current Month"), linetype = "dashed") +
          labs(
            title = paste0("Density of Misinformation - ", input$selected_month),
            x = "Misinformation",
            y = "Density"
          ) +
          scale_color_manual(values = "red", guide = guide_legend(title = "Mean")) +
          theme_minimal()
        
        # Filter previous month selected data based on the checkboxes
        if (input$organisations_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$Organizations > 0, ]
        }
        if (input$locations_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$Locations > 0, ]
        }
        if (input$symptoms_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$Symptoms > 0, ]
        }
        if (input$covid_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$COVID > 0, ]
        }
        if (input$vaccination_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$Vaccination > 0, ]
        }
        if (input$politics_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$Politics > 0, ]
        }
        if (input$conspiracy_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$Conspiracy > 0, ]
        }
        if (input$slurs_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$Slurs > 0, ]
        }
        if (input$masks_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$Masks > 0, ]
        }
        if (input$origin_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$origin > 0, ]
        }
        if (input$vaccine_conspiracy_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$vaccine_conspiracy > 0, ]
        }
        if (input$government_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$government > 0, ]
        }
        if (input$pharma_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$pharma > 0, ]
        }
        if (input$five_g_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$Five_G > 0, ]
        }
        if (input$gates_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$gates > 0, ]
        }
        if (input$nwo_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$nw0 > 0, ]
        }
        if (input$media_checkbox) {
          previous_month_data <- previous_month_data[previous_month_data$media > 0, ]
        }
        
        # Add similar filtering for other checkboxes...
        
        
        # Plot the density of misinformation for the previous month selected data
        plot2 <- ggplot(previous_month_data, aes(x = misinformation)) +
          geom_density(fill = "green", alpha = 0.5) +
          scale_x_continuous(limits = c(0, 1)) +
          geom_vline(aes(xintercept = previous_mean, color = "Previous Month"), linetype = "dashed") +
          labs(
            title = paste0("Density of Misinformation - ", format(previous_date, "%b-%Y")),
            x = "Misinformation",
            y = "Density"
          ) +
          scale_color_manual(values = "red", guide = guide_legend(title = "Mean")) +
          theme_minimal()
        
        # Combine the two density plots using grid.arrange
        library(gridExtra)
        grid.arrange(plot1, plot2, ncol = 2)
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
    
    
    processEntitiesData <- function(selected_month) {
      selected_date <- parse_date_time(selected_month, "b-y")
      file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
      data <- get(file_name)
      
      data <- data %>%
        select(-starts_with("hashtag_"))
      
      data <- data[data$misinformation >= input$misinfoSlider,]
      
      # Calculate category counts and observations for selected month
      variable_sums <- colSums(data[, 19:36])
      observations <- nrow(data)
      
      # Create data frame for selected month
      category_counts <- data.frame(Category = names(variable_sums), Count = variable_sums, Observations = observations)
      
      # Calculate proportions for selected month
      category_counts$Proportion <- category_counts$Count / category_counts$Observations
      
      # Calculate category counts and observations for previous month
      previous_month <- selected_date %m-% months(1)
      previous_file_name <- paste0("data_", format(previous_month, format = "%b_%Y"))
      
      previous_data <- get(previous_file_name)
      previous_data <- previous_data %>%
        select(-starts_with("hashtag_"))
      previous_data <- previous_data[previous_data$misinformation >= input$misinfoSlider,]
        
      previous_variable_sums <- colSums(previous_data[, 19:36])
      previous_observations <- nrow(previous_data)
        
      # Create data frame for previous month
      previous_category_counts <- data.frame(Category = names(previous_variable_sums), Count = previous_variable_sums, Observations = previous_observations)
        
      # Calculate proportions for previous month
      previous_category_counts$Proportion <- previous_category_counts$Count / previous_category_counts$Observations
        
      # Calculate difference in proportions
      category_counts$Difference <- category_counts$Proportion - previous_category_counts$Proportion
        
      # Determine color based on difference in proportions
      category_counts$Color <- ifelse(category_counts$Difference > 0, "red", "steelblue")
        
      return(category_counts)
    }
    
    
    
    output$barplot_categories <- renderPlot({
      category_counts <- processEntitiesData(input$selected_month)
      

      ggplot(category_counts, aes(x = reorder(Category, -Count), y = Count, fill = Color)) +
        geom_bar(stat = "identity") +
        labs(x = "Category", y = "Count") +
        scale_fill_manual(values = c("steelblue" = "steelblue", "red" = "red"),
                          labels = c("Increased", "Decreased")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$category_increase_text <- renderText({
      category_counts <- processEntitiesData(input$selected_month)
      
      increased_categories <- category_counts$Category[category_counts$Difference > 0]
      if (length(increased_categories) > 0) {
        text <- paste0("<strong>The categories which have increased this month are:</strong><br>",
                       paste(increased_categories, collapse = ", "))
      } else {
        text <- "<em>No categories have increased this month.</em>"
      }
      
      HTML(text)
    })
    
    output$hashtags_intervene <- renderText({
      selected_date <- parse_date_time(input$selected_month, "b-y")
      file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
      data <- get(file_name)
      
      data = data[data$misinformation >= input$misinfoSlider,]
      
      # Check if "Organisations" checkbox is checked
      if (input$organisations_checkbox) {
        data <- data[data$Organizations > 0, ]
      }
      
      # Check if "Locations" checkbox is checked
      if (input$locations_checkbox) {
        data <- data[data$Locations > 0, ]
      }
      
      # Check if "Symptoms" checkbox is checked
      if (input$symptoms_checkbox) {
        data <- data[data$Symptoms > 0, ]
      }
      
      # Check if "COVID" checkbox is checked
      if (input$covid_checkbox) {
        data <- data[data$COVID > 0, ]
      }
      
      # Check if "Vaccination" checkbox is checked
      if (input$vaccination_checkbox) {
        data <- data[data$Vaccination > 0, ]
      }
      
      # Check if "Politics" checkbox is checked
      if (input$politics_checkbox) {
        data <- data[data$Politics > 0, ]
      }
      
      # Check if "Conspiracy" checkbox is checked
      if (input$conspiracy_checkbox) {
        data <- data[data$Conspiracy > 0, ]
      }
      
      # Check if "Slurs" checkbox is checked
      if (input$slurs_checkbox) {
        data <- data[data$Slurs > 0, ]
      }
      
      # Check if "Masks" checkbox is checked
      if (input$masks_checkbox) {
        data <- data[data$Masks > 0, ]
      }
      
      # Check if "Origin" checkbox is checked
      if (input$origin_checkbox) {
        data <- data[data$origin > 0, ]
      }
      
      # Check if "Vaccine Conspiracy" checkbox is checked
      if (input$vaccine_conspiracy_checkbox) {
        data <- data[data$vaccine_conspiracy > 0, ]
      }
      
      # Check if "Government" checkbox is checked
      if (input$government_checkbox) {
        data <- data[data$government > 0, ]
      }
      
      # Check if "Pharma" checkbox is checked
      if (input$pharma_checkbox) {
        data <- data[data$pharma > 0, ]
      }
      
      # Check if "Five_G" checkbox is checked
      if (input$five_g_checkbox) {
        data <- data[data$Five_G > 0, ]
      }
      
      # Check if "Gates" checkbox is checked
      if (input$gates_checkbox) {
        data <- data[data$gates > 0, ]
      }
      
      # Check if "NWO" checkbox is checked
      if (input$nwo_checkbox) {
        data <- data[data$nwo > 0, ]
      }
      
      # Check if "Media" checkbox is checked
      if (input$media_checkbox) {
        data <- data[data$media > 0, ]
      }
      
      
      all_hashtags <- unlist(str_extract_all(data$hashtags, "\\w+"))
      
      # Find the 10 most common hashtags
      top_hashtags <- head(sort(table(all_hashtags), decreasing = TRUE), 10)
      most_common_hashtags <- names(top_hashtags)
      
      

      text <- paste0("<strong>The hashtags on which interventions would be effective for the chosen categories and misinformation are::</strong><br>",
                     paste(most_common_hashtags, collapse = ", "))
      
      
      HTML(text)
      
    })
    
    
    
    output$misinformation_month <- renderPlot({
      selected_date <- parse_date_time(input$selected_month, "b-%Y")
      
      # Filter data based on selected month
      selected_data <- global_trends[format(global_trends$date, "%b_%Y") == format(selected_date, "%b_%Y"), ]
      
      # Check if "Organisations" checkbox is checked
      if (input$organisations_checkbox) {
        selected_data <- selected_data[selected_data$Organizations > 0, ]
      }
      
      # Check if "Locations" checkbox is checked
      if (input$locations_checkbox) {
        selected_data <- selected_data[selected_data$Locations > 0, ]
      }
      
      # Check if "Symptoms" checkbox is checked
      if (input$symptoms_checkbox) {
        selected_data <- selected_data[selected_data$Symptoms > 0, ]
      }
      
      # Check if "COVID" checkbox is checked
      if (input$covid_checkbox) {
        selected_data <- selected_data[selected_data$COVID > 0, ]
      }
      
      # Check if "Vaccination" checkbox is checked
      if (input$vaccination_checkbox) {
        selected_data <- selected_data[selected_data$Vaccination > 0, ]
      }
      
      # Check if "Politics" checkbox is checked
      if (input$politics_checkbox) {
        selected_data <- selected_data[selected_data$Politics > 0, ]
      }
      
      # Check if "Conspiracy" checkbox is checked
      if (input$conspiracy_checkbox) {
        selected_data <- selected_data[selected_data$Conspiracy > 0, ]
      }
      
      # Check if "Slurs" checkbox is checked
      if (input$slurs_checkbox) {
        selected_data <- selected_data[selected_data$Slurs > 0, ]
      }
      
      # Check if "Masks" checkbox is checked
      if (input$masks_checkbox) {
        selected_data <- selected_data[selected_data$Masks > 0, ]
      }
      
      # Check if "Origin" checkbox is checked
      if (input$origin_checkbox) {
        selected_data <- selected_data[selected_data$origin > 0, ]
      }
      
      # Check if "Vaccine Conspiracy" checkbox is checked
      if (input$vaccine_conspiracy_checkbox) {
        selected_data <- selected_data[selected_data$vaccine_conspiracy > 0, ]
      }
      
      # Check if "Government" checkbox is checked
      if (input$government_checkbox) {
        selected_data <- selected_data[selected_data$government > 0, ]
      }
      
      # Check if "Pharma" checkbox is checked
      if (input$pharma_checkbox) {
        selected_data <- selected_data[selected_data$pharma > 0, ]
      }
      
      # Check if "Five_G" checkbox is checked
      if (input$five_g_checkbox) {
        selected_data <- selected_data[selected_data$Five_G > 0, ]
      }
      
      # Check if "Gates" checkbox is checked
      if (input$gates_checkbox) {
        selected_data <- selected_data[selected_data$gates > 0, ]
      }
      
      # Check if "NWO" checkbox is checked
      if (input$nwo_checkbox) {
        selected_data <- selected_data[selected_data$nwo > 0, ]
      }
      
      # Check if "Media" checkbox is checked
      if (input$media_checkbox) {
        selected_data <- selected_data[selected_data$media > 0, ]
      }
      
      
      misinfo_graph <- misinformation_month(selected_data)
      misinfo_graph
    })
    
    
    output$favourites_month <- renderPlot({
      selected_date <- parse_date_time(input$selected_month, "b-%Y")
      
      # Filter data based on selected month
      selected_data <- global_trends[format(global_trends$date, "%b_%Y") == format(selected_date, "%b_%Y"), ]
      
      selected_data = selected_data[selected_data$misinformation >= input$misinfoSlider,]
      
      # Check if "Organisations" checkbox is checked
      if (input$organisations_checkbox) {
        selected_data <- selected_data[selected_data$Organizations > 0, ]
      }
      
      # Check if "Locations" checkbox is checked
      if (input$locations_checkbox) {
        selected_data <- selected_data[selected_data$Locations > 0, ]
      }
      
      # Check if "Symptoms" checkbox is checked
      if (input$symptoms_checkbox) {
        selected_data <- selected_data[selected_data$Symptoms > 0, ]
      }
      
      # Check if "COVID" checkbox is checked
      if (input$covid_checkbox) {
        selected_data <- selected_data[selected_data$COVID > 0, ]
      }
      
      # Check if "Vaccination" checkbox is checked
      if (input$vaccination_checkbox) {
        selected_data <- selected_data[selected_data$Vaccination > 0, ]
      }
      
      # Check if "Politics" checkbox is checked
      if (input$politics_checkbox) {
        selected_data <- selected_data[selected_data$Politics > 0, ]
      }
      
      # Check if "Conspiracy" checkbox is checked
      if (input$conspiracy_checkbox) {
        selected_data <- selected_data[selected_data$Conspiracy > 0, ]
      }
      
      # Check if "Slurs" checkbox is checked
      if (input$slurs_checkbox) {
        selected_data <- selected_data[selected_data$Slurs > 0, ]
      }
      
      # Check if "Masks" checkbox is checked
      if (input$masks_checkbox) {
        selected_data <- selected_data[selected_data$Masks > 0, ]
      }
      
      # Check if "Origin" checkbox is checked
      if (input$origin_checkbox) {
        selected_data <- selected_data[selected_data$origin > 0, ]
      }
      
      # Check if "Vaccine Conspiracy" checkbox is checked
      if (input$vaccine_conspiracy_checkbox) {
        selected_data <- selected_data[selected_data$vaccine_conspiracy > 0, ]
      }
      
      # Check if "Government" checkbox is checked
      if (input$government_checkbox) {
        selected_data <- selected_data[selected_data$government > 0, ]
      }
      
      # Check if "Pharma" checkbox is checked
      if (input$pharma_checkbox) {
        selected_data <- selected_data[selected_data$pharma > 0, ]
      }
      
      # Check if "Five_G" checkbox is checked
      if (input$five_g_checkbox) {
        selected_data <- selected_data[selected_data$Five_G > 0, ]
      }
      
      # Check if "Gates" checkbox is checked
      if (input$gates_checkbox) {
        selected_data <- selected_data[selected_data$gates > 0, ]
      }
      
      # Check if "NWO" checkbox is checked
      if (input$nwo_checkbox) {
        selected_data <- selected_data[selected_data$nwo > 0, ]
      }
      
      # Check if "Media" checkbox is checked
      if (input$media_checkbox) {
        selected_data <- selected_data[selected_data$media > 0, ]
      }
      
      
      fav_graph <- favourites_month(selected_data)
      fav_graph
    })
    
    output$favourites <- renderPlot({
      
      # Filter data based on selected month
      selected_data <- global_trends
      
      selected_data = selected_data[selected_data$misinformation >= input$misinfoSlider,]
      
      # Check if "Organisations" checkbox is checked
      if (input$organisations_checkbox) {
        selected_data <- selected_data[selected_data$Organizations > 0, ]
      }
      
      # Check if "Locations" checkbox is checked
      if (input$locations_checkbox) {
        selected_data <- selected_data[selected_data$Locations > 0, ]
      }
      
      # Check if "Symptoms" checkbox is checked
      if (input$symptoms_checkbox) {
        selected_data <- selected_data[selected_data$Symptoms > 0, ]
      }
      
      # Check if "COVID" checkbox is checked
      if (input$covid_checkbox) {
        selected_data <- selected_data[selected_data$COVID > 0, ]
      }
      
      # Check if "Vaccination" checkbox is checked
      if (input$vaccination_checkbox) {
        selected_data <- selected_data[selected_data$Vaccination > 0, ]
      }
      
      # Check if "Politics" checkbox is checked
      if (input$politics_checkbox) {
        selected_data <- selected_data[selected_data$Politics > 0, ]
      }
      
      # Check if "Conspiracy" checkbox is checked
      if (input$conspiracy_checkbox) {
        selected_data <- selected_data[selected_data$Conspiracy > 0, ]
      }
      
      # Check if "Slurs" checkbox is checked
      if (input$slurs_checkbox) {
        selected_data <- selected_data[selected_data$Slurs > 0, ]
      }
      
      # Check if "Masks" checkbox is checked
      if (input$masks_checkbox) {
        selected_data <- selected_data[selected_data$Masks > 0, ]
      }
      
      # Check if "Origin" checkbox is checked
      if (input$origin_checkbox) {
        selected_data <- selected_data[selected_data$origin > 0, ]
      }
      
      # Check if "Vaccine Conspiracy" checkbox is checked
      if (input$vaccine_conspiracy_checkbox) {
        selected_data <- selected_data[selected_data$vaccine_conspiracy > 0, ]
      }
      
      # Check if "Government" checkbox is checked
      if (input$government_checkbox) {
        selected_data <- selected_data[selected_data$government > 0, ]
      }
      
      # Check if "Pharma" checkbox is checked
      if (input$pharma_checkbox) {
        selected_data <- selected_data[selected_data$pharma > 0, ]
      }
      
      # Check if "Five_G" checkbox is checked
      if (input$five_g_checkbox) {
        selected_data <- selected_data[selected_data$Five_G > 0, ]
      }
      
      # Check if "Gates" checkbox is checked
      if (input$gates_checkbox) {
        selected_data <- selected_data[selected_data$gates > 0, ]
      }
      
      # Check if "NWO" checkbox is checked
      if (input$nwo_checkbox) {
        selected_data <- selected_data[selected_data$nwo > 0, ]
      }
      
      # Check if "Media" checkbox is checked
      if (input$media_checkbox) {
        selected_data <- selected_data[selected_data$media > 0, ]
      }
      
      
      fav_graph <- favourites(selected_data)
      fav_graph
    })
    
    
    
    output$sentiment_plot <- renderPlot({
      # Load the necessary data (replace with your own data loading code)
      selected_date <- parse_date_time(input$selected_month, "b-y")
      file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
      data <- get(file_name)
      
      
      data <- data[data$misinformation >= input$misinfoSlider,]
      
      # Check if "Organisations" checkbox is checked
      if (input$organisations_checkbox) {
        data <- data[data$Organizations > 0, ]
      }
      
      # Check if "Locations" checkbox is checked
      if (input$locations_checkbox) {
        data <- data[data$Locations > 0, ]
      }
      
      # Check if "Symptoms" checkbox is checked
      if (input$symptoms_checkbox) {
        data <- data[data$Symptoms > 0, ]
      }
      
      # Check if "COVID" checkbox is checked
      if (input$covid_checkbox) {
        data <- data[data$COVID > 0, ]
      }
      
      # Check if "Vaccination" checkbox is checked
      if (input$vaccination_checkbox) {
        data <- data[data$Vaccination > 0, ]
      }
      
      # Check if "Politics" checkbox is checked
      if (input$politics_checkbox) {
        data <- data[data$Politics > 0, ]
      }
      
      # Check if "Conspiracy" checkbox is checked
      if (input$conspiracy_checkbox) {
        data <- data[data$Conspiracy > 0, ]
      }
      
      # Check if "Slurs" checkbox is checked
      if (input$slurs_checkbox) {
        data <- data[data$Slurs > 0, ]
      }
      
      # Check if "Masks" checkbox is checked
      if (input$masks_checkbox) {
        data <- data[data$Masks > 0, ]
      }
      
      # Check if "Origin" checkbox is checked
      if (input$origin_checkbox) {
        data <- data[data$origin > 0, ]
      }
      
      # Check if "Vaccine Conspiracy" checkbox is checked
      if (input$vaccine_conspiracy_checkbox) {
        data <- data[data$vaccine_conspiracy > 0, ]
      }
      
      # Check if "Government" checkbox is checked
      if (input$government_checkbox) {
        data <- data[data$government > 0, ]
      }
      
      # Check if "Pharma" checkbox is checked
      if (input$pharma_checkbox) {
        data <- data[data$pharma > 0, ]
      }
      
      # Check if "Five_G" checkbox is checked
      if (input$five_g_checkbox) {
        data <- data[data$Five_G > 0, ]
      }
      
      # Check if "Gates" checkbox is checked
      if (input$gates_checkbox) {
        data <- data[data$gates > 0, ]
      }
      
      # Check if "NWO" checkbox is checked
      if (input$nwo_checkbox) {
        data <- data[data$nwo > 0, ]
      }
      
      # Check if "Media" checkbox is checked
      if (input$media_checkbox) {
        data <- data[data$media > 0, ]
      }
      
      
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
        theme_minimal() +
        theme(
          legend.position = "top",            # Set legend position to top
          legend.justification = c(1, 0),     # Set legend justification to top right
          legend.margin = margin(6, 6, 6, 6)  # Set margin around the legend
        )
    })
    
    
    output$hashtags_by_category <- renderPlot({
      selected_date <- parse_date_time(input$selected_month, "b-y")
      file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
      data <- get(file_name)
      
      data = data[data$misinformation >= input$misinfoSlider,]
      
      # Check if "Organisations" checkbox is checked
      if (input$organisations_checkbox) {
        data <- data[data$Organizations > 0, ]
      }
      
      # Check if "Locations" checkbox is checked
      if (input$locations_checkbox) {
        data <- data[data$Locations > 0, ]
      }
      
      # Check if "Symptoms" checkbox is checked
      if (input$symptoms_checkbox) {
        data <- data[data$Symptoms > 0, ]
      }
      
      # Check if "COVID" checkbox is checked
      if (input$covid_checkbox) {
        data <- data[data$COVID > 0, ]
      }
      
      # Check if "Vaccination" checkbox is checked
      if (input$vaccination_checkbox) {
        data <- data[data$Vaccination > 0, ]
      }
      
      # Check if "Politics" checkbox is checked
      if (input$politics_checkbox) {
        data <- data[data$Politics > 0, ]
      }
      
      # Check if "Conspiracy" checkbox is checked
      if (input$conspiracy_checkbox) {
        data <- data[data$Conspiracy > 0, ]
      }
      
      # Check if "Slurs" checkbox is checked
      if (input$slurs_checkbox) {
        data <- data[data$Slurs > 0, ]
      }
      
      # Check if "Masks" checkbox is checked
      if (input$masks_checkbox) {
        data <- data[data$Masks > 0, ]
      }
      
      # Check if "Origin" checkbox is checked
      if (input$origin_checkbox) {
        data <- data[data$origin > 0, ]
      }
      
      # Check if "Vaccine Conspiracy" checkbox is checked
      if (input$vaccine_conspiracy_checkbox) {
        data <- data[data$vaccine_conspiracy > 0, ]
      }
      
      # Check if "Government" checkbox is checked
      if (input$government_checkbox) {
        data <- data[data$government > 0, ]
      }
      
      # Check if "Pharma" checkbox is checked
      if (input$pharma_checkbox) {
        data <- data[data$pharma > 0, ]
      }
      
      # Check if "Five_G" checkbox is checked
      if (input$five_g_checkbox) {
        data <- data[data$Five_G > 0, ]
      }
      
      # Check if "Gates" checkbox is checked
      if (input$gates_checkbox) {
        data <- data[data$gates > 0, ]
      }
      
      # Check if "NWO" checkbox is checked
      if (input$nwo_checkbox) {
        data <- data[data$nwo > 0, ]
      }
      
      # Check if "Media" checkbox is checked
      if (input$media_checkbox) {
        data <- data[data$media > 0, ]
      }
      
      
      all_hashtags <- unlist(str_extract_all(data$hashtags, "\\w+"))
      
      # Find the two most common hashtags
      top_hashtags <- head(sort(table(all_hashtags), decreasing = TRUE), 10)
      most_common_hashtags <- names(top_hashtags)
      
      # Create new variables for the most common hashtags
      for (hashtag in most_common_hashtags) {
        hashtag_column <- paste0("hashtag_", hashtag)  # Add prefix "hashtag_" to the column name
        data[[hashtag_column]] <- ifelse(grepl(hashtag, data$hashtags), 1, 0)
      }
      
      library(ggplot2)
      
      hashtag_columns <- data[, grepl("^hashtag_", colnames(data))]
      hashtag_counts <- colSums(hashtag_columns)
      
      # Sort the hashtags by count in descending order
      sorted_hashtags <- sort(hashtag_counts, decreasing = TRUE)
      
      # Remove the "hashtag_" prefix from the names
      hashtag_names <- sub("^hashtag_", "", names(sorted_hashtags))
      
      # Create a data frame for plotting
      plot_data <- data.frame(Hashtags = hashtag_names, Counts = sorted_hashtags)
      
      # Create the bar plot using ggplot2
      ggplot(plot_data, aes(x = reorder(Hashtags, -Counts), y = Counts)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(x = "Hashtags", y = "Counts", title = paste0("Hashtag counts - ", input$selected_month)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$hashtags_by_category_all <- renderPlot({
      data <- global_trends
      data <- data %>%
        select(-starts_with("hashtag_"))
      
      data = data[data$misinformation >= input$misinfoSlider,]
      
      # Check if "Organisations" checkbox is checked
      if (input$organisations_checkbox) {
        data <- data[data$Organizations > 0, ]
      }
      
      # Check if "Locations" checkbox is checked
      if (input$locations_checkbox) {
        data <- data[data$Locations > 0, ]
      }
      
      # Check if "Symptoms" checkbox is checked
      if (input$symptoms_checkbox) {
        data <- data[data$Symptoms > 0, ]
      }
      
      # Check if "COVID" checkbox is checked
      if (input$covid_checkbox) {
        data <- data[data$COVID > 0, ]
      }
      
      # Check if "Vaccination" checkbox is checked
      if (input$vaccination_checkbox) {
        data <- data[data$Vaccination > 0, ]
      }
      
      # Check if "Politics" checkbox is checked
      if (input$politics_checkbox) {
        data <- data[data$Politics > 0, ]
      }
      
      # Check if "Conspiracy" checkbox is checked
      if (input$conspiracy_checkbox) {
        data <- data[data$Conspiracy > 0, ]
      }
      
      # Check if "Slurs" checkbox is checked
      if (input$slurs_checkbox) {
        data <- data[data$Slurs > 0, ]
      }
      
      # Check if "Masks" checkbox is checked
      if (input$masks_checkbox) {
        data <- data[data$Masks > 0, ]
      }
      
      # Check if "Origin" checkbox is checked
      if (input$origin_checkbox) {
        data <- data[data$origin > 0, ]
      }
      
      # Check if "Vaccine Conspiracy" checkbox is checked
      if (input$vaccine_conspiracy_checkbox) {
        data <- data[data$vaccine_conspiracy > 0, ]
      }
      
      # Check if "Government" checkbox is checked
      if (input$government_checkbox) {
        data <- data[data$government > 0, ]
      }
      
      # Check if "Pharma" checkbox is checked
      if (input$pharma_checkbox) {
        data <- data[data$pharma > 0, ]
      }
      
      # Check if "Five_G" checkbox is checked
      if (input$five_g_checkbox) {
        data <- data[data$Five_G > 0, ]
      }
      
      # Check if "Gates" checkbox is checked
      if (input$gates_checkbox) {
        data <- data[data$gates > 0, ]
      }
      
      # Check if "NWO" checkbox is checked
      if (input$nwo_checkbox) {
        data <- data[data$nwo > 0, ]
      }
      
      # Check if "Media" checkbox is checked
      if (input$media_checkbox) {
        data <- data[data$media > 0, ]
      }
      
      
      all_hashtags <- unlist(str_extract_all(data$hashtags, "\\w+"))
      
      # Find the two most common hashtags
      top_hashtags <- head(sort(table(all_hashtags), decreasing = TRUE), 10)
      most_common_hashtags <- names(top_hashtags)
      
      # Create new variables for the most common hashtags
      for (hashtag in most_common_hashtags) {
        hashtag_column <- paste0("hashtag_", hashtag)  # Add prefix "hashtag_" to the column name
        data[[hashtag_column]] <- ifelse(grepl(hashtag, data$hashtags), 1, 0)
      }
      
      library(ggplot2)
      
      hashtag_columns <- data[, grepl("^hashtag_", colnames(data))]
      hashtag_counts <- colSums(hashtag_columns)
      
      # Sort the hashtags by count in descending order
      sorted_hashtags <- sort(hashtag_counts, decreasing = TRUE)
      
      # Remove the "hashtag_" prefix from the names
      hashtag_names <- sub("^hashtag_", "", names(sorted_hashtags))
      
      # Create a data frame for plotting
      plot_data <- data.frame(Hashtags = hashtag_names, Counts = sorted_hashtags)
      
      # Create the bar plot using ggplot2
      ggplot(plot_data, aes(x = reorder(Hashtags, -Counts), y = Counts)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(x = "Hashtags", y = "Counts", title = "Hashtag Counts - All time") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$co_occurrence_plot <- renderImage({
      # Construct the file path for the image
      selected_date <- parse_date_time(input$selected_month, "b-y")
      image_file <- paste0(format(selected_date, format = "%b_%Y"), ".png")
      image_path <- file.path("..", "results", image_file)
      
      # Return a list with the src and alt attributes
      list(src = image_path, alt = "Co-occurrence Plot", class = "custom-image")
    }, deleteFile = FALSE)
    
    
    
    output$misinformation = renderPlot({
      
      # Filter data based on selected month
      selected_data <- global_trends
      
      # Check if "Organisations" checkbox is checked
      if (input$organisations_checkbox) {
        selected_data <- selected_data[selected_data$Organizations > 0, ]
      }
      
      # Check if "Locations" checkbox is checked
      if (input$locations_checkbox) {
        selected_data <- selected_data[selected_data$Locations > 0, ]
      }
      
      # Check if "Symptoms" checkbox is checked
      if (input$symptoms_checkbox) {
        selected_data <- selected_data[selected_data$Symptoms > 0, ]
      }
      
      # Check if "COVID" checkbox is checked
      if (input$covid_checkbox) {
        selected_data <- selected_data[selected_data$COVID > 0, ]
      }
      
      # Check if "Vaccination" checkbox is checked
      if (input$vaccination_checkbox) {
        selected_data <- selected_data[selected_data$Vaccination > 0, ]
      }
      
      # Check if "Politics" checkbox is checked
      if (input$politics_checkbox) {
        selected_data <- selected_data[selected_data$Politics > 0, ]
      }
      
      # Check if "Conspiracy" checkbox is checked
      if (input$conspiracy_checkbox) {
        selected_data <- selected_data[selected_data$Conspiracy > 0, ]
      }
      
      # Check if "Slurs" checkbox is checked
      if (input$slurs_checkbox) {
        selected_data <- selected_data[selected_data$Slurs > 0, ]
      }
      
      # Check if "Masks" checkbox is checked
      if (input$masks_checkbox) {
        selected_data <- selected_data[selected_data$Masks > 0, ]
      }
      
      # Check if "Origin" checkbox is checked
      if (input$origin_checkbox) {
        selected_data <- selected_data[selected_data$origin > 0, ]
      }
      
      # Check if "Vaccine Conspiracy" checkbox is checked
      if (input$vaccine_conspiracy_checkbox) {
        selected_data <- selected_data[selected_data$vaccine_conspiracy > 0, ]
      }
      
      # Check if "Government" checkbox is checked
      if (input$government_checkbox) {
        selected_data <- selected_data[selected_data$government > 0, ]
      }
      
      # Check if "Pharma" checkbox is checked
      if (input$pharma_checkbox) {
        selected_data <- selected_data[selected_data$pharma > 0, ]
      }
      
      # Check if "Five_G" checkbox is checked
      if (input$five_g_checkbox) {
        selected_data <- selected_data[selected_data$Five_G > 0, ]
      }
      
      # Check if "Gates" checkbox is checked
      if (input$gates_checkbox) {
        selected_data <- selected_data[selected_data$gates > 0, ]
      }
      
      # Check if "NWO" checkbox is checked
      if (input$nwo_checkbox) {
        selected_data <- selected_data[selected_data$nwo > 0, ]
      }
      
      # Check if "Media" checkbox is checked
      if (input$media_checkbox) {
        selected_data <- selected_data[selected_data$media > 0, ]
      }
      
      misinfo_graph <- misinformation(selected_data)
      misinfo_graph
    })

    
    output$alltime_trends <- renderPlot({
      # Load the necessary data (replace with your own data loading code)
      data <- global_trends
      
      data <- data[data$misinformation >= input$misinfoSlider,]
      
      # Check if "Organisations" checkbox is checked
      if (input$organisations_checkbox) {
        data <- data[data$Organizations > 0, ]
      }
      
      # Check if "Locations" checkbox is checked
      if (input$locations_checkbox) {
        data <- data[data$Locations > 0, ]
      }
      
      # Check if "Symptoms" checkbox is checked
      if (input$symptoms_checkbox) {
        data <- data[data$Symptoms > 0, ]
      }
      
      # Check if "COVID" checkbox is checked
      if (input$covid_checkbox) {
        data <- data[data$COVID > 0, ]
      }
      
      # Check if "Vaccination" checkbox is checked
      if (input$vaccination_checkbox) {
        data <- data[data$Vaccination > 0, ]
      }
      
      # Check if "Politics" checkbox is checked
      if (input$politics_checkbox) {
        data <- data[data$Politics > 0, ]
      }
      
      # Check if "Conspiracy" checkbox is checked
      if (input$conspiracy_checkbox) {
        data <- data[data$Conspiracy > 0, ]
      }
      
      # Check if "Slurs" checkbox is checked
      if (input$slurs_checkbox) {
        data <- data[data$Slurs > 0, ]
      }
      
      # Check if "Masks" checkbox is checked
      if (input$masks_checkbox) {
        data <- data[data$Masks > 0, ]
      }
      
      # Check if "Origin" checkbox is checked
      if (input$origin_checkbox) {
        data <- data[data$origin > 0, ]
      }
      
      # Check if "Vaccine Conspiracy" checkbox is checked
      if (input$vaccine_conspiracy_checkbox) {
        data <- data[data$vaccine_conspiracy > 0, ]
      }
      
      # Check if "Government" checkbox is checked
      if (input$government_checkbox) {
        data <- data[data$government > 0, ]
      }
      
      # Check if "Pharma" checkbox is checked
      if (input$pharma_checkbox) {
        data <- data[data$pharma > 0, ]
      }
      
      # Check if "Five_G" checkbox is checked
      if (input$five_g_checkbox) {
        data <- data[data$Five_G > 0, ]
      }
      
      # Check if "Gates" checkbox is checked
      if (input$gates_checkbox) {
        data <- data[data$gates > 0, ]
      }
      
      # Check if "NWO" checkbox is checked
      if (input$nwo_checkbox) {
        data <- data[data$nwo > 0, ]
      }
      
      # Check if "Media" checkbox is checked
      if (input$media_checkbox) {
        data <- data[data$media > 0, ]
      }
      
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
          values = c("blue", "red", "black"),    # Include "black" for the additional point
          labels = c("Positive", "Negative", "All")
        ) +
        theme_minimal() +
        theme(
          legend.position = "topright"            # Set legend position to top right
        )
      
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
    
    # output$sentiment_neg_clusters <- renderPlot({
    #   # Load the necessary data (replace with your own data loading code)
    #   data <- sentiment_clusters
    #   clusters = ClusterAnalysis(data)
    #   negative_sentiments = clusters[[1]]
    #   
    #   EGA(negative_sentiments, cor = "spearman")
    #   
    # })
    # 
    # output$sentiment_neg_clusters_monthly <- renderPlot({
    #   # Load the necessary data (replace with your own data loading code)
    #   # Load the necessary data (replace with your own data loading code)
    #   selected_date <- parse_date_time(input$selected_month, "b-y")
    #   file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
    #   data <- get(file_name)
    #   clusters = ClusterAnalysis(data)
    #   negative_sentiments = clusters[[1]]
    #   
    #   EGA(negative_sentiments, cor = "spearman")
    #   
    # })
    # 
    # output$sentiment_pos_clusters <- renderPlot({
    #   # Load the necessary data (replace with your own data loading code)
    #   data <- sentiment_clusters
    #   clusters = ClusterAnalysis(data)
    #   
    #   positive_sentiments = clusters[[2]]
    #   
    #   EGA(positive_sentiments, cor = "spearman")
    # })
    # 
    # 
    # output$sentiment_pos_clusters_monthly <- renderPlot({
    #   # Load the necessary data (replace with your own data loading code)
    #   # Load the necessary data (replace with your own data loading code)
    #   selected_date <- parse_date_time(input$selected_month, "b-y")
    #   file_name <- paste0("data_", format(selected_date, format = "%b_%Y"))
    #   data <- get(file_name)
    #   clusters = ClusterAnalysis(data)
    #   negative_sentiments = clusters[[2]]
    #   
    #   EGA(negative_sentiments, cor = "spearman")
    # })
    
    
  })
}

# Run the app
shinyApp(ui = ui, server = server)

