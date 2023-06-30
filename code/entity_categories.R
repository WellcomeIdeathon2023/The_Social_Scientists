data = read.csv("vax_tweets_with_sentiment_entities_3.csv")

data$entities <- tolower(data$entities)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tidyr)

negative_data = data[data$polarity_text < 0,]

# Example categories and corresponding rules or dictionaries
categories <- list(
  Organizations = c("Pfizer", "AstraZeneca", "CDCgov", "NHS", "Moderna", "Johnson & Johnson", "CDC", "fda", "FDA", "EU", "WHO", "Fox News", "Boris Johnson", "Bill Gates"),
  Locations = c("America", "USA","American", "UK", "NYC", "Europe", "India", "US", "U.S", "Chicago", "Canada", "Australia", "Florida", "Israel", "California", "Ontario", "Russia", "NYC", "London", "Delhi", "Pakistan",
                "Japan", "Germany", "South Africa", "CovidUK", "Ireland", "US", "Indian", "USUN", "EU", "COVID19Aus", "IndiaFightsCorona", "Canadian", "Europe", "China", "Africa"),
  Symptoms = c("fever", "cough", "LongCovid", "ICU", "fatigue", "shortness of breath", "sore throat", "loss of taste", "headache", "long COVID", "ICU", "Health"),
  COVID = c("COVID19", "COVID", "Corona", "Covid19", "COVID-19", "COVID-19 variants", "Delta variant", "Omicron variant", "COVID19", "COVID", "Covid", "Delta", "DELTA", "ALPHA", "Alpha", "alpha", "detla", "omicron", "OMICRON", "Omicron","DeltaVariant"),
  Vaccination = c("COVID vaccine", "COVID vaccine", "COVIDVACCINE", "Covaxin", "COVID Vaccine", "COVID-19 vaccine", "VACCINE", "Chicago Vaccination", "Vaccine", "CovidVaccine", "COVIDvaccine", "COVIDVaccine", "BoosterJab", "Covaxin", "vaccine", "GetVaccinated", "booster shot", "get vaccinated", "vaccination", "BoosterJab", "COVID19Vic"),
  Politics = c("Trump", "Joe Biden", "JoeBiden", "Biden", "Boris Johnson", "POTUS", "MAGA", "TRUMP", "Johnson",  "Boris", "Don", "BorisJohnson"),
  Media = c("Fox News", "FoxNews"),
  Conspiracy = c("DrFauci", "Fauci", "Ivermectin", "ivermectin", "IVERMECTIN", "BillGates", "Secret", "mRNA", "EUA"),
  Slurs = c("COVIDIOTS", "COVIDIOT", "Sad"),
  Masks = c("MaskUp", "WearAMask"),
  Miscellaneous = c("Please", "NOT", "WTF", "Sorry", "Hey", "Visit", "THE", "Book","NOT","Everyone", "Anyone", "Black", "COVID19nsw","COVIDãf¼19", "Get", "Sad", "Sorry", "WTF", "Hey", "Book", "Visit", "CovidVic")
)


# Convert categories to lowercase
for (cat_name in names(categories)) {
  categories[[cat_name]] <- tolower(categories[[cat_name]])
}


# Create an empty dataframe
entity_category_df <- data.frame(Entity = character(), Category = character(), stringsAsFactors = FALSE)

# Iterate over each row in your dataset
for (row in 1:nrow(negative_data)) {
  entities <- negative_data[row, "entities"]
  
  # Check if the row has any entities
  if (entities != "[]") {
    # Remove square brackets and split the string into individual entities
    entities <- gsub("\\[|\\]|\'","", entities)
    entities <- strsplit(entities, ", ")[[1]]
    
    # Remove leading and trailing whitespaces from each entity
    entities <- trimws(entities)
    
    # Iterate over each entity
    for (entity in entities) {
      category <- NA  # Initialize category as NA
      
      # Check which category the entity belongs to
      for (cat_name in names(categories)) {
        if (entity %in% categories[[cat_name]]) {
          category <- cat_name
          break
        }
      }
      
      # Append the entity and category to the dataframe
      entity_category_df <- rbind(entity_category_df, data.frame(Entity = entity, Category = category, stringsAsFactors = FALSE))
    }
  }
}

# Replace NA values with "Undefined"
entity_category_df$Category[is.na(entity_category_df$Category)] <- "Undefined"

# Count the occurrences of each category
category_counts <- entity_category_df %>%
  group_by(Category) %>%
  summarise(Count = n())

# Count the entities
entity_counts <- entity_category_df %>%
  count(Entity, sort = TRUE) %>%
  head(10)  # Change the number as desired


#Barplot of most common entities
ggplot(entity_counts, aes(x = reorder(Entity, -n), y = n)) +
  geom_col(fill = "skyblue", width = 0.9) +
  xlab("Named Entity") +
  ylab("Count") +
  ggtitle("Top 10 Most Common Named Entities") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


# Barplot of categories
ggplot(category_counts[category_counts$Category != "Undefined",], aes(x=reorder(Category, -Count), y=Count)) +
  geom_bar(stat="identity") +
  geom_col(fill = "skyblue", width = 0.9) +
  xlab("Category") +
  ylab("Count") +
  ggtitle("Most common categories") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


sum(category_counts$Count)
sum(entity_counts$n)
