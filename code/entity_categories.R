data = read.csv("vax_tweets_with_sentiment_entities_3.csv")

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
  COVID = c("COVID19", "COVID vaccine", "COVID", "Corona", "Covid19", "COVID-19", "Covaxin", "COVID-19 vaccine", "COVID-19 variants", "Delta variant", "Omicron variant", "COVID19", "COVID", "Covid", "Delta", "DELTA", "ALPHA", "Alpha", "alpha", "detla", "omicron", "OMICRON", "Omicron","DeltaVariant"),
  Vaccination = c("COVID vaccine", "COVIDVACCINE", "COVID Vaccine", "VACCINE", "Chicago Vaccination", "Vaccine", "CovidVaccine", "COVIDvaccine", "COVIDVaccine", "BoosterJab", "Covaxin", "vaccine", "GetVaccinated", "booster shot", "get vaccinated", "vaccination", "BoosterJab", "COVID19Vic"),
  Politics = c("Trump", "Joe Biden", "JoeBiden", "Biden", "Boris Johnson", "POTUS", "MAGA", "TRUMP", "Johnson",  "Boris", "Don", "BorisJohnson"),
  Media = c("Fox News", "FoxNews"),
  Conspiracy = c("DrFauci", "Fauci", "Ivermectin", "ivermectin", "IVERMECTIN", "BillGates", "Secret", "mRNA", "EUA"),
  Slurs = c("COVIDIOTS", "COVIDIOT", "Sad"),
  Masks = c("MaskUp", "WearAMask"),
  Miscellaneous = c("Please", "NOT", "WTF", "Sorry", "Hey", "Visit", "THE", "Book","NOT","Everyone", "Anyone", "Black", "COVID19nsw","COVIDãf¼19", "Get", "Sad", "Sorry", "WTF", "Hey", "Book", "Visit", "CovidVic")
)
                    



categorize_named_entities <- function(named_entities) {
  categorized_entities <- vector("list", length(named_entities))
  
  for (i in seq_along(named_entities)) {
    entity_vector <- gsub("\\[|\\]|'|\"", "", named_entities[i])
    entities <- strsplit(entity_vector, ",\\s*")[[1]]
    
    categorized_entity_list <- vector("character", length(entities))
    
    for (j in seq_along(entities)) {
      entity_category <- trimws(entities[j])
      entity <- ""
      category <- "Undefined"
      matched_category <- FALSE
      
      if (grepl("::", entity_category)) {
        parts <- strsplit(entity_category, "::")[[1]]
        entity <- trimws(parts[1])
        category <- trimws(parts[2])
        
        for (cat in names(categories)) {
          if (category %in% categories[[cat]]) {
            category <- cat
            matched_category <- TRUE
            break
          }
        }
      } else {
        entity <- entity_category
      }
      
      if (!matched_category) {
        for (cat in names(categories)) {
          if (grepl(paste0("\\b", entity, "\\b"), categories[[cat]], ignore.case = TRUE)) {
            category <- cat
            matched_category <- TRUE
            break
          }
        }
      }
      
      categorized_entity_list[j] <- paste(entity, category, sep = "::")  # Combine entity and category
    }
    
    categorized_entities[[i]] <- list(entities = categorized_entity_list)
  }
  
  return(categorized_entities)
}


named_entities <- negative_data$entities
categorized_entities <- categorize_named_entities(named_entities)


# Create a new data frame with named entities and categories
categorized_data <- data.frame(do.call(rbind, categorized_entities), stringsAsFactors = FALSE)

# Split the entities column into separate rows

library(stringr)

categorized_data <- categorized_data %>%
  mutate(entities = ifelse(is.na(entities), "Undefined", entities)) %>%
  mutate(entities = gsub("\\[|\\]|'|\"", "", entities)) %>%
  separate_rows(entities, sep = ",") %>%
  separate(entities, into = c("Entity", "Category"), sep = "::", extra = "merge") %>%
  mutate(Entity = str_trim(Entity)) %>%
  mutate(Category = ifelse(is.na(Category), "Undefined", str_trim(Category))) %>%
  mutate(Category = str_remove(Category, "\\)$"))

categorized_data <- categorized_data[complete.cases(categorized_data), ]


# Count the occurrences of each category
category_counts <- categorized_data %>%
  group_by(Category) %>%
  summarise(Count = n())


# Add the "Undefined" category if there are undefined entities
if (!any(category_counts$Category == "Undefined")) {
  category_counts <- rbind(category_counts, data.frame(Category = "Undefined", Count = 0))
}

# Remove "c(" and ")" from the Entity column
categorized_data$Entity <- gsub("^c\\(|\\)$", "", categorized_data$Entity)

# Filter out rows with empty entities (character(0))
categorized_data <- categorized_data[!(categorized_data$Entity == "character(0" | categorized_data$Entity == "character(0)"), ]

# Count the entities
entity_counts <- categorized_data %>%
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

