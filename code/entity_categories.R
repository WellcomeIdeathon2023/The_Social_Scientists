data = read.csv("vax_tweets_with_sentiment_entities_3.csv")
data$date <- as.Date(data$date)

data = data[data$polarity_text < 0, ]

# Example categories and corresponding rules or dictionaries
categories <- list(
  Organizations = c("CDCgov", "CDC", "NHS", "Moderna", "fda", "WHO", "Fox News"),
  Locations = c("America", "USA","American", "UK", "NYC", "Europe", "India", "US", "U.S", "Chicago", "Canada", "Australia", "Florida", "Israel", "California", "Ontario", "Russia", "NYC", "London", "Delhi", "Pakistan",
                "Japan", "Germany", "South Africa", "CovidUK", "Ireland", "US", "Indian", "USUN", "EU", "COVID19Aus", "IndiaFightsCorona", "Canadian", "Europe", "China", "Africa"),
  Symptoms = c("fever", "cough", "LongCovid", "ICU", "fatigue", "shortness of breath", "sore throat", "loss of taste", "headache", "long COVID", "ICU", "Health"),
  COVID = c("COVID19", "COVID", "Corona", "Covid19", "COVID-19", "COVID-19 variants", "Delta variant", "Omicron variant", "COVID19", "COVID", "Covid", "Delta", "DELTA", "ALPHA", "Alpha", "alpha", "detla", "omicron", "OMICRON", "Omicron","DeltaVariant"),
  Vaccination = c("COVID vaccine", "COVID vaccine", "COVIDVACCINE", "Covaxin", "COVID Vaccine", "COVID-19 vaccine", "VACCINE", "Chicago Vaccination", "Vaccine", "CovidVaccine", "COVIDvaccine", "COVIDVaccine", "BoosterJab", "Covaxin", "vaccine", "GetVaccinated", "booster shot", "get vaccinated", "vaccination", "BoosterJab", "COVID19Vic"),
  Politics = c("Trump", "Joe Biden", "JoeBiden", "Biden", "Boris Johnson", "POTUS", "MAGA", "TRUMP", "Johnson",  "Boris", "Don", "BorisJohnson"),
  Conspiracy = c("DrFauci", "Fauci", "Ivermectin", "ivermectin", "IVERMECTIN", "BillGates", "Secret"),
  Slurs = c("COVIDIOTS", "COVIDIOT", "Sad"),
  Masks = c("MaskUp", "WearAMask"),
  Miscellaneous = c("Please", "NOT", "WTF", "Sorry", "Hey", "Visit", "THE", "Book","NOT","Everyone", "Anyone", "Black", "COVID19nsw","COVIDãf¼19", "Get", "Sad", "Sorry", "WTF", "Hey", "Book", "Visit", "CovidVic"),
  origin = c(
    "Wuhan lab leak", "Wuhan", "lab leak", "lab",
    "Virus bioweapon", "bioweapon",
    "COVID cover-up","cover-up", "cover up",
    "Origin conspiracy", "origin",
    "Natural virus theory", "Natural virus",
    "Virus escape", "escape",
    "Lab-created virus", "lab-created",
    "Pandemic engineered", "engineered", "engineer",
    "Virus manipulation",
    "Origin of COVID",
    "COVID origins",
    "Virus origin",
    "Virus release", "release"
  ),
  vaccine_conspiracy = c(
    "Vaccine microchipping", "microchipping", "microchip", "chip",
    "Vaccine side effects cover-up", "side effects", "side-effects", "effects",
    "Vaccine efficacy doubts", "efficacy",
    "Vaccine hidden agenda",
    "Vaccine conspiracy",
    "Vaccine dangers", "mrna", "rna", "dna", "EUA", "Emergency use", "Emergency-Use",
    "Vaccine depopulation",
    "Vaccine profit motive", "profit", "for-profit", "for profit",
    "Vaccine long-term effects", "long-term", "unauthorised", "approval", "approve", "approved", 
    "COVID vaccine conspiracy",
    "Vaccine risks", "risks",
    "Vaccine safety concerns", "safety",
    "Vaccine skepticism", "skepticism", "skeptic"
  ),
  government = c(
    "Government control agenda",
    "Data manipulation",
    "Secret experiments",
    "Government conspiracy",
    "Government hidden motives",
    "Government cover-up",
    "Government propaganda",
    "Government censorship",
    "Government corruption",
    "COVID government conspiracy",
    "Government interference",
    "Government deceit",
    "Government misinformation"
  ),
pharma = c(
  "Big Pharma profit motive", "Pfizer", "AstraZeneca", "Johnson and Johnson", "Johnson & Johnson", "J and J", "Moderna", "Biontech", 
  "Alternative treatments suppression", "Alternative treatments",
  "Vaccine shortcuts", "Pharma", 
  "Pharmaceutical conspiracy",
  "Pharmaceutical cover-up",
  "Pharmaceutical secrets",
  "Pharmaceutical agenda",
  "Pharmaceutical control",
  "Pharmaceutical manipulation",
  "Pharmaceutical industry corruption", "industry corruption",
  "Pharma influence",
  "Pharma propaganda",
  "Pharma greed"
),
Five_G = c(
  "5G conspiracy", "5G", "Towers", "Masts", "5G masts", "5G towers",
  "COVID and 5G link", "Cell phone", "Cell tower", "Mobile", "Mobile Phone", "Mobile phone mast",
  "EMF radiation effects",
  "Wireless technology dangers", "Wireless", "Radiation", "Microwaves",
  "5G health risks",
  "5G hidden agenda",
  "EMF cover-up", "EMF",
  "Electromagnetic radiation conspiracy", "Electromagnetic", "Electromagnetic radiation", "EMR",
  "Wireless radiation dangers", "Radiation",
  "5G health concerns",
  "Electromagnetic hypersensitivity", "hypersensitivity",
  "EMF radiation risks",
  "Wireless radiation hazards"
),
gates = c(
  "Bill Gates microchipping", "Billgates", "Billgatesfoundation",
  "Depopulation agenda",
  "Gates Foundation conspiracy",
  "Population control conspiracy",
  "Gates vaccination plot",
  "Gates hidden motives",
  "Gates depopulation theory", "Gates depopulation",
  "Bill Gates secret plan",
  "Gates Foundation hidden agenda", "Gates foundation", "Bill Gates foundation", "Melinda", "Melinda Gates", "Bill and Melinda", "Bill & Melinda", "Bill and Melinda Gates",
  "Bill Gates conspiracy",
  "Gates global control",
  "Gates population reduction",
  "Gates vaccine skepticism", "Gates", "Bill Gates"
),
nwo = c(
  "COVID global control",
  "One world government conspiracy", "One world government",
  "Agenda 21 implications", "Agenda 21", "21",
  "Agenda 2030 conspiracy", "Agenda 2030", "2030",
  "NWO hidden agenda",
  "Global governance plot",
  "Globalist control theory", "control theory",
  "New world order conspiracy", "NWO conspiracy", "NWO plot",
  "World domination plan", "World domination", "domination",
  "Global control conspiracy", "Global Control", 
  "World government agenda", "world government", "UN agenda", "WHO agenda",
  "NWO takeover", "NWO", "New World Order", "NewWorldOrder",
  "Globalist power grab", "globalist", "power grab", "power-grab"
),
media = c(
  "Media manipulation",
  "Censorship agenda", "Censorship",
  "Fake news", "Fakenews",
  "Media control",
  "Media bias", "bias", "biased",
  "Information suppression",
  "Misinformation dissemination", "misinformation",
  "Propaganda machine", "propaganda",
  "Media cover-up",
  "Media conspiracy",
  "Censored information", "Censored",
  "Media distortion",
  "News manipulation", "News", "Media",
  "Fox News", "FoxNews", "BBC", "CNN", "Sky News")
)


# Convert categories to lowercase
for (cat_name in names(categories)) {
  categories[[cat_name]] <- tolower(categories[[cat_name]])
}


library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tidyr)

# Create an empty dataframe
entity_category_df <- data.frame(Entity = character(), Category = character(), stringsAsFactors = FALSE)

# Iterate over each row in your data
for (row in 1:nrow(data)) {
  entities <- data[row, "entities"]
  
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


######
library(Matrix)

# Extract entities from the dataset
all_entities <- unlist(strsplit(data$entities, ", "))

# Remove square brackets and speech marks
all_entities <- gsub("\\[|\\]|\"","", all_entities)

# Remove leading and trailing whitespaces
all_entities <- trimws(all_entities)

# Count the frequency of each entity
entity_counts <- table(all_entities)

# Get the top 10 most common entities (excluding empty entities)
top_entities <- names(head(sort(entity_counts[entity_counts != ""], decreasing = TRUE), 10))

# Create an empty co-occurrence matrix
cooc_matrix <- Matrix(0, nrow = length(top_entities), ncol = length(top_entities), dimnames = list(top_entities, top_entities))

# Iterate over each tweet in the dataset
for (i in 1:nrow(data)) {
  entities <- data[i, "entities"]
  
  # Check if the tweet has any entities
  if (entities != "[]") {
    # Remove square brackets and speech marks and split the string into individual entities
    entities <- gsub("\\[|\\]|\"","", entities)
    entities <- strsplit(entities, ", ")[[1]]
    
    # Remove leading and trailing whitespaces from each entity
    entities <- trimws(entities)
    
    # Find the indices of the entities that are in the top list
    entity_indices <- which(entities %in% top_entities)
    
    # Update the co-occurrence matrix for the combinations of entities in the tweet
    if (length(entity_indices) > 1) {
      # Filter the entity indices to only include valid indices within the co-occurrence matrix
      valid_indices <- entity_indices[entity_indices <= length(top_entities)]
      
      # Generate all pairwise combinations of valid entity indices if there are at least 2 valid indices
      if (length(valid_indices) >= 2) {
        pairs <- combn(valid_indices, 2)
        
        # Update the co-occurrence matrix for the valid entity pairs
        for (j in 1:ncol(pairs)) {
          cooc_matrix[top_entities[pairs[1, j]], top_entities[pairs[2, j]]] <- cooc_matrix[top_entities[pairs[1, j]], top_entities[pairs[2, j]]] + 1
          cooc_matrix[top_entities[pairs[2, j]], top_entities[pairs[1, j]]] <- cooc_matrix[top_entities[pairs[2, j]], top_entities[pairs[1, j]]] + 1
        }
      }
    }
  }
}

# Print the co-occurrence matrix
print(cooc_matrix)


library(ggplot2)
library(ggraph)
library(igraph)

# Convert the co-occurrence matrix to an adjacency matrix
adj_matrix <- as.matrix(cooc_matrix)

# Create an igraph graph object from the adjacency matrix
graph <- graph.adjacency(adj_matrix, weighted = TRUE, mode = "undirected")

# Set the vertex size based on the number of appearances of each entity
vertex_size <- entity_counts[top_entities] * 10

# Retrieve the co-occurrence counts from the graph object
edge_counts <- get.edge.attribute(graph, "weight")

# Set the edge width based on the co-occurrence counts
edge_width <- edge_counts * 2

# Create a data frame for the graph nodes (entities)
nodes <- data.frame(name = V(graph)$name, size = vertex_size)

# Create a data frame for the graph edges (co-occurrences)
edges <- data.frame(from = character(0), to = character(0), weight = numeric(0), stringsAsFactors = FALSE)

# Iterate over each edge in the graph
edge_list <- get.edgelist(graph)
for (i in 1:length(edge_counts)) {
  from <- V(graph)$name[edge_list[i, 1]]
  to <- V(graph)$name[edge_list[i, 2]]
  weight <- edge_counts[i]
  edges <- rbind(edges, data.frame(from = from, to = to, weight = weight, stringsAsFactors = FALSE))
}

# Create a separate variable for node size
node_sizes <- nodes$size.Freq
node_sizes <- as.numeric(node_sizes)

# Plot the graph with the modified layout
gg <- ggraph(graph, layout = layout) +
  geom_edge_link(aes(width = weight)) +
  geom_node_point(aes(size = node_sizes)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_width_continuous(range = c(0.5, 3)) +
  scale_size_continuous(range = c(3, 8)) +
  theme_void()

print(gg)









