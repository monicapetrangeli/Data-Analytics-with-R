library(shiny)
library(dplyr)
library(tidyverse)
library(lubridate)
library(pacman)
library(cluster)
library(factoextra)
library(ggplot2)
library(plotly)
library(networkD3)
library(stringr)
library(htmlwidgets)
library(scales)
library(tidyr)


#Importing the data set
data <- read.csv('Amazon Customer Behavior Survey.csv')

# Check for missing data
print(sum(is.na(data)))

# Step 1: Split and clean purchase categories
purchase_categories <- data$Purchase_Categories %>%
  str_split(pattern = ";") %>%
  unlist() %>%
  str_trim() %>%              # Remove leading/trailing whitespace
  unique()

# Step 2: Create boolean columns for each category
list_purchase_categories <- map(.x = purchase_categories, 
                                .f = ~ str_detect(str_trim(data$Purchase_Categories), 
                                                  regex(.x, ignore_case = TRUE)))

# Step 3: Clean column names (replace spaces/dots with underscores and convert to lowercase)
names(list_purchase_categories) <- purchase_categories %>%
  str_replace_all("[[:space:]]+", "_") %>%
  str_replace_all("[[:punct:]]+", "_") %>%
  tolower()

# Step 4: Bind new boolean columns to original data
data <- data %>%
  bind_cols(as.data.frame(list_purchase_categories)) %>%
  select(-Purchase_Categories)

#Feature engineering date feature to extract date and time
data$Timestamp<-data$Timestamp%>%
  gsub(pattern='GMT.*',replacement='',x=.)%>%
  as.POSIXct(format= "%Y/%m/%d %I:%M:%S %p")
#Extracting the date
data$Date<-data$Timestamp%>%
  as.Date()
data$year<-year(data$Timestamp)
data$month<-month(data$Timestamp)
data$day<-day(data$Timestamp)
#Extract the time
data$hour<-hour(data$Timestamp)
data$minute<-minute(data$Timestamp)
data$second<-second(data$Timestamp)
data<-select(data,-Timestamp)

#Setting the seed for reproducibility
set.seed(123)
#Clustering the data set using K- mean algorithm
#Elbow method will be used to determine the optimal number of clusters (k)

#Encoding the categorical variables
data_encoded<-data%>%
  mutate(
    Gender=as.numeric(factor(data$Gender)),
    Purchase_Frequency=as.numeric(factor(data$Purchase_Frequency,levels=c('Multiple times a week','Once a week','Few times a month','Once a month','Less than once a month'))),
    Personalized_Recommendation_Frequency=as.numeric(factor(data$Personalized_Recommendation_Frequency,levels=c('Yes','Sometimes','No'))),
    Browsing_Frequency=as.numeric(factor(data$Browsing_Frequency,levels=c('Multiple times a day','Few times a week','Few times a month','Rarely'))),
    Product_Search_Method=as.numeric(factor(data$Product_Search_Method)),
    Search_Result_Exploration=as.numeric(factor(data$Search_Result_Exploration)),
    Add_to_Cart_Browsing=as.numeric(factor(data$Add_to_Cart_Browsing,levels=c('Yes','Maybe','No'))),
    Cart_Completion_Frequency=as.numeric(factor(data$Cart_Completion_Frequency,levels=c('Always','Often','Sometimes','Rarely','Never'))),
    Cart_Abandonment_Factors=as.numeric(factor(data$Cart_Abandonment_Factors)),
    Saveforlater_Frequency=as.numeric(factor(data$Saveforlater_Frequency,levels=c('Always','Often','Sometimes','Rarely','Never'))),
    Review_Left=as.numeric(factor(data$Review_Left)),
    Review_Reliability=as.numeric(factor(data$Review_Reliability,levels=c('Heavily','Moderately','Occasionally','Rarely','Never'))),
    Review_Helpfulness=as.numeric(factor(data$Review_Helpfulness,levels=c('Yes','Sometimes','No'))),
    Recommendation_Helpfulness=as.numeric(factor(data$Recommendation_Helpfulness,levels=c('Yes','Sometimes','No'))),
    Service_Appreciation=as.numeric(factor(data$Service_Appreciation)),
    Improvement_Areas=as.numeric(factor(data$Improvement_Areas))
  )%>%
  select(-Date)   #Removing the date feature as it is not required for clustering

#Mutating the logical columns to numeric
data_encoded<-data_encoded %>%
  mutate(across(where(is.logical), as.numeric))

#Elbow Method
wss <- sapply(1:10, function(k) {
  kmeans(data_encoded, centers = k, nstart = 25)$tot.withinss
})
#Plotting the Elbow Method
plot(1:10, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within Sum of Squares")

optimal_k <- which(diff(diff(wss)) == min(diff(diff(wss)))) + 1

# Perform K-means Clustering
set.seed(123)
kmeans_result <- kmeans(data_encoded, centers = optimal_k, nstart = 25)

# Add cluster assignments to original data
data$Cluster <- kmeans_result$cluster


counts <- data %>%
  group_by(Cluster) %>%
  summarise(across(
    c(beauty_and_personal_care, clothing_and_fashion, groceries_and_gourmet_food, others, home_and_kitchen),
    ~ sum(. == TRUE)
  ))

# Step 2: Convert counts to percentages within each cluster
cluster_percentages <- counts %>%
  mutate(across(-Cluster, ~ . / sum(.) * 100)) %>%
  pivot_longer(-Cluster, names_to = "Category", values_to = "Percentage")

# Step 3: Create the horizontal stacked bar chart
ggplot(cluster_percentages, aes(y = factor(Cluster), x = Percentage, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3) +
  labs(title = "Stacked Bar Chart of Category Search Frequency by Cluster",
       y = "Cluster", 
       x = "Percentage") +
  scale_fill_brewer(palette = "Set2") +  # Optional color palette
  theme_minimal()

daily_counts <- data %>%
  group_by(day) %>%
  summarise(
    beauty_and_personal_care = sum(beauty_and_personal_care == TRUE),
    clothing_and_fashion = sum(clothing_and_fashion == TRUE),
    groceries_and_gourmet_food = sum(groceries_and_gourmet_food == TRUE),
    others = sum(others == TRUE),
    home_and_kitchen = sum(home_and_kitchen == TRUE)
  ) %>%
  pivot_longer(
    cols = -day, 
    names_to = "Category", 
    values_to = "Count"
  )

# Step 3: Plot the line chart
ggplot(daily_counts, aes(x = day, y = Count, color = Category)) +
  geom_line(size = 1) +
  geom_point(size = 2) + 
  labs(
    title = "Daily Searches by Category",
    x = "Day of the Month",
    y = "Number of Searches"
  ) +
  scale_color_brewer(palette = "Set2") +  # Optional color palette
  theme_minimal()

age_gender_data <- data %>%
  group_by(Cluster, Gender, age) %>%
  summarise(Count = n(), .groups = 'drop')  # Count number of people per age and gender in each cluster

# Step 2: Plot the area graph
ggplot(data, aes(x = factor(Cluster), y = age, fill = Gender)) +
  geom_violin(trim = FALSE, alpha = 0.4) +  # Keep full shape and make it transparent
  labs(
    title = "Age Distribution by Cluster and Gender",
    x = "Cluster",
    y = "Age"
  ) +
  scale_fill_manual(
    values = c("Female" = "pink", "Male" = "blue", "Prefer not to say" = "gray")
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    strip.text.y = element_text(angle = 0, size = 10),  
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank(),  
    panel.grid.minor.y = element_blank(),  
    panel.grid.major.y = element_line(color = "gray", linewidth = 0.5)
  )
# Function to create diverging bar chart
create_diverging_chart <- function(data_with_clusters, input_clusters = NULL) {
  # Create a copy of data with cluster information
  data_encoded_copy <- data_encoded
  data_encoded_copy$Cluster <- data_with_clusters$Cluster
  
  # Filter by number of clusters if specified
  if (!is.null(input_clusters)) {
    data_encoded_copy <- data_encoded_copy %>% filter(as.numeric(Cluster) <= input_clusters)
  }
  
  # Summarize data by cluster
  cluster_summary <- data_encoded_copy %>%
    group_by(Cluster) %>%
    summarize(
      Purchase_Frequency = mean(Purchase_Frequency, na.rm = TRUE),
      Browsing_Frequency = mean(Browsing_Frequency, na.rm = TRUE)
    )
  
  # Create a diverging data frame
  # We'll use Purchase_Frequency as positive values and Browsing_Frequency as negative values
  data_diverging <- cluster_summary %>%
    mutate(
      Cluster = paste0("Cluster ", Cluster),
      Browsing_Frequency = -Browsing_Frequency  # Make browsing frequency negative for diverging effect
    ) %>%
    pivot_longer(
      cols = c(Purchase_Frequency, Browsing_Frequency),
      names_to = "Metric",
      values_to = "Value"
    )
  
  # Create labels for the y-axis
  y_breaks <- seq(-5, 5, 1)
  y_labels <- abs(y_breaks)
  
  # Create the diverging bar chart
  p <- ggplot(data_diverging, aes(x = reorder(Cluster, desc(Cluster)), y = Value, fill = Metric)) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_manual(
      values = c("Purchase_Frequency" = "#4472C4", "Browsing_Frequency" = "#ED7D31"),
      labels = c("Purchase Frequency", "Browsing Frequency")
    ) +
    scale_y_continuous(
      breaks = y_breaks,
      labels = y_labels,
      limits = c(min(data_diverging$Value) - 0.5, max(data_diverging$Value) + 0.5)
    ) +
    coord_flip() +
    labs(
      title = "Purchase vs. Browsing Frequency by Cluster",
      subtitle = "Higher values indicate stronger engagement",
      x = "",
      y = "Frequency",
      fill = "Metric"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 10, hjust = 0),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    # Add a vertical line at zero
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    # Add value labels
    geom_text(
      aes(
        label = sprintf("%.1f", abs(Value)),
        hjust = ifelse(Value >= 0, -0.3, 1.3)
      ),
      size = 3.5,
      color = "black"
    )
  
  return(p)
}

# Function to create Sankey diagram
create_sankey_diagram <- function(data_with_clusters, input_clusters = NULL) {
  # Create a copy of data with cluster information
  data_encoded_copy <- data_encoded
  data_encoded_copy$Cluster <- data_with_clusters$Cluster
  
  # Filter by number of clusters if specified
  if (!is.null(input_clusters)) {
    data_encoded_copy <- data_encoded_copy %>% filter(as.numeric(Cluster) <= input_clusters)
  }
  
  # Prepare data relating clusters to purchase categories
  category_data <- data_encoded_copy %>%
    select(Cluster, 
           beauty_and_personal_care,
           clothing_and_fashion, 
           groceries_and_gourmet_food, 
           others, 
           home_and_kitchen) %>%
    group_by(Cluster) %>%
    summarize(
      beauty_and_personal_care = mean(beauty_and_personal_care, na.rm = TRUE),
      clothing_and_fashion = mean(clothing_and_fashion, na.rm = TRUE),
      groceries_and_gourmet_food = mean(groceries_and_gourmet_food, na.rm = TRUE),
      others = mean(others, na.rm = TRUE),
      home_and_kitchen = mean(home_and_kitchen, na.rm = TRUE)
    )
  
  # Convert to long format for Sankey diagram
  sankey_data <- category_data %>%
    mutate(Cluster = paste0("Cluster ", Cluster)) %>%
    pivot_longer(
      cols = -Cluster,
      names_to = "Category",
      values_to = "Value"
    ) %>%
    # Clean up category names for display
    mutate(
      Category = str_replace_all(Category, "_", " "),
      Category = str_to_title(Category)
    )
  
  # Create nodes dataframe with group information
  nodes <- data.frame(
    name = c(unique(sankey_data$Cluster), unique(sankey_data$Category)),
    group = c(rep("Cluster", length(unique(sankey_data$Cluster))), 
              rep("Category", length(unique(sankey_data$Category))))
  )
  
  # Create links dataframe
  links <- data.frame(
    source = match(sankey_data$Cluster, nodes$name) - 1,
    target = match(sankey_data$Category, nodes$name) - 1,
    value = sankey_data$Value * 15  # Scale values for better visualization
  )
  
  # Custom color palette
  cluster_colors <- colorRampPalette(c("#2E86C1", "#1ABC9C"))(length(unique(sankey_data$Cluster)))
  category_colors <- colorRampPalette(c("#F39C12", "#E74C3C"))(length(unique(sankey_data$Category)))
  
  # Create custom JS for coloring
  color_scale <- paste0(
    "d3.scaleOrdinal()
      .domain([", paste(shQuote(as.character(nodes$name)), collapse = ", "), "])
      .range([", paste(shQuote(c(cluster_colors, category_colors)), collapse = ", "), "])"
  )
  
  # Create the enhanced Sankey diagram
  sankey_plot <- sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    NodeGroup = "group",
    fontSize = 15,
    nodeWidth = 35,
    nodePadding = 25,
    height = 600,
    width = 800,
    sinksRight = TRUE,
    colourScale = JS(color_scale),
    iterations = 32
  )
  
  # Add custom CSS styling for interactive elements
  sankey_plot <- onRender(
    sankey_plot,
    '
    function(el, x) {
      // Customize text labels
      d3.select(el).selectAll(".node text")
        .style("font-weight", "bold")
        .style("text-shadow", "1px 1px 1px rgba(0, 0, 0, 0.2)");
      
      // Customize node appearance
      d3.select(el).selectAll(".node rect")
        .style("stroke", "#555")
        .style("stroke-width", "1px")
        .style("opacity", 0.9);
        
      // Add hover effects
      d3.select(el).selectAll(".node")
        .on("mouseover", function() {
          d3.select(this).select("rect")
            .style("stroke-width", "2px")
            .style("opacity", 1);
        })
        .on("mouseout", function() {
          d3.select(this).select("rect")
            .style("stroke-width", "1px")
            .style("opacity", 0.9);
        });
        
      // Customize link appearance
      d3.select(el).selectAll(".link")
        .style("stroke-opacity", 0.6)
        .on("mouseover", function() {
          d3.select(this)
            .style("stroke-opacity", 0.8);
        })
        .on("mouseout", function() {
          d3.select(this)
            .style("stroke-opacity", 0.6);
        });
    }
    '
  )
  
  # Add a title
  sankey_plot <- htmlwidgets::prependContent(
    sankey_plot,
    htmltools::tags$h2(
      "Customer Cluster to Purchase Category Relationships",
      style = "text-align: center; font-family: Arial; color: #2C3E50;"
    )
  )
  
  return(sankey_plot)

