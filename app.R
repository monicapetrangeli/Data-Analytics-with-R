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
library(ggimage)
library(ggridges)
library(fmsb)
library(treemapify)
library(circlize)
library(wordcloud)
library(RColorBrewer)

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

#Perform K-means Clustering
set.seed(123)
kmeans_result <- kmeans(data_encoded, centers = optimal_k, nstart = 25)

# Define custom colors for each cluster and purchasing category
custom_colors <- c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08d", 
                   "#e6f598", "#abdda4", "#66c2a5", "#3288db", "#5e4fa2")

purchase_colors<-c('#55356E','#E9E2F3','#68A4A5','#F4CFDF','#76A787')

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel('Amazon Customer Behavior Analysis'),
  sidebarPanel(width=3,
               sliderInput('k_slider','Select the number of clusters for the analysis',
                           min=2, max=9, value=3, step=1),
               br(),
               h4('Cluster Legend'),
               uiOutput('clusterLegend')
               ),
  mainPanel(width=9,
            tabsetPanel(
              tabPanel('Cluster Analysis',
                       fluidRow(
                         column(width=12,
                                h4("Analysing the customers' segments demographic distribution", align='left'),
                                br())
                       ),
                       fluidRow(
                         column(width=12,
                                plotOutput('areaPlot'))
                       ),
                       br(),
                       fluidRow(
                         column(width=12,
                                plotOutput('clockView'))
                       ),
                       br(),
                       fluidRow(
                         column(width=12,
                                plotOutput('barChart'))
                       )
                       ),
              tabPanel('Purchasing Behaviour',
                       fluidRow(
                         column(width=12,
                                h4("Analysing the customers' segment purchasing behaviour", align='left'),
                                br()
                         )
                       ),
                       fluidRow(
                         column(width=12,
                                plotOutput('wordMap'))
                       ),
                       br(),
                       fluidRow(
                         column(width=12,
                                plotOutput('donutChart'))
                       ),
                       br(),
                       fluidRow(
                         column(width=12,
                                plotOutput('tornadoChart'))
                       ))
                        )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  cluster_data <- reactive({
    k <- input$k_slider
    set.seed(123)  # For reproducibility
    kmeans_result <- kmeans(data_encoded, centers = k, nstart = 25)
    
    # Create a new data frame with cluster assignments
    data_clustered <- data
    data_clustered$Cluster <- kmeans_result$cluster
    return(data_clustered)
  })
  
  output$clusterLegend <- renderUI({
    k <- input$k_slider
    legend <- lapply(1:k, function(i) {
      color <- custom_colors[i]
      cluster <- paste("Cluster", i)
      tags$div(
        style = paste0("background-color:", color, "; padding: 5px; margin: 5px; display: inline-block;"),
        cluster
      )
    })
    do.call(tagList, legend)
  })
  
  #Plotting an Area Plot to visualize the age distribution of each cluster
  output$areaPlot <- renderPlot({
    current_data <- cluster_data() 
    
    #Calculating the median age per customer
    median_data <- current_data %>%
      group_by(Cluster) %>%
      summarise(median_age = median(age), .groups = "drop")
    
    #Plot
    ggplot(current_data, aes(x = age, y = factor(Cluster), fill = factor(Cluster))) + 
      geom_density_ridges(scale = 2, alpha = 0.7, quantile_lines = TRUE, quantiles = 2) + 
      #Adding the text labels for median values
      geom_text(data = median_data, 
                aes(x = median_age + 5, y = factor(Cluster), 
                    label = paste("Median:", round(median_age, 1))),
                hjust = 0, vjust = 0.5, size = 3.5, fontface = "bold") +
      scale_fill_manual(values = custom_colors) + 
      labs(title = "Age Distribution by Cluster", 
           x = "Age", y = "Cluster") + 
      theme_minimal() + 
      theme(legend.position = "none",
            title = element_text(size = 14, face = "bold"),
            panel.grid.major.y = element_blank()) 
  })
  
  #Plotting a clock view to visualize the customers' hour preferences for purchasing per cluster
  output$clockView <- renderPlot({
    current_data <- cluster_data()  # Make sure to call the reactive expression
    
    #Counting the number of customers per hour per cluster
    hourly_data <- current_data %>%
      group_by(Cluster, hour) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      ungroup()
    
    #Plot
    ggplot(hourly_data, aes(x = factor(hour), y = Count, fill = factor(Cluster))) +
      geom_bar(stat = "identity", width = 0.7) +
      scale_fill_manual(values = custom_colors) +
      coord_polar(start = 0) +
      scale_x_discrete(limits = factor(0:23)) +
      labs(title = "Customer Count by Hour", 
           subtitle = "Visualizing the customers' hour preferences for purchasing per cluster") +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0, color='gray50',face='italic'),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 10)
      ) +
      facet_wrap(~ Cluster, ncol = max(3, round(input$k_slider / 2))) #Ensuring that three clock views are visualized per row, and adjusting based on the number of clusters choosen by the users
  })
  
  #Plotting a stacked bar chart to visualize the gender distribution per cluster
  output$barChart<-renderPlot({
    current_data <- cluster_data()
    
    #Counting the number of females, males, prefer not to say, and others per cluster
    gender_counts <- current_data %>%
      group_by(Cluster, Gender) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      group_by(Cluster) %>%
      mutate(Total = sum(Count),
             Percentage = (Count / Total) * 100) %>%
      ungroup()
    
    #Defining the colors per gender
    gender_colors <- c("Male" = "#B6D8F2", "Female" = "#F4CFDF", "Prefer not to say" = "#cecece", "Others" = "#828282")
    
    #Plot
    ggplot(gender_counts, aes(y = factor(Cluster), x = Percentage, fill = Gender)) +
      geom_bar(stat = "identity", position = "stack", width = 0.6) + 
      scale_fill_manual(values = gender_colors) +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                position = position_stack(vjust = 0.5), color = "black", size = 5) +
      labs(title = "Gender Distribution per Cluster", y='Cluster') + 
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),  
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank(), 
        panel.grid = element_blank(),
        legend.title = element_blank(),
        title = element_text(size = 14, face = "bold")
      )
  }, height = function() {
    100 + input$k_slider * 50 #adjusting the size of the plot based on the number of clusters chosen by the user
  })
  
  #Plotting a word map to visualize the top three reasons to abandon the cart by cluster
  output$wordMap<-renderPlot({
    current_data<-cluster_data()
    
    #Mapping each reason to a shortened version for easier interpretability by the user
    reason_mapping <- c(
      "Found a better price elsewhere" = "price",
      "High shipping costs" = "shipping costs",
      "Changed my mind" = "changed mind",
      "No longer need the item" = "changed mind"
    )
    
    #Applying the mapping to create a new column 'Shortened_Reason'
    current_data$Shortened_Reason <- current_data$Cart_Abandonment_Factors
    current_data$Shortened_Reason <- recode(current_data$Shortened_Reason, !!!reason_mapping)
    
    #Other reasons that do not match will remain others
    current_data$Shortened_Reason[is.na(current_data$Shortened_Reason)] <- "others"
    
    #Calculating the frequency of each shortened reason for each cluster
    abandonment_reasons <- current_data %>%
      filter(!is.na(Shortened_Reason)) %>%
      group_by(Cluster, Shortened_Reason) %>%
      tally() %>%
      top_n(3, n)  #Filtering the top 3 reasons
    
    wordcloud_data <- abandonment_reasons %>%
      mutate(
        Cluster = factor(Cluster),
        Color = custom_colors[Cluster]  # Mapping each cluster to its predefined color
      )
    num_words <- nrow(wordcloud_data)
    height_ratio <- 1
    
    #Plot
    wordcloud(
      words = wordcloud_data$Shortened_Reason,  
      freq = wordcloud_data$n,                  
      min.freq = 1,                             #Minimum frequency to display
      scale = c(7, 2),                          
      colors = wordcloud_data$Color,            
      random.order = FALSE,                     
      rot.per = 0.25,                           
      use.r.layout = FALSE,                     
      ordered.colors = TRUE,                    
      max.words = num_words,                    
      width = 800,                              
      height = num_words * height_ratio         
    )
    mtext("Reasons for Customer Purchases", 
          side = 3,          
          line = 0,          
          adj = 0,           
          col = "black",     
          font = 2,          
          cex = 1.5) 
  })
  
  #Plotting a donuts chart to visualize the distribution of purchasing categories per cluster
  output$donutChart<-renderPlot({
    current_data <- cluster_data()
    
    #Defining the product categories
    category_columns <- c("beauty_and_personal_care", "clothing_and_fashion", 
                          "groceries_and_gourmet_food", "others", "home_and_kitchen")
    
    #Ensuring category_columns is available for use in across
    counts <- current_data %>%
      group_by(Cluster) %>%
      summarise(across(all_of(category_columns), ~ sum(. == 1, na.rm = TRUE))) %>%
      pivot_longer(cols = -Cluster, names_to = "Category", values_to = "Count")
    
    #Calculating the total number of rows per cluster
    total_counts <- counts %>%
      group_by(Cluster) %>%
      summarise(Total = sum(Count))
    
    #Adding percentage to each count
    counts <- counts %>%
      left_join(total_counts, by = "Cluster") %>%
      mutate(Percentage = (Count / Total) * 100) %>%
      arrange(Cluster, desc(Percentage)) %>%  # Arrange for proper stacking
      group_by(Cluster) %>%
      mutate(ypos = cumsum(Percentage) - (Percentage / 2))  # Position for text labels

    
    num_clusters <- length(unique(counts$Cluster))
    
    #Dynamically adjusting the text size
    dynamic_size <- case_when(
      num_clusters <= 4  ~ 6, 
      num_clusters <= 8  ~ 5,  
      num_clusters <= 12 ~ 4,  
      TRUE              ~ 3   
    )
    
    #Plot
    ggplot(counts, aes(x = "", y = Percentage, fill = Category)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y", start = 0) +  
      scale_fill_manual(values = purchase_colors) +
      facet_wrap(~ Cluster, ncol = max(3, round(num_clusters / 2))) + #Similarly to the clock view graphs we are visualizing minimum three charts per row and adjusting this number based on the number of clusters chosen
      labs(title = "Product Category Distribution by Cluster",
           subtitle="Visualizing each cluster's product category preference for browsing and purchasing") +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face='italic'),
        legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0, color='gray50',face='italic')
      ) +
      #Adding the percentage labels with dynamic size
      geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
                position = position_stack(vjust = 0.5), 
                color = "white", size = dynamic_size) +
      #Adding a white circle in the center to create the donut effect
      annotate("text", x = 0, y = 0, label = "", color = "white")
  })
  
  #Plotting the average purchasing and browsing frequency per cluster
  output$tornadoChart<-renderPlot({
    current_data<-cluster_data()
    
    #Mapping the categorical values to numeric values to be able to calculate the average within clusters
    purchase_frequency_mapping <- c(
      "Less than once a month" = 1,
      "Once a month" = 2,
      "Few times a month" = 3,
      "Once a week" = 4,
      "Multiple times a week" = 5
    )
    
    browsing_frequency_mapping <- c(
      "Rarely" = 1,
      "Few times a month" = 2,
      "Few times a week" = 3,
      "Once a week" = 4,
      "Multiple times a week" = 5
    )
    
    #Applying the mapping to the columns, ensuring that NAs are handled
    current_data$Purchase_Frequency_Num <- purchase_frequency_mapping[current_data$Purchase_Frequency]
    current_data$Browsing_Frequency_Num <- browsing_frequency_mapping[current_data$Browsing_Frequency]
    
    #Checking for any NAs in the transformed data and remove rows with NA in Purchase or Browsing Frequency
    current_data <- current_data %>%
      filter(!is.na(Purchase_Frequency_Num) & !is.na(Browsing_Frequency_Num))
    
    #Calculating the mean of the numeric values for each cluster
    cluster_summary <- current_data %>%
      group_by(Cluster) %>%
      summarize(
        Purchase_Frequency = mean(Purchase_Frequency_Num, na.rm = TRUE),
        Browsing_Frequency = mean(Browsing_Frequency_Num, na.rm = TRUE)
      )
    
    data_diverging <- cluster_summary %>%
      mutate(
        Cluster = paste0('Cluster',Cluster),
        Browsing_Frequency = -Browsing_Frequency  #Making the browsing frequency negative to visualize it together with the purchasing frequency
      ) %>%
      pivot_longer(
        cols = c(Purchase_Frequency, Browsing_Frequency),
        names_to = "Metric",
        values_to = "Value"
      )
    
    y_breaks <- seq(-5, 5, 1)
    y_labels <- abs(y_breaks)
    
    #Plot
    ggplot(data_diverging, aes(x = Cluster, y = Value, fill = Cluster)) +
      geom_bar(stat = "identity", width = 0.7, aes(alpha = Metric)) +
      scale_fill_manual(values = custom_colors) +
      scale_alpha_manual(values = c("Purchase_Frequency" = 1, "Browsing_Frequency" = 0.6),
                         labels = c("Purchase Frequency", "Browsing Frequency")) +
      scale_y_continuous(
        breaks = y_breaks,
        labels = y_labels,
        limits = c(min(data_diverging$Value) - 0.5, max(data_diverging$Value) + 0.5)
      ) +
      coord_flip() +
      labs(
        title = "Purchase vs. Browsing Frequency by Cluster",
        subtitle = "Visualizing the relationship between purchasing and browsing frequency per cluster",
        x = "",
        y = "Purchasing Frequency                                       Browsing Frequency",
        fill = "Cluster"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 12, hjust = 0, color='gray50',face='italic'),
        plot.margin = margin(20, 20, 20, 20),
      ) +
      # Add vertical line at zero
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
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
