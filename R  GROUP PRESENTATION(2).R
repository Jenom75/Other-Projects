                          # A. GETTING THE DATASET


# loading the necessary packages
library(tidyverse)
library(lubridate)
library(summarytools)
library(Amelia)
library(dplyr)
library(stringr)
library(shiny)
library(ggcorrplot)

# Setting our working directory
#setwd("C:/Users/ibrah/OneDrive/Documents/R Projects/Day 3")

# Checking the working directory
getwd()

# Loading the data on R
df <- read_csv("1- mental-illnesses-prevalence_Assignment.csv")


                     # B. PERFORMING EXPLORATORY DATA ANALYSIS

# Checking the first few rows of the dataset
head(df)

# Checking the last few rows of the dataset
tail(df)

# Checking the structure of the dataset
str(df)


# Using glimpse to get a summary of the dataset
glimpse(df)

# Checking the summary of the dataset
summary(df)

# checking the dimensions of the dataset
dim(df)


# Summarising the dataset
summarytools:: dfSummary(df) %>% print(method = "viewer")

# Checking for missing values
sum(is.na(df))

# checking for columns missing data
colSums(is.na(df))

# Using pipe to summarise missing values
df %>% summarise_all(~ sum(is.na(.)))

# Visualizing the columns with missing values
missmap(df, main = "Missing values vs observed")

# Checking for duplicates
sum(duplicated(df))

# Replacing missing values in Numerical columns with zero
df <- df %>% 
  mutate(across(where(is.numeric), ~ replace_na(.,0)))

# Replacing missing values in Categorical columns with "Unknown"
df <- df %>% 
  mutate(across(where(is.character), ~ replace_na(.,"Unknown")))


                          # C. KPIs


# Separating the countries from the continents in the dataset
countries_df <- df %>% 
  filter(!str_detect(Entity, "\\(IHME GBD\\)$"))  # Exclude rows with "(IHME GBD)"

countries_df

View(countries_df)

# Separating the continents from the countries in the dataset
continents_df <- df %>% 
  filter(str_detect(Entity, "\\(IHME GBD\\)$"))  

continents_df
view(continents_df)

# Total number of Countries in the dataset
n_distinct(countries_df$Entity)

# Total number of Countries in the dataset
n_distinct(continents_df$Entity)

                            # D. GETTING INSIGHTS FROM THE DATASET

                  # 1. Each continent with its most prevailing mental illness


# Inspect data after filtering
print(head(continents_df))

# Summarize data for continents
continents_summary <- continents_df %>%
  group_by(Entity) %>%
  summarise(
    Schizophrenia = mean(as.numeric(`Schizophrenia disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
    Depression = mean(as.numeric(`Depressive disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
    Anxiety = mean(as.numeric(`Anxiety disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
    Bipolar = mean(as.numeric(`Bipolar disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
    Eating_Disorder = mean(as.numeric(`Eating disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE)
  )
print(continents_summary)

# Reshape the summarized data
reshaped_data <- continents_summary %>%
  pivot_longer(
    cols = c(Schizophrenia, Depression, Anxiety, Bipolar, Eating_Disorder),
    names_to = "Illness",
    values_to = "Count"
  )
print(reshaped_data)

# Verify plot
ggplot(reshaped_data, aes(x = Entity, y = Count, fill = Illness)) +  
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Prevalence of Mental Illnesses by Continent",
    x = "Continents",
    y = "Average Prevalence (Share of Population)",
    fill = "Mental Illness"
  )


                        # 2. Time series analysis of each illness over time

  
  countries_df %>%
    group_by(Year) %>%
    summarise(
      Schizophrenia = mean(as.numeric(`Schizophrenia disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
      Depression = mean(as.numeric(`Depressive disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
      Anxiety = mean(as.numeric(`Anxiety disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
      Bipolar = mean(as.numeric(`Bipolar disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
      Eating_Disorder = mean(as.numeric(`Eating disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE)
    ) %>%
    pivot_longer(cols = -Year, names_to = "Disease", values_to = "Prevalence") %>%
    ggplot(aes(x = Year, y = Prevalence, color = Disease)) + 
    geom_line() + 
    labs(title = 'Disease Trend Over Time', x = 'Year', y = 'Prevalence (%)') +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Load necessary libraries
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  
  # UI: User Interface
  ui <- fluidPage(
    # Application title
    titlePanel("Disease Trend Over Time"),
    
    # Sidebar layout with filters for countries and continents
    sidebarLayout(
      sidebarPanel(
        selectInput("continent", "Select Continent:", 
                    choices = unique(df$Continent), 
                    selected = unique(df$Continent)[1]),  # Default selection
        selectInput("country", "Select Country:", 
                    choices = NULL,  # Initially empty, will be populated based on selected continent
                    selected = NULL)
      ),
      
      # Main panel to display the plot
      mainPanel(
        plotOutput("disease_plot")
      )
    )
  )
  
  # Server: Define server logic required to draw the plot
  server <- function(input, output, session) {
    
    # Update country choices based on the selected continent
    observe({
      continent_selected <- input$continent
      countries_in_continent <- unique(df$Country[df$Continent == continent_selected])
      updateSelectInput(session, "country", choices = countries_in_continent)
    })
    
    # Reactive plot based on the selected country and continent
    output$disease_plot <- renderPlot({
      # Filter data based on selected country and continent
      filtered_data <- df %>%
        filter(Continent == input$continent & Country == input$country)
      
      # Summarize the data
      summarized_data <- filtered_data %>%
        group_by(Year) %>%
        summarise(
          Schizophrenia = mean(as.numeric(`Schizophrenia disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
          Depression = mean(as.numeric(`Depressive disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
          Anxiety = mean(as.numeric(`Anxiety disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
          Bipolar = mean(as.numeric(`Bipolar disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
          Eating_Disorder = mean(as.numeric(`Eating disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE)
        ) %>%
        pivot_longer(cols = -Year, names_to = "Disease", values_to = "Prevalence")
      
      # Plotting the data
      ggplot(summarized_data, aes(x = Year, y = Prevalence, color = Disease)) + 
        geom_line() + 
        labs(title = paste("Disease Trend in", input$country, "-", input$continent),
             x = 'Year', y = 'Prevalence (%)') +
        theme_minimal() +
        theme(legend.position = "bottom")
    })
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
  
  
  
                               # 3. Correlation between different illness
  
  
  # Summarize disease data by continent or another grouping variable
  summarized_df <- continents_df %>%
    group_by(Entity) %>%  # Replace 'Entity' with 'Continent' if appropriate
    summarise(
      Schizophrenia = mean(as.numeric(`Schizophrenia disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
      Depression = mean(as.numeric(`Depressive disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
      Anxiety = mean(as.numeric(`Anxiety disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
      Bipolar = mean(as.numeric(`Bipolar disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
      Eating = mean(as.numeric(`Eating disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE)
    )
  
  # Drop the grouping column for correlation analysis
  cor_matrix <- cor(summarized_df %>% select(-Entity), use = "complete.obs", method = "pearson")
  print(cor_matrix)
  
  
  ggcorrplot(cor_matrix, 
             method = "circle", 
             type = "lower", 
             lab = TRUE, 
             title = "Correlation Between Diseases")
  
  
  
                    # 4.Countries with the highest prevalence of specific diseases
  
  
  
  # Select the top 3 countries with the highest prevalence for each disease
  top_3_countries <- countries_df %>%
    select(
      Entity,
      Schizophrenia = `Schizophrenia disorders (share of population) - Sex: Both - Age: Age-standardized`,
      Depression = `Depressive disorders (share of population) - Sex: Both - Age: Age-standardized`,
      Anxiety = `Anxiety disorders (share of population) - Sex: Both - Age: Age-standardized`,
      Bipolar = `Bipolar disorders (share of population) - Sex: Both - Age: Age-standardized`,
      Eating = `Eating disorders (share of population) - Sex: Both - Age: Age-standardized`
    ) %>%
    pivot_longer(cols = -Entity, names_to = "Disease", values_to = "Prevalence") %>%
    group_by(Disease) %>%
    arrange(desc(Prevalence)) %>%
    slice_head(n = 3) %>%
    ungroup()
  
  # Create the bubble chart
  library(ggplot2)
  
  ggplot(top_3_countries, aes(x = Disease, y = Prevalence, size = Prevalence, color = Entity)) +
    geom_point(alpha = 0.7) +
    labs(
      title = "Top 3 Countries with the Highest Prevalence of Specific Diseases",
      x = "Disease",
      y = "Prevalence (%)",
      size = "Prevalence",
      color = "Country"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  
  
  
  
  # Select the top 3 countries with the highest prevalence for each disease
  top_3_countries <- df %>%
    select(
      Entity,
      Schizophrenia = `Schizophrenia disorders (share of population) - Sex: Both - Age: Age-standardized`,
      Depression = `Depressive disorders (share of population) - Sex: Both - Age: Age-standardized`,
      Anxiety = `Anxiety disorders (share of population) - Sex: Both - Age: Age-standardized`,
      Bipolar = `Bipolar disorders (share of population) - Sex: Both - Age: Age-standardized`,
      Eating = `Eating disorders (share of population) - Sex: Both - Age: Age-standardized`
    ) %>%
    pivot_longer(cols = -Entity, names_to = "Disease", values_to = "Prevalence") %>%
    group_by(Disease) %>%
    arrange(desc(Prevalence)) %>%
    slice_head(n = 3) %>%
    ungroup()
  
  # Create the bubble chart
  library(ggplot2)
  
  ggplot(top_3_countries, aes(x = Disease, y = Prevalence, size = Prevalence, color = Entity)) +
    geom_point(alpha = 0.7) +
    labs(
      title = "Top 3 Countries with the Highest Prevalence of Specific Diseases",
      x = "Disease",
      y = "Prevalence (%)",
      size = "Prevalence",
      color = "Country"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Data Cleaning: Convert columns to numeric and handle non-numeric entries
  df_clean <- df %>%
    mutate(
      Schizophrenia = as.numeric(`Schizophrenia disorders (share of population) - Sex: Both - Age: Age-standardized`),
      Depression = as.numeric(`Depressive disorders (share of population) - Sex: Both - Age: Age-standardized`),
      Anxiety = as.numeric(`Anxiety disorders (share of population) - Sex: Both - Age: Age-standardized`),
      Bipolar = as.numeric(`Bipolar disorders (share of population) - Sex: Both - Age: Age-standardized`),
      Eating = as.numeric(`Eating disorders (share of population) - Sex: Both - Age: Age-standardized`)
    ) %>%
    filter(!is.na(Entity))  # Remove rows with missing countries
  
  # Select the top 3 countries with the highest prevalence for each disease
  top_3_countries <- df_clean %>%
    select(
      Entity,
      Schizophrenia, Depression, Anxiety, Bipolar, Eating
    ) %>%
    pivot_longer(cols = -Entity, names_to = "Disease", values_to = "Prevalence") %>%
    filter(!is.na(Prevalence) & Prevalence > 0) %>%  # Exclude invalid or zero prevalence
    group_by(Disease) %>%
    arrange(desc(Prevalence)) %>%
    slice_head(n = 3) %>%
    ungroup()
  
  # Inspect the cleaned data
  print(top_3_countries)

  
  # Create the bubble chart
  library(ggplot2)
  
  ggplot(top_3_countries, aes(x = Disease, y = Prevalence, size = Prevalence, color = Entity)) +
    geom_point(alpha = 0.7) +
    labs(
      title = "Top 3 Countries with the Highest Prevalence of Specific Diseases",
      x = "Disease",
      y = "Prevalence (%)",
      size = "Prevalence",
      color = "Country"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
    
  
  
  
  
  
  
  
  
  
  # Summarize data by country and disease
  countries_df %>%
    group_by(Entity) %>%
    summarise(
      Schizophrenia = mean(as.numeric(`Schizophrenia disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
      Depression = mean(as.numeric(`Depressive disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
      Anxiety = mean(as.numeric(`Anxiety disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
      Bipolar = mean(as.numeric(`Bipolar disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE),
      Eating = mean(as.numeric(`Eating disorders (share of population) - Sex: Both - Age: Age-standardized`), na.rm = TRUE)
    ) %>%
    # Reshape the data into long format to have one column for disease and one for prevalence
    pivot_longer(cols = c(Schizophrenia, Depression, Anxiety, Bipolar, Eating),
                 names_to = "Disease", values_to = "Prevalence") %>%
    # For each disease, get the top 3 countries based on prevalence
    group_by(Disease) %>%
    top_n(3, Prevalence) %>%
    # Create a new column for the rank/position of each country within each disease
    mutate(Rank = rank(-Prevalence, ties.method = "min")) %>%
    # Plot the data using a bar chart with the disease categories
    ggplot(aes(x = reorder(Entity, Prevalence), y = Prevalence, fill = Disease)) +  
    geom_bar(stat = "identity", position = "dodge") +  # Bar chart with dodge position
    geom_text(aes(label = Rank), size = 4, color = "black", fontface = "bold", vjust = 0.5) +  # Place the rank inside bars
    labs(title = "Top 3 Countries for Each Disease", 
         x = "Country", 
         y = "Prevalence (%)") +
    theme_minimal() +
    theme(legend.position = "top",  # Position the legend at the top
          axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
          strip.text = element_text(size = 12),  # Make facet labels bigger
          axis.text.y = element_text(size = 10))  # Adjust y-axis text size