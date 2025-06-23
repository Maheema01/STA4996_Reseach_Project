# Load necessary libraries
library(readxl)
library(dplyr)
#install.packages("tidyr")
library(tidyr)

# Read the Excel file
data <- read_excel("C:/Users/DELL/Downloads/nutrition.xlsx", sheet = "final_data (1)")

# Select the relevant columns
selected_data <- data %>%
  select(Country, Stunting_survey, Wasting_survey, Overweight_survey, Underweight_survey)

# Calculate the percentage of non-missing values for each variable by country
percentage_data <- selected_data %>%
  group_by(Country) %>%
  summarise(
    Stunting_percent = mean(!is.na(Stunting_survey)) * 100,
    Wasting_percent = mean(!is.na(Wasting_survey)) * 100,
    Overweight_percent = mean(!is.na(Overweight_survey)) * 100,
    Underweight_percent = mean(!is.na(Underweight_survey)) * 100
  )

# Filter countries with more than 50% data for any of the variables
filtered_countries <- percentage_data %>%
  filter(
    Stunting_percent > 30 |
      Wasting_percent > 30 |
      Overweight_percent > 30 |
      Underweight_percent > 30
  )

# Print the result
print(filtered_countries,n=30)

