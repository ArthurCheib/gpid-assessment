# Script Name: FGT_calculator.R
# Author: Arthur Cheib
# Date: Nov 10, 2023
# Description: script that creates a function to calculate the Foster–Greer–Thorbecke indices for a given
#             household survey
#
# More about it: The Foster–Greer–Thorbecke indices are a family of poverty metrics.
# The most commonly used index from the family, FGT2, puts higher weight on the poverty of the poorest
# individuals, making it a combined measure of poverty and income inequality and a popular choice within
# development economics. The indices were introduced in a 1984 paper by economists Erik Thorbecke,
# Joel Greer, and James Foster.

# Dependencies: `readr`, 'here'
library(dplyr)
library(tidyr)

# Example usage: 
# fgt_table <- FGT_calculator(survey_data = household_survey_data)

# ----------------------------------------------------------

# Function: FGT_calculator
# Description: This function calculates the chosen Foster–Greer–Thorbecke indices for a given household survey
#
# Parameters:
#   `survey_data` - household survey data containing the three necessary columns for calculating the indices
#
# Returns:
#   a table (tibble format) containing the three FGT indices for every given year contained in the survey data

FGT_calculator <- function(survey_data, year, poverty_line) {
  
  # Filtering the survey data for a given year
  survey_year <- survey_data[[year]] |> 
    as_tibble()
  
  # Identifying the poors in the population + relevant info about them + their weights to be included
  poors <- survey_year[['income']] < poverty_line
  poors_income <- survey_year[poors, 'income']
  poors_weights <- survey_year[poors, 'weight']
  weights <- survey_year[['weight']]
  
  # Parameters for the fgt function
  denominator <- poverty_line
  numerator <- (poverty_line - poors_income)
  
  ## Calculating a given FGT (weighted)
  
  # Creating an inner function to integrate the calculation for a given alpha
  inner_helper_FGT <- function(alpha) {
    
    FGT <- (sum((poors_weights) * (numerator / denominator) ** alpha) / sum(weights))
    
    return(FGT)
    
  }
  
  ## What to return for: a table with household data, for a given year and a given poverty line
  data <- tibble(year = as.numeric(str_remove(year, 'Y')),
                 pov_line = poverty_line,
                 headcount = inner_helper_FGT(alpha = 0),
                 povgap = inner_helper_FGT(alpha = 1),
                 povseverity = inner_helper_FGT(alpha = 2))
  
  return(data)
  
}

# End of script
