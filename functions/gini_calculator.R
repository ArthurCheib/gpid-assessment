# Script Name: gini_calculator.R
# Author: Arthur Cheib
# Date: Nov 10, 2023
# Description: script that creates a function to compute the gini coeffiecint

# Dependencies: none

# Example usage: 
# gini_coef <- gini_calculator(household_data[['year']]))

# ----------------------------------------------------------

# Function: get_data
# Description: This function computes the gini coefficient from a given household survey data
#
# Parameters:
#   x = the household survey data
#
# Returns:
#   The gini coefficient (numeric single vector)

gini_calculator <- function(data) {
  
  data <- as_tibble(data)
  
  # Separate all the necessary parameters
  ordered_data <- order(data[['income']])
  income <- data[['income']]
  weight <- data[['weight']]
  
  # Calculate cumulative proportions
  # Xk =is the cumulated proportion of the population variable (we used the weights in our case)
  cum_weight <- cumsum(weight) / sum(weight)
  # Y_k = is the cumulated proportion of the income variable, for k = 0,...,n, with Y0 = 0, Yn = 1.
  cum_income <- cumsum(income * weight) / sum(income * weight)
  
  # Computing the area under the Lorenz curve:
  # We used the trapezoidal rule = said in the Wikipedia to be a good method for when
  # the entire Lorenz curve is not known
  B <- sum((cum_weight[-1] - cum_weight[-length(cum_weight)]) * 
             (cum_income[-1] + cum_income[-length(cum_income)])) / 2
  
  ## Computing the gini coefficient
  gini <- 1 - (B * 2)
  
  return(gini)
  
}

# End of script
