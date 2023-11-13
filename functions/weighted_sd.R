# Script Name: weighted_sd.R
# Author: Arthur Cheib
# Date: Nov 10, 2023
# Description: script that creates a function to compute a weighted standard deviation

# Dependencies: none

# Example usage: 

# ----------------------------------------------------------

# Function: weighted_sd
# Description: This function computes a weighted standard deviation given two numeric vectors
#
# Parameters:
#   `vec1` - numeric vector with the numbers to have their standard deviation calculated
#   `vec2` - numeric vector with the numbers to be used as a weight to compute the sd
#
# Returns:
#   A weighted standard deviation given the values of the vector to be used for calculation
#   and the values of the vector to be used as weight for the calculation.

weighted_sd <- function(obs, weight, na.rm = FALSE) {
  
  ## Because the length of observations and weights vectors have to be same, we need a check
  if (na.rm) {
    
    all_good <- !is.na(obs) & !is.na(weight)
    obs <- obs[all_good]
    weight <- weight[all_good]
    
  }
  
  if (length(obs) != length(weight)) {
    
    stop("The lengths of 'obs' and 'weight' must be the same.")
    
  }
  
  ## Computing the weighted mean for all observations given a chosen weight
  w_mean <- weighted.mean(obs, weight, na.rm = TRUE)
  
  ## Numerator = the sum of the weight times (the observation minus the weighted mean) squared
  numerator <- sum(weight * (obs - w_mean)^2, na.rm = TRUE)
  
  ## Denominator
  denominator <- sum(weight, na.rm=TRUE)
  
  ## Taking the square root
  result <- sqrt(numerator / denominator)
  
  return(result)
  
} 


# End of script
