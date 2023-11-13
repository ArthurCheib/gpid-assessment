# Script Name: df_lorenz.R
# Author: Arthur Cheib
# Date: Nov 10, 2023
# Description: script that creates a function to computes values for the Lorenz Curve

# Dependencies: `dplyr`, 'here'
library(dplyr)
library(here)

# Example usage: 
# lorenz_table <- df_lorenz(x = my_table, level = 1000)

# ----------------------------------------------------------

# Function: df_lorenz
# Description: This function computes the varialbes of the Lorenz curve at the percentile level
#
# Parameters:
#   `x` - the table used as input to compute the Lorenz's variables
#   `level` - number (double) indicating the percentile level of the data
#
# Returns:
#   A dataframe containing the Lorenz's variables

df_lorenz <- function(x, level) {
  
  ## Transformations
  lorenz_curve <- x |> 
    arrange(year, income) |> 
    group_by(year) |> 
    # Get the bin for each group of 1% of the units
    mutate(bin = ((row_number() - 1) %/% level) + 1) |>
    group_by(year, bin) |> 
    # Compute the welfare and pop_sum for each pair year-bin
    summarize(welfare = sum(income * weight),
              population = as.double(n()), .groups = 'drop') |>
    group_by(year) |> 
    # Compute the cumulative sum for the variables
    mutate(cum_welfare = cumsum(welfare) / sum(welfare),
           cum_population = cumsum(population) / sum(population)) |> 
    select(welfare, cum_welfare, cum_population, year, bin) |>
    ungroup()
    
  
  return(lorenz_curve)
  
}

# End of script
