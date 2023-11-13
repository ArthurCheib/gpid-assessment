# Script Name: survey_long.R
# Author: Arthur Cheib
# Date: Nov 10, 2023
# Description: script that creates a function to transform surveys data into long format, using the year col
# to 'unpack' the data

# Dependencies: `readr`, `here`, `tidyr`
library(readr)
library(here)
library(tidyr)

# Example usage: 
# long_table <- survey_long(wide_data = my_dataframe)

# ----------------------------------------------------------

# Function: survey_long
# Description: This function transforms the each year's household data into a tibble longer format, using `year` as a column
#
# Parameters:
#   `wide_data` - data we want to transform into longer format
#
# Returns:
#   long format tibble that has year as a single column instead (tidy)

survey_long <- function(name, df) {
  
  ## Unpacking
  tibble(df, year = as.numeric(str_remove(name, 'Y')))
  
}

# End of script
