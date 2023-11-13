# Script Name: get_data.R
# Author: Arthur Cheib
# Date: Nov 10, 2023
# Description: script that creates a function to read data from @randreascataneda's github for the
#              GPID R Assessment

# Dependencies: `readr`, 'here', `stringr`
library(readr)
library(here)
library(stringr)

# Example usage: 
# my_table <- get_data(rds_file = 'wdi_in1.Rds', save_file = TRUE, directory_to_save = 'raw-data')

# ----------------------------------------------------------

# Function: get_data
# Description: This function gets data from @randreascataneda (github), pub_data folder and saves it to a 
#             local directory if requested by the user.
#
# Parameters:
#   `rds_file` - character containing the .Rds file to be read
#   `save_file`. - boolean indicating if user want to save the file locally or not
#   `directory_to_save` - character indicating local directory (making use of here package)
#
# Returns:
#   .Rds file in a tibble format and/or a .Rds file locally written.

get_data <- function(rds_file, save_file = FALSE, directory_to_save) {
  
  ## File location
  data_url <- paste0("https://github.com/randrescastaneda/pub_data/raw/",
                     ## Tag info
                     "202311081903",
                     ## Data dir
                     "/data/Rtest1/")

  
  ## 1st step: if user wants to overwrite the file, then download + save it + return table
  if (isTRUE(save_file)) {
    
    ## Getting the file
    data <- readr::read_rds(paste0(data_url, rds_file)) |> 
      as_tibble()
    
    ## Saving the file
    write_rds(data, here(directory_to_save, rds_file))
    
    ## Returning it as a table
    return(data)
    
    ## 2nd step: if user don't want to download it, then just read the table and return it
  } else {
    
    ## Getting the file
    data <- readr::read_rds(paste0(data_url, rds_file)) |> 
      as_tibble()
    
    return(data)
  
  }

}

# End of script
