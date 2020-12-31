# Function: convert_variable_to_string_vo1.R

################################################################################
# Load required packages.
################################################################################

library( tidyverse )                    # I live on the tidyverse environment.           

################################################################################
# Get data to be used for testing.
################################################################################

convert_variable_to_string <- 
  function( variable  ){
      deparse(substitute( variable ))
  }
