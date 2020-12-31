# Function: display_error_v01.R

################################################################################
# Load required paxkages.
################################################################################

library( tidyverse,                      # I live in the tidyverse.   
         quietly =TRUE )                   


################################################################################
# Load required function definitions.
################################################################################

source('D:/R-Projects/loess/functions/convert_variable_to_string_v01.R')  

################################################################################
# Begin function definition
################################################################################

display_error <- 
  function( str_variable,
            data_source ){
 
    ############################################################################
    # We create the character string name of the data_source object.
    ############################################################################
    
    str_data_source <- 
      convert_variable_to_string( data_source )
    
    ############################################################################
    # Construct our error message
    ############################################################################
    
    message <- paste( str_variable,
                      "not found in ",
                      str_data_source )
    
    ############################################################################
    # Stop and display error message.
    ############################################################################
    
    stop( message )
  }