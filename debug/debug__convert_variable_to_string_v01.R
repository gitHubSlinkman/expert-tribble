# debugging__convert_variable_to_string.R

################################################################################
# Load required packages.
################################################################################

library( tidyverse )                    # I live on the tidyverse environment.  


################################################################################
# Load required functions
################################################################################

source('D:/R-Projects/loess/functions/convert_variable_to_string_v01.R')
       

################################################################################
# Test function: convert_variable_to_string
################################################################################

data( economics )
economics
convert_variable_to_string( economics )

apple  <- 1:10
convert_variable_to_string( apple )

banana <- "yellow"
convert_variable_to_string( banana )

chuck  <- rnorm( 100, 75, 5)
convert_variable_to_string( chuck )

convert_variable_to_string( convert_variable_to_string )
