# debug__display_error_v01.R

################################################################################
# Load required packages.
################################################################################

library( tidyverse, quitely = TRUE)     # We live in the tidyverse.


################################################################################
# Load required functions including the one we are testing.
################################################################################

source('D:/R-Projects/loess/functions/display_error_v01.R')


################################################################################
# Note that the stip statement will cause this script to stop and display the
# message.
################################################################################

data( faithful )

str_variable = "xyz"

display_error( "xyz", faithful )
