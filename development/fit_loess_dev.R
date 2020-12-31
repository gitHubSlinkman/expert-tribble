# in_development.r

################################################################################
# Develop a script that will optimize the smooth of a simple linear 
# regression model using the lowless smoothing that will have a better user
# interface than the actual loess function.  Essentially this is a user 
# friendly wrapper to loess for simple linear regression.
################################################################################


################################################################################
# Load required packages.
################################################################################

library(tidyverse)                      # I live in the tidyverse environment.


################################################################################
# Functions needed.
################################################################################

source('D:/R-Projects/RStatistics/find_column_position_v01.R')


################################################################################
# Helper functions
################################################################################


################################################################################
# Convert variable name to a string
################################################################################

convert_variable_toString <- 
  function(variable) {deparse(substitute(variable))}


################################################################################
# Function display variable nor found in data source.
################################################################################

display_not_found <- 
  function( str_variable, data){
    data <- convert_variable_toString( data )
    message <- paste( str_variable,
                      "not found is data object:",
                      data )
    stop( message )
  }


################################################################################
# Get the data we are going to smooth.
################################################################################

data( faithful )                        # The old faithful data set


################################################################################
# Change the data set into a tibble because they are safer and easier to work
# with.  We then will rename the variables to make them more descriptive than
# the base R version.  We make them more consumer friendly.
#
# Remarks:
# length is the length of the prior eruption of the Old Faithful geyser
# measured in minutes.  This is our predictor variable.
#
# interval is the interval of time between the end of the prior eruption and
# the start of the following eruption.  It is also measure in minutes.
#
# The regression model R-notation is interval ~ length
################################################################################

faithful <-                               
  as_tibble( faithful ) %>%             # Cast dataframe to tibble and ...
    rename( "length" = "eruptions",     # change variable names.
            "interval" = "waiting" )

faithful                                # Display first 10 rows of tibble.


################################################################################
# Function definition
################################################################################

fit_loess <- 
  function( data,             # Data source (dataframe of tibble).
            str_id,           # String model identifer
            str_predictor,    # Quoted string name of predictor variable.
            str_response,     # Quoted string name of response variable.
            span = 2/3 )      # Fraction of data to be for local fitting.
  {
     ###########################################################################
     # We ascertain the column position od the predictor and the response
     # variable so we can extract them from the dataframe and use them for
     # the loess fit.  If they are not found then the function is halted.
     ###########################################################################
      
     predictor_position <- 
       find_column_position( data,
                             str_predictor )
     response_position  <- 
       find_column_position( data,
                            str_response )
    
     
     ###########################################################################
     # We extract variable values from the data frames into two variable called
     # predictor and response.  We then begin to construct the augmented 
     # tibble that eventually will contain the orginal predictor, response,
     #  the fitted values, and the residuals.
     ###########################################################################
    
     predictor <- 
       data %>% 
        pull( predictor_position )
    
     response <- 
       data %>% 
        pull( response_position )

     augmented_data <-       # Begin constructing augment_data  tibble.          
       tibble( predictor,    
               response )

     ###########################################################################
     # We fit the loess function saving and save the fit.  We are using
     # Tukey's biweight (family = "symmetric") to make the solution robust.
     ###########################################################################

     fit <- loess( formula = response ~ predictor,
                   augmented_data,
                   family = "symmetric",
                   span = span  )

    ############################################################################
    # Get the residuals and the fitted values
    ############################################################################
    
    fitted <- fitted( fit )  
    residual <- residuals( fit )
    
    
    ############################################################################
    # add the fitted values to the augmented data tibble.
    ############################################################################
    
    augmented_data <- 
      augmented_data %>% 
        add_column( fitted ) %>% 
        add_column( residual )
    
  
    
    ############################################################################
    # Get the performance measures.
    ############################################################################
    
    n    <- dim( augmented_data)[1]      # Sample size.
    enp  <- fit$fnp                      # Effective number of parameters.
    bias <- mean( residual )             # Compute model bias 
    s <- fit$s                           # Get standard deviation fo residuals.
    dev <- ( response - mean(response))  # Compute deviation frm mean response.
    ssy <- sum( dev^2 )                  # compute sum of squared deviations.
    sse <- sum( residual^2)              # Compute sum of squared errors.
    R2 <- 1 - sse /ssy                 # compute effective R-squared.
    
    ############################################################################
    # We make a list of the above statistics
    ############################################################################
    
    
      list( n, enp, bias, s, R2 )
    
   
    
    ############################################################################
    # We have a lot of different types of data that the user may wish to
    # know and therefore we need to pass the information back.  Therefore we
    # pass back a list.
    ############################################################################
    
    list_out <- list( str_id, 
                      augmented_data, 
                      n, 
                      enp, 
                      bias,
                      s,
                      R2 )
    
    list_out
  }

################################################################################
# These are statistics accessor functions.
################################################################################

get_id             <- function( fit ){ fit[[1]] }
get_augmented_data <- function( fit ){ fit[[2]] }
get_n              <- function( fit ){ fit[[3]] }
get_enp            <- function( fit ){ fit[[4]] }
get_bias           <- function( fit ){ fit[[5]] }
get_s              <- function( fit ){ fit[[6]] }
get_R2             <- function( fit ){ fit[[7]] }


################################################################################
# We should move this to the test file.
################################################################################

test_fit <- 
  fit_loess( data = faithful,
         str_id = "Test",
         str_predictor = "length",
         str_response = "interval" )


################################################################################
# Tests to be moved to test script
################################################################################

get_id( test_fit )
get_augmented_data ( test_fit )
get_augmented_data 