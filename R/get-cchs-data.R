#' get_cchs_data
#'
#' Loads the data in the passed config object and returns it
#' 
#' @param cchsflow_config The config named list to use. Look at the config.yml
#' file in this project to see the structure
#'
#' @return A named list where the item names are the same as those in the data
#' argument of the config list. The value of each list item is the loaded data.
#' @export
#'
get_cchs_data <- function(cchsflow_config) {
  # The named list of CCHS datasets for this project. The index will be the name
  # of the database whereas the value will be the dataframe with the data
  cchs_data <- list()
  
  # The environment in which we will load all the data to ensure it does not
  # pollute the global environment
  data_env <- new.env()
  
  # The vector of cchs database names
  data_names <- names(cchsflow_config$data)
  # This for loop will populate the cchs_data variable with all the cchs
  # databases we will be using
  for(data_index in seq_along(data_names)) {
    # Get the name of the current cchs dataset
    data_name <- data_names[[data_index]]
    # Using the path, load the .RData file into the environment
    load(cchsflow_config$data[[data_name]], envir =  data_env)
    # Add the loaded dataset to the list, whose index is its name
    cchs_data[[data_name]] <- get(data_name, envir = data_env)
  }
  
  return(cchs_data)
}
