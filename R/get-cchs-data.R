#' get_cchs_data
#'
#' Loads the data in the passed config object and returns it
#' 
#' @param cchs_config The config named list to use. Look at the config.yml
#' file in this project to see the structure
#'
#' @return A named list where the item names are the same as those in the data
#' argument of the config list. The value of each list item is the loaded data.
#' @export
#'
#' @examples
create_study_data <- function(variables_sheet, variables_details_sheet, cchs_config) {
  harmonized_data <- NULL
  
  # The environment in which we will load all the data to ensure it does not
  # pollute the global environment
  data_env <- new.env()
  
  # The vector of cchs database names
  data_names <- names(cchs_config$data)
  # This for loop will populate the cchs_data variable with all the cchs
  # databases we will be using
  for(data_index in seq_along(data_names)) {
    # Get the name of the current cchs dataset
    data_name <- data_names[[data_index]]
    print(paste("Start harmonization for", data_name))
    # Using the path, load the .RData file into the environment
    load(cchs_config$data[[data_name]], envir = data_env)
    
    current_harmonized_data <- cchsflow::rec_with_table(
      get(data_name, envir = data_env),
      variables = variables_sheet,
      database_name = data_name,
      variable_details = variables_details_sheet,
      # id_role_name = "id",
      custom_function_path = "R/custom-functions.R",
      notes = FALSE,
    )
    current_harmonized_data$SurveyCycle <- data_name
    
    # If the harmonized_data has not been initialized then set to the
    # current one.
    # Otherwise row append the current one to the harmonize_data
    if (is.null(harmonized_data)) {
      harmonized_data <- current_harmonized_data
    } else {
      harmonized_data <-
        dplyr::bind_rows(harmonized_data, current_harmonized_data)
    }
    
    rm(list = data_name, envir = data_env)
    
    print(paste("Done harmonization for", data_name))
  }
  
  return(harmonized_data)
}
