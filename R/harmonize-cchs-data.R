#' harmonize_cchs_data
#' 
#' Harmonizes all the CCHS datasets in the cchs_data argument. They are 
#' harmonized to the variables in the passed variables sheet. Uses the rules
#' in the passed variabled details sheet.
#'
#' @param variables_sheet The variables sheet which has the variables to
#' harmonize the datasets to
#' @param variables_details_sheet The variables details sheet which holds the
#' rules to use when harmonizing variables 
#' @param cchs_data A named list where the index is the name of the dataset
#' and the value is the data frame containing the data.
#'
#' @return A dataframe containing the combined and harmonized cchs datasets
#' @export
#'
#' @examples
harmonize_cchs_data <- function(
  variables_sheet,
  variables_details_sheet,
  cchs_data
) {
  # The dataframe that will have all the cchs data variables recoded into the
  # ones for the study
  harmonized_data <- NULL
  # Go through each of the cchs datasets, recode it and add it to the
  # harmonized_data variable
  for (cchs_data_index in seq_along(names(cchs_data))) {
    # The name of the current cchs data
    current_data_name <- names(cchs_data)[[cchs_data_index]]
    # The current cchs dataset we will harmonize
    current_cchs_data <- cchs_data[[current_data_name]]
    
    # The harmonized version of the current cchs dataset
    current_harmonized_data <- recodeflow::rec_with_table(
      current_cchs_data,
      variables = variables_sheet,
      database_name = current_data_name,
      variable_details = variables_details_sheet,
      id_role_name = "id",
      custom_function_path = "R/custom-functions.R",
      notes = FALSE,
    )
    
    # If the harmonized_data has not been initialized then set to the
    # current one.
    # Otherwise row append the current one to the harmonize_data
    if (is.null(harmonized_data)) {
      harmonized_data <- current_harmonized_data
    } else {
      harmonized_data <-
        dplyr::bind_rows(harmonized_data, current_harmonized_data)
    }
  }
  
  harmonized_data <- recodeflow::set_data_labels(
    harmonized_data, variables_details_sheet, variables_sheet)
  
  return(harmonized_data)
}