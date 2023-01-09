config_file <- config::get(file = "config.yml")

source("R/get-cchs-data.R")
source("R/harmonize-cchs-data.R")

cchs_data <- create_study_data(variables, variable_details, config_file)
harmonized_data <- harmonize_cchs_data(variables, variable_details, cchs_data)