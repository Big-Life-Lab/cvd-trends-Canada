library(targets)
library(magrittr)
library(bllflow)

source("R/get-cchs-data.R")
source("R/harmonize-cchs-data.R")

# Targets plan to read in all the worksheets and configuration files that
# this project needs
read_worksheets <- list(
  # Keep track of changes to the config.yml file
  targets::tar_target(
    config_file,
    "./config.yml",
    format = "file"
  ),
  # Read in the config file
  targets::tar_target(
    cchs_config,
    config::get(file = config_file)
  ),
  # Keep track of changes to the variables sheet
  targets::tar_target(
    cchs_variables_sheet_file,
    cchs_config$variables,
    format = "file"
  ),
  # Create the variables sheet for the project
  targets::tar_target(
    cchs_variables_sheet,
    read.csv(cchsflow_variables_sheet_file)
  ),
  # Keep track of changes to the cchsflow variable details file
  targets::tar_target(
    cchs_variables_details_sheet_file,
    cchs_config$cchsflow_variable_details,
    format = "file"
  ),
  # Create the variable details sheet for the project
  targets::tar_target(
    cchs_variables_details_sheet,
    rbind(
      read.csv(cchs_variables_details_sheet_file)
    )
  )
)

harmonize_data_plan <- list(
  targets::tar_target(
    cchs_data,
    get_cchs_data(cchs_config)
  ),
  targets::tar_target(
    harmonized_cchs_data,
    harmonize_cchs_data(
      cchs_variables_sheet,
      cchs_variables_details_sheet,
      cchs_data
    )
  ),
  targets::tar_target(
    harmonized_cchs_data,
    working_data
  )
)

table_1_a_plan <- list(
  tar_target(
    cchs_flow,
    # Create the bllflow object 
    bllflow::build_bllflow(
      harmonized_cchs_data, 
      cchs_variables_sheet, 
      cchs_variables_details_sheet
    )
  ),
  tar_target(
    # Get the variables we will use as the stratifier for table 1 a
    table_1_a_stratifier,
    recodeflow:::select_vars_by_role(
      "table-1-a-stratifier", cchs_flow$variables),
  ),
  tar_target(
    table_1_a,
    # Create table 1 a
    bllflow::CreateTableOne(
      cchs_flow, select_role = "table-1-a", strata = table_1_a_stratifier
    )
  )
)

