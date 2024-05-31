harmonized_2001 <- cchsflow::rec_with_table(cchs2001, variables = variables, variable_details = variable_details, 
                         custom_function_path = "R/custom-functions.R",
                         notes = FALSE
)
harmonized_2001$year <- 2001

harmonized_2003 <- cchsflow::rec_with_table(cchs2003, variables = variables, variable_details = variable_details, 
                                            custom_function_path = "R/custom-functions.R",
                                            notes = FALSE
)
harmonized_2003$year <- 2003

harmonized_2005 <- cchsflow::rec_with_table(cchs2005, variables = variables, variable_details = variable_details, 
                                            custom_function_path = "R/custom-functions.R",
                                            notes = FALSE
)
harmonized_2005$year <- 2005
harmonized_2007_2008 <- cchsflow::rec_with_table(cchs2007_2008, variables = variables, variable_details = variable_details, 
                                            custom_function_path = "R/custom-functions.R",
                                            notes = FALSE
)
harmonized_2007_2008$year <- 2007
harmonized_2009_2010 <- cchsflow::rec_with_table(cchs2009_2010, variables = variables, variable_details = variable_details, 
                                                 custom_function_path = "R/custom-functions.R",
                                                 notes = FALSE
)
harmonized_2009_2010$year <- 2009
harmonized_2011_2012 <- cchsflow::rec_with_table(cchs2011_2012, variables = variables, variable_details = variable_details, 
                                                 custom_function_path = "R/custom-functions.R",
                                                 notes = FALSE
)
harmonized_2011_2012$year <- 2011
harmonized_2013_2014 <- cchsflow::rec_with_table(cchs2013_2014, variables = variables, variable_details = variable_details, 
                                                 custom_function_path = "R/custom-functions.R",
                                                 notes = FALSE
)
harmonized_2013_2014$year <- 2013

harmonized_2015_2016 <- cchsflow::rec_with_table(cchs2015_2016, variables = variables, variable_details = variable_details, 
                                                 custom_function_path = "R/custom-functions.R",
                                                 notes = FALSE
)
harmonized_2015_2016$year <- 2015
harmonized_2015_2016 <- cbind(harmonized_2015_2016, bsw1516[,-(1:2)])

harmonized_2017_2018 <- cchsflow::rec_with_table(cchs2017_2018, variables = variables, variable_details = variable_details, 
                                                 custom_function_path = "R/custom-functions.R",
                                                 notes = FALSE
)
harmonized_2017_2018$year <- 2017
harmonized_2017_2018 <- cbind(harmonized_2017_2018, bsw1718[,-(1:2)])

# Combined dataset without bootstrap
harmonized_combined <- merge_rec_data(harmonized_2001, harmonized_2003, harmonized_2005, harmonized_2007_2008, harmonized_2009_2010,harmonized_2011_2012, harmonized_2013_2014, harmonized_2015_2016, harmonized_2017_2018)
harmonized <- list(harmonized_2001, harmonized_2003, harmonized_2005, harmonized_2007_2008, harmonized_2009_2010,harmonized_2011_2012, harmonized_2013_2014, harmonized_2015_2016, harmonized_2017_2018)
harmonized_combined <- bind_rows(harmonized)

# Add labelled categories
harmonized_combined <- harmonized_combined %>%
  mutate(Province = case_when(GEOGPRV == 10 ~ "Newfoundland and Labrador",
                              GEOGPRV == 11 ~ "Prince Edward Island",
                              GEOGPRV == 12 ~ "Nova Scotia",
                              GEOGPRV == 13 ~ "New Brunswick",
                              GEOGPRV == 24 ~ "Quebec",
                              GEOGPRV == 35 ~ "Ontario",
                              GEOGPRV == 46 ~ "Manitoba",
                              GEOGPRV == 47 ~ "Saskatchewan",
                              GEOGPRV == 48 ~ "Alberta",
                              GEOGPRV == 59 ~ "British Columbia",
                              GEOGPRV == 60 ~ "Yukon/NWT/Nunavut")) %>%
  mutate(activity = case_when(energy_exp < 1.5 ~ "Inactive",
                              energy_exp >= 1.5 & energy_exp < 3 ~ "Moderately active",
                              energy_exp >= 3 ~ "Active")) %>%
  mutate(Age_Group = case_when(DHHGAGE_C %in% c(1:2) ~ "<18",
                               DHHGAGE_C %in% c(3:9) ~ "18-49",
                               DHHGAGE_C %in% c(10:12) ~ "50-64",
                               DHHGAGE_C %in% c(13:15) ~ "65-79",
                               DHHGAGE_C == 16 ~ "80+"))

# Remove NA immigration population
harmonized_combined <-harmonized_combined %>%
  filter(immigration_der %in% c(1:6)) %>%
  mutate(Immigration = case_when(immigration_der ==1 ~ "Non-racialized Canadian-born",
                          immigration_der ==2 ~ "Racialized Canadian-born",
                          immigration_der ==3 ~ 'Non-racialized recent immigrant',
                          immigration_der ==4 ~ 'Racialized recent immigrant',
                          immigration_der ==5 ~ 'Non-racialized established immigrant',
                          immigration_der ==6 ~ 'Racialized established immigrant'
  ))

# Add # of risk factors - go to custom-function.R to run alc_cat_fun and risk_factor_fun functions
attach(harmonized_combined)
harmonized_combined$Alcohol <-alc_cat_fun(DHH_SEX, ALC_1, ALWDWKY)
harmonized_combined$Risk_factor<-risk_factor_fun(CCC_071, CCC_101, HWTGBMI_der_cat4, activity, SMKDSTY_cat3, Alcohol, CCC_075)


#### Bootstrap for 2015-2018 ###
# add bootstrap (bsw1516 - bootstrap file for 2015/16; bsw1718 - bootstrap file for 2017/18 )
harmonized_2015_2016_bsw <- cbind(harmonized_2015_2016, bsw1516[,-(1:2)])
harmonized_2017_2018_bsw <- cbind(harmonized_2017_2018, bsw1718[,-(1:2)])

# combine data set
harmonized_bsw <- bind_rows(harmonized_2015_2016_bsw, harmonized_2017_2018_bsw)

# Add labelled categories
harmonized_bsw <- harmonized_bsw %>%
  mutate(Province = case_when(GEOGPRV == 10 ~ "Newfoundland and Labrador",
                              GEOGPRV == 11 ~ "Prince Edward Island",
                              GEOGPRV == 12 ~ "Nova Scotia",
                              GEOGPRV == 13 ~ "New Brunswick",
                              GEOGPRV == 24 ~ "Quebec",
                              GEOGPRV == 35 ~ "Ontario",
                              GEOGPRV == 46 ~ "Manitoba",
                              GEOGPRV == 47 ~ "Saskatchewan",
                              GEOGPRV == 48 ~ "Alberta",
                              GEOGPRV == 59 ~ "British Columbia",
                              GEOGPRV == 60 ~ "Yukon/NWT/Nunavut")) %>%
  mutate(activity = case_when(energy_exp < 1.5 ~ "Inactive",
                              energy_exp >= 1.5 & energy_exp < 3 ~ "Moderately active",
                              energy_exp >= 3 ~ "Active")) %>%
  mutate(Age_Group = case_when(DHHGAGE_C %in% c(1:2) ~ "<18",
                               DHHGAGE_C %in% c(3:9) ~ "18-49",
                               DHHGAGE_C %in% c(10:12) ~ "50-64",
                               DHHGAGE_C %in% c(13:15) ~ "65-79",
                               DHHGAGE_C == 16 ~ "80+"))

# Remove NA immigration population
harmonized_bsw <-harmonized_bsw %>%
  filter(immigration_der %in% c(1:6)) %>%
  mutate(Immigration = case_when(immigration_der ==1 ~ "Non-racialized and non-Indigenous Canada-born",
                                 immigration_der ==2 ~ "Racialized Canada-born",
                                 immigration_der ==3 ~ 'Non-racialized and non-Indigenous recent immigrant',
                                 immigration_der ==4 ~ 'Racialized recent immigrant',
                                 immigration_der ==5 ~ 'Non-racialized and non-Indigenous established immigrant',
                                 immigration_der ==6 ~ 'Racialized established immigrant'
  ))

# Add # of risk factors  - go to custom-function.R to run alc_cat_fun and risk_factor_fun functions
attach(harmonized_bsw)
harmonized_bsw$Alcohol <-alc_cat_fun(DHH_SEX, ALC_1, ALWDWKY)
harmonized_bsw$Risk_factor<-risk_factor_fun(CCC_071, CCC_101, HWTGBMI_der_cat4, activity, SMKDSTY_cat3, Alcohol, CCC_075)
