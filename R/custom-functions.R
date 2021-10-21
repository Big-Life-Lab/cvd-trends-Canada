# These are all the functions that the derived variables needs in the
# variables details files

binge_drinker_fun <- cchsflow::binge_drinker_fun
diet_score_fun <- cchsflow::diet_score_fun
bmi_fun <- cchsflow::bmi_fun
pack_years_fun <- cchsflow::pack_years_fun
pct_time_fun <- cchsflow::pct_time_fun
if_else2 <- cchsflow::if_else2
time_quit_smoking_fun <- cchsflow::time_quit_smoking_fun
smoke_simple_fun <- cchsflow::smoke_simple_fun

SurveyCycle.fun <- function(data_name) {
  switch(
    data_name,
    cchs2001 = {
      return("2001")
    },
    cchs2003_p = {
      return("2003")
    },
    cchs2005_p = {
      return("2005")
    },
    cchs2007_2008p = {
      return("2007_2008")
    },
    cchs2009_2010p = {
      return("2009_2010")
    },
    cchs2011_2012_p = {
      return("2011_2012")
    },
    cchs2013_2014p = {
      return("2013_2014")
    },
    cchs2015_2016_p = {
      return("2015_2016")
    },
    cchs2017_2018_p = {
      return("2017_2018")
    }
  )
  
  stop(paste(
    "Unknown data_name argument when creating SurveyCycle variable",
    data_name
  ))
}