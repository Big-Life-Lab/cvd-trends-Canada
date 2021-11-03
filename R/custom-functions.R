# These are all the functions that the derived variables needs in the
# variables details files

if_else2 <- cchsflow::if_else2
low_drink_short_fun <- cchsflow::low_drink_short_fun
low_drink_long_fun <- cchsflow::low_drink_long_fun
age_cat_fun <- cchsflow::age_cat_fun
energy_exp_fun <- cchsflow::energy_exp_fun
bmi_fun <- cchsflow::bmi_fun
bmi_fun_cat <- cchsflow::bmi_fun_cat
immigration_fun <- cchsflow::immigration_fun
low_drink_score_fun <- cchsflow::low_drink_score_fun
low_drink_score_fun1 <- cchsflow::low_drink_score_fun1
pack_years_fun <- cchsflow::pack_years_fun
pack_years_fun_cat <- cchsflow::pack_years_fun_cat
pct_time_fun <- cchsflow::pct_time_fun
pct_time_fun_cat<- cchsflow::pct_time_fun_cat
SMKG040_fun <- cchsflow::SMKG040_fun
time_quit_smoking_fun <- cchsflow::time_quit_smoking_fun
smoke_simple_fun <- cchsflow::smoke_simple_fun

SurveyCycle.fun <- function(data_name) {
  switch(
    data_name,
    cchs2001 = {
      return("2001")
    },
    cchs2003 = {
      return("2003")
    },
    cchs2005 = {
      return("2005")
    },
    cchs2007_2008 = {
      return("2007_2008")
    },
    cchs2009_2010 = {
      return("2009_2010")
    },
    cchs2011_2012 = {
      return("2011_2012")
    },
    cchs2013_2014 = {
      return("2013_2014")
    },
    cchs2015_2016 = {
      return("2015_2016")
    },
    cchs2017_2018 = {
      return("2017_2018")
    }
  )
  
  stop(paste(
    "Unknown data_name argument when creating SurveyCycle variable",
    data_name
  ))
}

# Categorical alcohol using seven more year study
alc_cat_fun<- 
  function(DHH_SEX, ALC_1, ALWDWKY){
    # Current non-drinker
    if_else2(ALC_1 ==2, 1,
    # Light
    if_else2(ALC_1 ==1 & DHH_SEX ==1 & ALWDWKY %in% c(0:4), 2,
    if_else2(ALC_1 ==1 & DHH_SEX ==2 & ALWDWKY %in% c(0:2), 2,
    # Moderate
    if_else2(ALC_1 ==1 & DHH_SEX ==1 & ALWDWKY %in% c(5:9), 3,
    if_else2(ALC_1 ==1 & DHH_SEX ==2 & ALWDWKY %in% c(3:5), 3,
    # Heavy
    if_else2(ALC_1 ==1 & DHH_SEX ==1 & ALWDWKY %in% c(10:24), 4,
    if_else2(ALC_1 ==1 & DHH_SEX ==2 & ALWDWKY %in% c(6:17), 4,
    # Binge 
    if_else2(ALC_1 ==1 & DHH_SEX ==1 & ALWDWKY > 24, 5,
    if_else2(ALC_1 ==1 & DHH_SEX ==2 & ALWDWKY > 17, 5,
    if_else2(ALWDWKY == "NA(b)" | ALC_1 == "NA(b)"|DHH_SEX== "NA(b)", tagged_na("b"), tagged_na("a"))
    )))))))))
  }