###
library(cchsflow)
library(dplyr)
library(tidyr)

#### CCHS data ####
cchs2001 <- read.csv("~/Undergrad/Honours/cchs-82M0013-E-2001-c1-1-general-file_F1.csv")
cchs2003 <- read.csv("~/Undergrad/Honours/cchs-82M0013-E-2003-c2-1-GeneralFile_F1.csv")
cchs2005 <- read.csv("~/Undergrad/Honours/cchs-82M0013-E-2005-c3-1-main-file_F1.csv")
cchs2007_2008 <- read.csv("~/Undergrad/Honours/cchs-82M0013-E-2007-2008-AnnualComponent_F1.csv")
cchs2009_2010 <- read.csv("~/Undergrad/Honours/cchs-82M0013-E-2009-2010-AnnualComponent_F1.csv")
cchs2011_2012 <- read.csv("~/Undergrad/Honours/cchs-82M0013-E-2011-2012-Annual-Component_F1.csv")
cchs2013_2014 <- read.csv("~/Undergrad/Honours/cchs-82M0013-E-2013-2014-Annual-Component_F1.csv")
cchs2015_2016 <- read.csv("~/Undergrad/Honours/cchs-82M0013-E-2015-2016-Annual-Component_F1.csv")
cchs2017_2018 <- read.csv("~/Undergrad/Honours/cchs-82M0013-E-2017-2018-Annual-Component_F1.csv")

#### Risk behaviours ####
### hypertension, diabetes, smoking, activity levels, and obesity #

## Smoker ##
Smoker2001 <- rec_with_table(cchs2001, c("SMKG207_cont","SMK_204","SMK_01A","SMK_05B","SMK_05C","SMK_09A_cont","SMK_208","SMKDSTY","SMKG01C_cont","SMKG09C","SMKG203_cont","pack_years_der"), log = TRUE  )
Smoker2001$year <- 2001
Smoker2003 <- rec_with_table(cchs2003, c("SMKG207_cont","SMK_204","SMK_01A","SMK_05B","SMK_05C","SMK_09A_cont","SMK_208","SMKDSTY","SMKG01C_cont","SMKG09C","SMKG203_cont","pack_years_der"), log = TRUE  )
Smoker2003$year <- 2003
Smoker2005 <- rec_with_table(cchs2005, c("SMKG207_cont","SMK_204","SMK_01A","SMK_05B","SMK_05C","SMK_09A_cont","SMK_208","SMKDSTY","SMKG01C_cont","SMKG09C","SMKG203_cont","pack_years_der"), log = TRUE  )
Smoker2005$year <- 2005
Smoker2007_2008 <- rec_with_table(cchs2007_2008, c("SMKG207_cont","SMK_204","SMK_01A","SMK_05B","SMK_05C","SMK_09A_cont","SMK_208","SMKDSTY","SMKG01C_cont","SMKG09C","SMKG203_cont","pack_years_der"), log = TRUE  )
Smoker2007_2008$year <- 2007
Smoker2009_2010 <- rec_with_table(cchs2009_2010, c("SMKG207_cont","SMK_204","SMK_01A","SMK_05B","SMK_05C","SMK_09A_cont","SMK_208","SMKDSTY","SMKG01C_cont","SMKG09C","SMKG203_cont","pack_years_der"), log = TRUE  )
Smoker2009_2010$year <- 2009
Smoker2011_2012 <- rec_with_table(cchs2011_2012, c("SMKG207_cont","SMK_204","SMK_01A","SMK_05B","SMK_05C","SMK_09A_cont","SMK_208","SMKDSTY","SMKG01C_cont","SMKG09C","SMKG203_cont","pack_years_der"), log = TRUE  )
Smoker2011_2012$year <- 2011
Smoker2013_2014 <- rec_with_table(cchs2013_2014, c("SMKG207_cont","SMK_204","SMK_01A","SMK_05B","SMK_05C","SMK_09A_cont","SMK_208","SMKDSTY","SMKG01C_cont","SMKG09C","SMKG203_cont","pack_years_der"), log = TRUE  )
Smoker2013_2014$year <- 2013

Smoker <- list(Smoker2001, Smoker2003, Smoker2005, Smoker2007_2008, Smoker2009_2010,Smoker2011_2012, Smoker2013_2014)
Smoker_combined <- bind_rows(Smoker)

get_label(Smoker_combined)
labeled_Smoker_combined <- set_data_labels(Smoker_combined,variable_details,variables)
get_label(labeled_Smoker_combined)


## Activity PACDEE ##
METS2001 <- rec_with_table(cchs2001, "PACDEE", log = TRUE )
METS2001$year <- 2001
METS2003 <- rec_with_table(cchs2003, "PACDEE", log = TRUE  )
METS2003$year <- 2003
METS2005 <- rec_with_table(cchs2005, "PACDEE", log = TRUE  )
METS2005$year <- 2005
METS2007_2008 <- rec_with_table(cchs2007_2008, "PACDEE", log = TRUE  )
METS2007_2008$year <- 2007
METS2009_2010 <- rec_with_table(cchs2009_2010, "PACDEE", log = TRUE  )
METS2009_2010$year <- 2009
METS2011_2012 <- rec_with_table(cchs2011_2012, "PACDEE", log = TRUE  )
METS2011_2012$year <- 2011
METS2013_2014 <- rec_with_table(cchs2013_2014, "PACDEE", log = TRUE  )
METS2013_2014$year <- 2013

METS <- list(METS2001, METS2003, METS2005, METS2007_2008, METS2009_2010,METS2011_2012, METS2013_2014)
METS_combined <- bind_rows(METS)

get_label(METS_combined)
labeled_METS_combined <- set_data_labels(METS_combined,variable_details,variables)
get_label(labeled_METS_combined)

## Obesity HWTGBMI ##
BMI2001 <- rec_with_table(cchs2001, c("HWTGHTM", "HWTGWTK", "HWTGBMI_der","HWTGBMI"), log = TRUE )
BMI2001$year <- 2001
BMI2003 <- rec_with_table(cchs2003, c("HWTGHTM", "HWTGWTK", "HWTGBMI_der","HWTGBMI"), log = TRUE  )
BMI2003$year <- 2003
BMI2005 <- rec_with_table(cchs2005, c("HWTGHTM", "HWTGWTK", "HWTGBMI_der","HWTGBMI"), log = TRUE  )
BMI2005$year <- 2005
BMI2007_2008 <- rec_with_table(cchs2007_2008, c("HWTGHTM", "HWTGWTK", "HWTGBMI_der","HWTGBMI"), log = TRUE  )
BMI2007_2008$year <- 2007
BMI2009_2010 <- rec_with_table(cchs2009_2010, c("HWTGHTM", "HWTGWTK", "HWTGBMI_der","HWTGBMI"), log = TRUE  )
BMI2009_2010$year <- 2009
BMI2011_2012 <- rec_with_table(cchs2011_2012, c("HWTGHTM", "HWTGWTK", "HWTGBMI_der","HWTGBMI"), log = TRUE  )
BMI2011_2012$year <- 2011
BMI2013_2014 <- rec_with_table(cchs2013_2014, c("HWTGHTM", "HWTGWTK", "HWTGBMI_der","HWTGBMI"), log = TRUE  )
BMI2013_2014$year <- 2013


BMI <- list(BMI2001, BMI2003, BMI2005, BMI2007_2008, BMI2009_2010,BMI2011_2012, BMI2013_2014)
BMI_combined <- bind_rows(BMI)

get_label(BMI_combined)
labeled_BMI_combined <- set_data_labels(BMI_combined,variable_details,variables)
get_label(labeled_BMI_combined)

## Diabetes CCC_101 ##

Diabetes2001 <- rec_with_table(cchs2001, "CCC_101", log = TRUE )
Diabetes2001$year <- 2001
Diabetes2003 <- rec_with_table(cchs2003, "CCC_101", log = TRUE  )
Diabetes2003$year <- 2003
Diabetes2005 <- rec_with_table(cchs2005, "CCC_101", log = TRUE  )
Diabetes2005$year <- 2005
Diabetes2007_2008 <- rec_with_table(cchs2007_2008, "CCC_101", log = TRUE  )
Diabetes2007_2008$year <- 2007
Diabetes2009_2010 <- rec_with_table(cchs2009_2010, "CCC_101", log = TRUE  )
Diabetes2009_2010$year <- 2009
Diabetes2011_2012 <- rec_with_table(cchs2011_2012, "CCC_101", log = TRUE  )
Diabetes2011_2012$year <- 2011
Diabetes2013_2014 <- rec_with_table(cchs2013_2014, "CCC_101", log = TRUE  )
Diabetes2013_2014$year <- 2013


Diabetes <- list(Diabetes2001, Diabetes2003, Diabetes2005, Diabetes2007_2008, Diabetes2009_2010,Diabetes2011_2012, Diabetes2013_2014)
Diabetes_combined <- bind_rows(Diabetes)

get_label(Diabetes_combined)
labeled_Diabetes_combined <- set_data_labels(Diabetes_combined,variable_details,variables)
get_label(labeled_Diabetes_combined)

## Hypertension ##

HT2001 <- rec_with_table(cchs2001, "CCC_071", log = TRUE )
HT2001$year <- 2001
HT2003 <- rec_with_table(cchs2003, "CCC_071", log = TRUE  )
HT2003$year <- 2003
HT2005 <- rec_with_table(cchs2005, "CCC_071", log = TRUE  )
HT2005$year <- 2005
HT2007_2008 <- rec_with_table(cchs2007_2008, "CCC_071", log = TRUE  )
HT2007_2008$year <- 2007
HT2009_2010 <- rec_with_table(cchs2009_2010, "CCC_071", log = TRUE  )
HT2009_2010$year <- 2009
HT2011_2012 <- rec_with_table(cchs2011_2012, "CCC_071", log = TRUE  )
HT2011_2012$year <- 2011
HT2013_2014 <- rec_with_table(cchs2013_2014, "CCC_071", log = TRUE  )
HT2013_2014$year <- 2013


HT <- list(HT2001, HT2003, HT2005, HT2007_2008, HT2009_2010,HT2011_2012, HT2013_2014)
HT_combined <- bind_rows(HT)

get_label(HT_combined)
labeled_HT_combined <- set_data_labels(HT_combined,variable_details,variables)
get_label(labeled_HT_combined)

## Stratify by (age, sex, province, health region, household size(2003 and up)) ##
REC2001 <- rec_with_table(cchs2001, c("WTS_M","DHH_SEX","DHHGAGE_cont","DHHGAGE_C","GEOGPRV","INCGHH_cont"), log = TRUE )
REC2001$year <- 2001
REC2003 <- rec_with_table(cchs2003, c("WTS_M","DHH_SEX","DHHGAGE_cont","DHHGAGE_C","GEOGPRV","DHHGHSZ"), log = TRUE  )
REC2003$year <- 2003
Income2003 <- rec_with_table(cchs2003,"INCGHH_cont",log = TRUE)
REC2003 <- cbind(REC2003, Income2003)
REC2005 <- rec_with_table(cchs2005, c("WTS_M","DHH_SEX","DHHGAGE_cont","DHHGAGE_C","GEOGPRV","DHHGHSZ"), log = TRUE  )
REC2005$year <- 2005
Income2005 <- rec_with_table(cchs2005,"INCGHH_cont",log = TRUE)
REC2005 <- cbind(REC2005, Income2005)
REC2007_2008 <- rec_with_table(cchs2007_2008, c("WTS_M","DHH_SEX","DHHGAGE_cont","DHHGAGE_C","GEOGPRV","DHHGHSZ"), log = TRUE  )
REC2007_2008$year <- 2007
Income2007_2008 <- rec_with_table(cchs2007_2008,"INCGHH_cont",log = TRUE)
REC2007_2008 <- cbind(REC2007_2008, Income2007_2008)
REC2009_2010 <- rec_with_table(cchs2009_2010, c("WTS_M","DHH_SEX","DHHGAGE_cont","DHHGAGE_C","GEOGPRV","DHHGHSZ"), log = TRUE  )
REC2009_2010$year <- 2009
Income2009_2010 <- rec_with_table(cchs2009_2010,"INCGHH_cont",log = TRUE)
REC2009_2010 <- cbind(REC2009_2010, Income2009_2010)
REC2011_2012 <- rec_with_table(cchs2011_2012, c("WTS_M","DHH_SEX","DHHGAGE_cont","DHHGAGE_C","GEOGPRV","DHHGHSZ"), log = TRUE  )
REC2011_2012$year <- 2011
Income2011_2012 <- rec_with_table(cchs2011_2012,"INCGHH_cont",log = TRUE)
REC2011_2012 <- cbind(REC2011_2012, Income2011_2012)
REC2013_2014 <- rec_with_table(cchs2013_2014, c("WTS_M","DHH_SEX","DHHGAGE_cont","DHHGAGE_C","GEOGPRV","DHHGHSZ"), log = TRUE  )
REC2013_2014$year <- 2013
Income2013_2014 <- rec_with_table(cchs2013_2014,"INCGHH_cont",log = TRUE)
REC2013_2014 <- cbind(REC2013_2014, Income2013_2014)

REC <- list(REC2001, REC2003, REC2005, REC2007_2008, REC2009_2010,REC2011_2012, REC2013_2014)
REC_combined <- bind_rows(REC)

get_label(REC_combined)
labeled_REC_combined <- set_data_labels(REC_combined,variable_details,variables)
get_label(labeled_REC_combined)

# combined 

combined <-cbind(labeled_REC_combined, labeled_BMI_combined, labeled_Diabetes_combined,
                 labeled_HT_combined, labeled_METS_combined, labeled_Smoker_combined)
labeled_REC_combined[,c('data_name','year')] <- NULL
labeled_BMI_combined[,c('date_year','year')] <- NULL
labeled_Diabetes_combined[,c('date_year','year')] <- NULL
labeled_HT_combined[,c('date_year','year')] <- NULL
labeled_METS_combined[,c('date_year','year')] <- NULL

combined <-cbind(labeled_REC_combined, labeled_BMI_combined, labeled_Diabetes_combined,
                 labeled_HT_combined, labeled_METS_combined, labeled_Smoker_combined)

## Additional columns ##
combined <- combined %>%
  # Household = 1 for 1-2 persons, Household = 2 for 3-4 persons, Household = 5+ persons
  mutate(Household = case_when(DHHGHSZ == 1 |DHHGHSZ == 2 ~ 1,
                               DHHGHSZ == 3 |DHHGHSZ == 4 ~ 2,
                               DHHGHSZ == 5 ~ 3)) %>%
  #Income adequacy according to StatCan's 4-group categorization of income adequacy
  mutate(Income_Adequency = case_when((INCGHH_cont < 15000 & Household == 1)|(INCGHH_cont < 20000 & Household == 2)|(INCGHH_cont < 30000 & Household == 3) ~ "Q1",
                                      (INCGHH_cont >= 15000 & INCGHH_cont <= 29999 & Household == 1)|(INCGHH_cont >= 20000 & INCGHH_cont <= 39999 & Household == 2)|(INCGHH_cont >= 30000 & INCGHH_cont <= 59999 & Household == 3)~ "Q2",
                                      (INCGHH_cont >= 30000 & INCGHH_cont <= 59999 & Household == 1)|(INCGHH_cont >= 40000 & INCGHH_cont <= 79999 & Household == 2)|(INCGHH_cont >= 60000 & INCGHH_cont <= 79999 & Household == 3)~"Q3",
                                      (INCGHH_cont >= 60000 & Household == 1)|(INCGHH_cont >= 80000 & Household == 2)|(INCGHH_cont >= 80000 & Household == 3)~"Q4")) %>%
  mutate(DHHGAGE_C=suppressWarnings(as.numeric(as.character(DHHGAGE_C))))%>%
  # Age Group = 1 for 12-34 years old (DHHGAGE_C=1-6), Age Group = 2 for 35-49 years old (DHHGAGE_C=7-9), Age Group = 3 for 50-64(DHHGAGE_C=10-12), Age Group = 4 for 65-74 years old (DHHGAGE_C=13-14), Age Group = 5 for 75+ years old (15-16)
  # mutate(Age_Group = if_else2(DHHGAGE_C >=1 & DHHGAGE_C <= 6, 1,
  #                             if_else2(DHHGAGE_C >= 7 & DHHGAGE_C <=9, 2,
  #                                      if_else2(DHHGAGE_C >= 10 & DHHGAGE_C <= 12, 3,
  #                                               if_else2(DHHGAGE_C ==13 | DHHGAGE_C == 14, 4,
  #                                                        if_else2(DHHGAGE_C == 15| DHHGAGE_C == 16, 5, "MISSING")))))) %>%
  mutate(Age_Group = case_when(DHHGAGE_C >=1 & DHHGAGE_C <= 6 ~ 1,
                               DHHGAGE_C >= 7 & DHHGAGE_C <=9 ~ 2,
                               DHHGAGE_C >= 10 & DHHGAGE_C <= 12 ~ 3,
                               DHHGAGE_C ==13 | DHHGAGE_C == 14 ~ 4,
                               DHHGAGE_C == 15| DHHGAGE_C == 16 ~ 5)) %>%
  # BMI category = 1 for der BMI <25, BMI category = 2 for der BMI 25-29, BMI category = 3 for der BMI >=30
  mutate(BMI = case_when(HWTGBMI_der < 25 ~ 1,
                         HWTGBMI_der >= 25 & HWTGBMI_der < 29 ~ 2,
                         HWTGBMI_der >= 30 ~ 3)) %>%
  # provinces
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
            GEOGPRV == 60 ~ "Yukon/NWT/Nunavut"))
# refer to pack_year_fun
attach(combined)
combined$Smoke_Status <-pack_years_fun(SMKDSTY, DHHGAGE_cont, SMK_09A_cont, SMKG09C, SMKG203_cont,
                                           SMKG207_cont, SMK_204, SMK_05B,
                                           SMK_208, SMK_05C, SMKG01C_cont, SMK_01A)
# NA?

sum(is.na(REC2013_2014$WTS_M)) # 3 NA in the 2013 weights when using flow but no NA is cchs2013_2014 #

combined$WTS_M[is.na(combined$WTS_M)] <- 99999
combined <- combined %>%
  mutate(WTS_M=replace(WTS_M,WTS_M == 99999 & HWTGBMI == 25.14, 1.07))%>%
  mutate(WTS_M=replace(WTS_M,WTS_M == 99999 & HWTGBMI == 23.83, 1.07)) %>%
  mutate(WTS_M=replace(WTS_M, WTS_M== 99999 & HWTGBMI == 26.91, 22.2))

sum(is.na(combined$WTS_M)) # check
