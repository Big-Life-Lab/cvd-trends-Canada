#### Population ####

##Pop number by year (weighted)
Wpop2001 <- combined %>%
  filter(year == "2001") %>%
  summarise(sum(WTS_M))

Wpop2003 <- combined %>%
  filter(year == "2003") %>%
  summarise(sum(WTS_M))

Wpop2005 <- combined %>%
  filter(year == "2005") %>%
  summarise(sum(WTS_M))

Wpop2007_2008 <- combined %>%
  filter(year == "2007") %>%
  summarise(sum(WTS_M))

Wpop2009_2010 <- combined %>%
  filter(year == "2009") %>%
  summarise(sum(WTS_M))

Wpop2011_2012 <- combined %>%
  filter(year == "2011") %>%
  summarise(sum(WTS_M))

Wpop2013_2014 <- combined %>%
  filter(year == "2013") %>%
  summarise(sum(WTS_M)) 

Wpop <- as.integer(c(Wpop2001,Wpop2003,Wpop2005,Wpop2007_2008,Wpop2009_2010,Wpop2011_2012,Wpop2013_2014))

##Pop number by year and sex (weighted)
#Male
Wpop2001_Male <- combined %>%
  filter(year == "2001" & DHH_SEX ==1) %>%
  summarise(sum(WTS_M))

Wpop2003_Male <- combined %>%
  filter(year == "2003" & DHH_SEX ==1) %>%
  summarise(sum(WTS_M))

Wpop2005_Male <- combined %>%
  filter(year == "2005" & DHH_SEX ==1) %>%
  summarise(sum(WTS_M))

Wpop2007_2008_Male <- combined %>%
  filter(year == "2007" & DHH_SEX ==1) %>%
  summarise(sum(WTS_M))

Wpop2009_2010_Male <- combined %>%
  filter(year == "2009" & DHH_SEX ==1) %>%
  summarise(sum(WTS_M))

Wpop2011_2012_Male <- combined %>%
  filter(year == "2011" & DHH_SEX ==1) %>%
  summarise(sum(WTS_M))

Wpop2013_2014_Male <- combined %>%
  filter(year == "2013" & DHH_SEX ==1) %>%
  summarise(sum(WTS_M)) 

#Female
Wpop2001_Female <- combined %>%
  filter(year == "2001" & DHH_SEX ==2) %>%
  summarise(sum(WTS_M))

Wpop2003_Female <- combined %>%
  filter(year == "2003" & DHH_SEX ==2) %>%
  summarise(sum(WTS_M))

Wpop2005_Female <- combined %>%
  filter(year == "2005" & DHH_SEX ==2) %>%
  summarise(sum(WTS_M))

Wpop2007_2008_Female <- combined %>%
  filter(year == "2007" & DHH_SEX ==2) %>%
  summarise(sum(WTS_M))

Wpop2009_2010_Female <- combined %>%
  filter(year == "2009" & DHH_SEX ==2) %>%
  summarise(sum(WTS_M))

Wpop2011_2012_Female <- combined %>%
  filter(year == "2011" & DHH_SEX ==2) %>%
  summarise(sum(WTS_M))

Wpop2013_2014_Female <- combined %>%
  filter(year == "2013" & DHH_SEX ==2) %>%
  summarise(sum(WTS_M))

#####################################################################################################################
#### survey analysis ####
library(survey)

combined_sd <-svydesign(ids=~1, data = combined, weight = combined$WTS_M)

##### Prevalence #####
prev <- function (var) {
  group_vars <- sym(var)
  svystandardize(combined_sd, by = ~year, over = make.formula(var),
                 population = Wpop) %>%
    group_by(!groups_vars)%>%
    summarize(n = unweighted(n()),
              pct = survey_mean()) %>% 
    mutate_at("pct", function(x) round(100 * x, 1)) %>% 
    mutate_at("pct_se", function(x) round(100 * x, 3))
} #eek


prev("Age_Group")

##### Prevalence of risk factors by age categories #####
### Diabetes ###
#Both sex
DiabetesT1 <-combined %>%
  filter(CCC_101 == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

#Male
DiabetesT1_Male <-combined %>%
  filter(CCC_101 == 1 & DHH_SEX == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Male)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Male)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Male)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Male)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Male)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Male)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Male)*100, .keep="unused") 

#Female 
DiabetesT1_Female <-combined %>%
  filter(CCC_101 == 1 & DHH_SEX == 2) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Female)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Female)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Female)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Female)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Female)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Female)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Female)*100, .keep="unused")

#Combined table
Tb_Diabetes<-rbind("Both sex",DiabetesT1, "Male", DiabetesT1_Male, "Female", DiabetesT1_Female)

### Hypertension ###
#Both sex
HTT1 <-combined %>%
  filter(CCC_071 == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

#Male
HTT1_Male <-combined %>%
  filter(CCC_071 == 1 & DHH_SEX == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Male)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Male)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Male)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Male)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Male)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Male)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Male)*100, .keep="unused") 

#Female 
HTT1_Female <-combined %>%
  filter(CCC_071 == 1 & DHH_SEX == 2) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Female)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Female)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Female)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Female)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Female)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Female)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Female)*100, .keep="unused") 

#Combined table
Tb_HT<-rbind("Both sex",HTT1, "Male", HTT1_Male, "Female", HTT1_Female)

### Obesity ###
#Both sex
BMIT1 <-combined %>%
  filter(BMI == 3) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

#Male
BMIT1_Male <-combined %>%
  filter(BMI == 3 & DHH_SEX == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year, Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Male)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Male)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Male)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Male)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Male)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Male)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Male)*100, .keep="unused") 

#Female 
BMIT1_Female <-combined %>%
  filter(BMI == 3 & DHH_SEX == 2) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Female)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Female)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Female)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Female)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Female)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Female)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Female)*100, .keep="unused") 

#Combined table
Tb_BMI<-rbind("Both sex",BMIT1, "Male", BMIT1_Male, "Female", BMIT1_Female)

### Inactivity ###
#Both sex
METST1 <-combined %>%
  filter(PACDEE<=1.5) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

#Male
METST1_Male <-combined %>%
  filter(PACDEE<=1.5 & DHH_SEX == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Male)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Male)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Male)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Male)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Male)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Male)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Male)*100, .keep="unused") 

#Female 
METST1_Female <-combined %>%
  filter(PACDEE<=1.5 & DHH_SEX == 2) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Female)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Female)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Female)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Female)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Female)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Female)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Female)*100, .keep="unused") 

#Combined table
Tb_METS<-rbind("Both sex",METST1, "Male", METST1_Male, "Female", METST1_Female)

### smoking ###
#Both sex
SmokeT1 <-combined %>%
  filter(SMKDSTY == 1 | SMKDSTY == 2) %>%
  filter(Smoke_Status >= 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

#Male
SmokeT1_Male <-combined %>%
  filter(SMKDSTY == 1 | SMKDSTY == 2) %>%
  filter(Smoke_Status >= 1)%>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Male)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Male)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Male)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Male)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Male)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Male)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Male)*100, .keep="unused") 

#Female 
SmokeT1_Female <-combined %>%
  filter(SMKDSTY == 1 | SMKDSTY == 2) %>%
  filter(Smoke_Status >= 1)%>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Female)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Female)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Female)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Female)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Female)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Female)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Female)*100, .keep="unused") 

#Combined table
Tb_Smoke<-rbind("Both sex",SmokeT1, "Male", SmokeT1_Male, "Female", SmokeT1_Female)
