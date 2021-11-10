##### Prevalence of risk factors by age categories #####
pop_weight_year_age_age <- harmonized_combined %>%
  select(year,DHH_SEX,Age_Group,WTS_M)%>%
  group_by(year,Age_Group)%>%
  summarise(Weighted_pop=sum(WTS_M))%>% 
  spread(year, Weighted_pop)

pop_weight_male_age <- harmonized_combined %>%
  select(year,DHH_SEX,Age_Group,WTS_M)%>%
  group_by(year,Age_Group,DHH_SEX)%>%
  summarise(Weighted_pop=sum(WTS_M))%>% 
  spread(year, Weighted_pop)%>%
  filter(DHH_SEX ==1)

pop_weight_female_age <- harmonized_combined %>%
  select(year,DHH_SEX,Age_Group,WTS_M)%>%
  group_by(year,Age_Group,DHH_SEX)%>%
  summarise(Weighted_pop=sum(WTS_M))%>% 
  spread(year, Weighted_pop)%>%
  filter(DHH_SEX ==2)

### Diabetes ###
#Both sex
DiabetesT1 <-harmonized_combined %>%
  filter(CCC_101 == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_year_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_year_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_year_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_year_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_year_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_year_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_year_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_year_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_year_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )

#Male
DiabetesT1_Male <-harmonized_combined %>%
  filter(CCC_101 == 1 & DHH_SEX == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_male_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_male_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_male_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_male_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_male_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_male_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_male_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_male_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_male_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
#Female 
DiabetesT1_Female <-harmonized_combined %>%
  filter(CCC_101 == 1 & DHH_SEX == 2) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_female_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_female_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_female_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_female_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_female_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_female_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_female_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_female_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_female_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )

#Combined table
Tb_Diabetes<-rbind("",DiabetesT1, "", DiabetesT1_Male, "", DiabetesT1_Female)
rownames(Tb_Diabetes)[rownames(Tb_Diabetes)==1]="Both sex"
rownames(Tb_Diabetes)[rownames(Tb_Diabetes)==7]="Male"
rownames(Tb_Diabetes)[rownames(Tb_Diabetes)==13]="Female"

### Hypertension ###
#Both sex
HTT1 <-harmonized_combined %>%
  filter(CCC_071 == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_year_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_year_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_year_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_year_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_year_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_year_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_year_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_year_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_year_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) ) 

#Male
HTT1_Male <-harmonized_combined %>%
  filter(CCC_071 == 1 & DHH_SEX == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_male_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_male_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_male_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_male_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_male_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_male_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_male_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_male_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_male_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )

#Female 
HTT1_Female <-harmonized_combined %>%
  filter(CCC_071 == 1 & DHH_SEX == 2) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_female_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_female_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_female_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_female_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_female_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_female_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_female_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_female_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_female_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) ) 

#Combined table
Tb_HT<-rbind("",HTT1, "", HTT1_Male, "", HTT1_Female)
rownames(Tb_HT)[rownames(Tb_HT)==1]="Both sex"
rownames(Tb_HT)[rownames(Tb_HT)==7]="Male"
rownames(Tb_HT)[rownames(Tb_HT)==13]="Female"

### Obesity ###
#Both sex
BMIT1 <-harmonized_combined %>%
  filter(HWTGBMI_der_cat4 %in% c(3:4)) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_year_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_year_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_year_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_year_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_year_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_year_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_year_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_year_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_year_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )

#Male
BMIT1_Male <-harmonized_combined %>%
  filter(HWTGBMI_der_cat4 %in% c(3:4) & DHH_SEX == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year, Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_male_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_male_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_male_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_male_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_male_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_male_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_male_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_male_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_male_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) ) 

#Female 
BMIT1_Female <-harmonized_combined %>%
  filter(HWTGBMI_der_cat4 %in% c(3:4) & DHH_SEX == 2) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_female_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_female_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_female_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_female_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_female_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_female_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_female_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_female_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_female_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )

#Combined table
Tb_BMI<-rbind("",BMIT1, "", BMIT1_Male, "", BMIT1_Female)
rownames(Tb_BMI)[rownames(Tb_BMI)==1]="Both sex"
rownames(Tb_BMI)[rownames(Tb_BMI)==7]="Male"
rownames(Tb_BMI)[rownames(Tb_BMI)==13]="Female"

### Inactivity ###
#Both sex
METST1 <-harmonized_combined %>%
  filter(activity == "Inactive") %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_year_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_year_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_year_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_year_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_year_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_year_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_year_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_year_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_year_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) ) 

#Male
METST1_Male <-harmonized_combined %>%
  filter(activity == "Inactive" & DHH_SEX == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_male_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_male_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_male_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_male_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_male_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_male_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_male_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_male_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_male_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) ) 

#Female 
METST1_Female <-harmonized_combined %>%
  filter(activity == "Inactive" & DHH_SEX == 2) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_female_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_female_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_female_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_female_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_female_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_female_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_female_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_female_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_female_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )

#Combined table
Tb_METS<-rbind("",METST1, "", METST1_Male, "", METST1_Female)
rownames(Tb_METS)[rownames(Tb_METS)==1]="Both sex"
rownames(Tb_METS)[rownames(Tb_METS)==7]="Male"
rownames(Tb_METS)[rownames(Tb_METS)==13]="Female"

### smoking ###
#Both sex
SmokeT1 <-harmonized_combined %>%
  filter(SMKDSTY_cat3 == 1) %>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_year_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_year_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_year_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_year_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_year_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_year_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_year_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_year_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_year_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) ) 

#Male
SmokeT1_Male <-harmonized_combined %>%
  filter(SMKDSTY_cat3 == 1)%>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_male_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_male_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_male_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_male_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_male_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_male_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_male_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_male_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_male_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )

#Female 
SmokeT1_Female <-harmonized_combined %>%
  filter(SMKDSTY_cat3 == 1)%>%
  select(year,Age_Group,WTS_M) %>%
  group_by(year,Age_Group)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight)%>%
  mutate(Prev2001 = `2001`/pop_weight_female_age$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_female_age$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_female_age$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_female_age$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_female_age$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_female_age$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_female_age$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_female_age$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_female_age$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )

#Combined table
Tb_Smoke<-rbind("",SmokeT1, "", SmokeT1_Male, "", SmokeT1_Female)
rownames(Tb_Smoke)[rownames(Tb_Smoke)==1]="Both sex"
rownames(Tb_Smoke)[rownames(Tb_Smoke)==7]="Male"
rownames(Tb_Smoke)[rownames(Tb_Smoke)==13]="Female"

