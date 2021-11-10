##### Age and sex adjusted prevalence of risk factors #####
## Weighted population by year and sex
pop_weight_year <- harmonized_combined %>%
  select(year,WTS_M)%>%
  group_by(year)%>%
  summarise(Weighted_pop=sum(WTS_M))%>% 
  spread(year, Weighted_pop)

pop_weight_male <- harmonized_combined %>%
  select(year,DHH_SEX,WTS_M)%>%
  group_by(year,DHH_SEX,)%>%
  summarise(Weighted_pop=sum(WTS_M))%>% 
  spread(year, Weighted_pop)%>%
  filter(DHH_SEX ==1)

pop_weight_female <- harmonized_combined %>%
  select(year,DHH_SEX,WTS_M)%>%
  group_by(year,DHH_SEX,)%>%
  summarise(Weighted_pop=sum(WTS_M))%>% 
  spread(year, Weighted_pop)%>%
  filter(DHH_SEX ==2)

# Both sex ----------------------------------------------------------------

# Hypertension
HT1 <-harmonized_combined %>% 
  filter(CCC_071 == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_year$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_year$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_year$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_year$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_year$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_year$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_year$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_year$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_year$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(HT1)[rownames(HT1)==1]="Hypertension"

# Diabetes
db1 <-harmonized_combined %>%
  filter(CCC_101 == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_year$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_year$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_year$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_year$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_year$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_year$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_year$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_year$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_year$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(db1)[rownames(db1)==1]="Diabetes mellitus"

# Obesity
bmi1 <-harmonized_combined %>%
  filter(HWTGBMI_der_cat4 %in% c(3:4)) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_year$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_year$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_year$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_year$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_year$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_year$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_year$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_year$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_year$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(bmi1)[rownames(bmi1)==1]="Obesity"

# Inactivity
mets1 <-harmonized_combined %>%
  filter(activity == "Inactive") %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_year$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_year$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_year$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_year$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_year$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_year$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_year$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_year$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_year$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(mets1)[rownames(mets1)==1]="Inactivity"

# Smoking
smoke1 <-harmonized_combined %>%
  filter(SMKDSTY_cat3 == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_year$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_year$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_year$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_year$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_year$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_year$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_year$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_year$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_year$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(smoke1)[rownames(smoke1)==1]="Current smoking"

T2_both <- rbind("", HT1, db1, smoke1, bmi1, mets1)
rownames(T2_both)[rownames(T2_both)==1]="BOTH SEX"


# Male --------------------------------------------------------------------

# Hypertension
HT1_Male <-harmonized_combined %>% 
  filter(CCC_071 == 1 & DHH_SEX == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_male$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_male$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_male$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_male$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_male$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_male$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_male$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_male$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_male$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(HT1_Male)[rownames(HT1_Male)==1]="Hypertension"

# Diabetes
db1_Male <-harmonized_combined %>%
  filter(CCC_101 == 1 & DHH_SEX == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_male$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_male$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_male$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_male$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_male$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_male$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_male$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_male$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_male$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(db1_Male)[rownames(db1_Male)==1]="Diabetes mellitus"

# Obesity
bmi1_Male <-harmonized_combined %>%
  filter(HWTGBMI_der_cat4 %in% c(3:4) & DHH_SEX == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_male$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_male$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_male$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_male$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_male$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_male$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_male$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_male$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_male$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(bmi1_Male)[rownames(bmi1_Male)==1]="Obesity"

# Inactivity
mets1_Male <-harmonized_combined %>%
  filter(activity == "Inactive" & DHH_SEX == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_male$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_male$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_male$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_male$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_male$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_male$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_male$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_male$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_male$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(mets1_Male)[rownames(mets1_Male)==1]="Inactivity"

# Smoking
smoke1_Male <-harmonized_combined %>%
  filter(SMKDSTY_cat3 == 1 & DHH_SEX == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_male$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_male$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_male$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_male$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_male$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_male$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_male$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_male$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_male$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(smoke1_Male)[rownames(smoke1_Male)==1]="Current smoking"

T2_Male <- rbind("", HT1_Male, db1_Male, smoke1_Male, bmi1_Male, mets1_Male)
rownames(T2_Male)[rownames(T2_Male)==1]="MALE"


# Female ------------------------------------------------------------------

# Hypertension
HT1_Female <-harmonized_combined %>% 
  filter(CCC_071 == 1 & DHH_SEX == 2) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_female$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_female$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_female$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_female$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_female$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_female$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_female$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_female$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_female$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(HT1_Female)[rownames(HT1_Female)==1]="Hypertension"

# Diabetes
db1_Female <-harmonized_combined %>%
  filter(CCC_101 == 1 & DHH_SEX == 2) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_female$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_female$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_female$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_female$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_female$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_female$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_female$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_female$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_female$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(db1_Female)[rownames(db1_Female)==1]="Diabetes mellitus"

# Obesity
bmi1_Female <-harmonized_combined %>%
  filter(HWTGBMI_der_cat4 %in% c(3:4) & DHH_SEX == 2) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_female$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_female$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_female$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_female$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_female$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_female$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_female$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_female$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_female$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(bmi1_Female)[rownames(bmi1_Female)==1]="Obesity"

# Inactivity
mets1_Female <-harmonized_combined %>%
  filter(activity == "Inactive" & DHH_SEX == 2) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_female$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_female$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_female$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_female$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_female$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_female$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_female$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_female$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_female$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(mets1_Female)[rownames(mets1_Female)==1]="Inactivity"

# Smoking
smoke1_Female <-harmonized_combined %>%
  filter(SMKDSTY_cat3 == 1 & DHH_SEX == 2) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight_female$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight_female$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight_female$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight_female$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight_female$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight_female$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight_female$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight_female$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight_female$`2017`*100, .keep="unused") %>%
  mutate(Change = ((Prev2017 - Prev2001) / Prev2001*100) )
rownames(smoke1_Female)[rownames(smoke1_Female)==1]="Current smoking"

T2_Female <- rbind("", HT1_Female, db1_Female, smoke1_Female, bmi1_Female, mets1_Female)
rownames(T2_Female)[rownames(T2_Female)==1]="FEMALE"


# harmonized_combined ----------------------------------------------------------------

T2_ALL <- rbind(T2_both, T2_Male, T2_Female)
