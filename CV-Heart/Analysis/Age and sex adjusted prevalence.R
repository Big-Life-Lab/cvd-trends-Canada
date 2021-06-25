
# Both sex ----------------------------------------------------------------

# Hypertension
HT1 <-combined %>% 
  filter(CCC_071 == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(HT1)[rownames(HT1)==1]="Hypertension"

# Diabetes
db1 <-combined %>%
  filter(CCC_101 == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(db1)[rownames(db1)==1]="Diabetes mellitus"

# Obesity
bmi1 <-combined %>%
  filter(BMI == 3) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(bmi1)[rownames(bmi1)==1]="Obesity"

# Inactivity
mets1 <-combined %>%
  filter(PACDEE<=1.5) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(mets1)[rownames(mets1)==1]="Inactivity"

# Smoking
smoke1 <-combined %>%
  filter(SMKDSTY == 1 | SMKDSTY == 2) %>%
  filter(Smoke_Status >= 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(smoke1)[rownames(smoke1)==1]="Current smoking"

T2_both <- rbind("", HT1, db1, smoke1, bmi1, mets1)
rownames(T2_both)[rownames(T2_both)==1]="BOTH SEX"


# Male --------------------------------------------------------------------

# Hypertension
HT1_Male <-combined %>% 
  filter(CCC_071 == 1 & DHH_SEX == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Male)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Male)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Male)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Male)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Male)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Male)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Male)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(HT1_Male)[rownames(HT1_Male)==1]="Hypertension"

# Diabetes
db1_Male <-combined %>%
  filter(CCC_101 == 1 & DHH_SEX == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Male)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Male)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Male)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Male)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Male)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Male)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Male)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(db1_Male)[rownames(db1_Male)==1]="Diabetes mellitus"

# Obesity
bmi1_Male <-combined %>%
  filter(BMI == 3 & DHH_SEX == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Male)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Male)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Male)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Male)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Male)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Male)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Male)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(bmi1_Male)[rownames(bmi1_Male)==1]="Obesity"

# Inactivity
mets1_Male <-combined %>%
  filter(PACDEE<=1.5 & DHH_SEX == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Male)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Male)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Male)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Male)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Male)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Male)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Male)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(mets1_Male)[rownames(mets1_Male)==1]="Inactivity"

# Smoking
smoke1_Male <-combined %>%
  filter(SMKDSTY == 1 | SMKDSTY == 2) %>%
  filter(Smoke_Status >= 1 & DHH_SEX == 1) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Male)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Male)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Male)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Male)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Male)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Male)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Male)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(smoke1_Male)[rownames(smoke1_Male)==1]="Current smoking"

T2_Male <- rbind("", HT1_Male, db1_Male, smoke1_Male, bmi1_Male, mets1_Male)
rownames(T2_Male)[rownames(T2_Male)==1]="MALE"


# Female ------------------------------------------------------------------

# Hypertension
HT1_Female <-combined %>% 
  filter(CCC_071 == 1 & DHH_SEX == 2) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Female)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Female)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Female)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Female)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Female)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Female)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Female)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(HT1_Female)[rownames(HT1_Female)==1]="Hypertension"

# Diabetes
db1_Female <-combined %>%
  filter(CCC_101 == 1 & DHH_SEX == 2) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Female)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Female)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Female)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Female)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Female)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Female)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Female)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(db1_Female)[rownames(db1_Female)==1]="Diabetes mellitus"

# Obesity
bmi1_Female <-combined %>%
  filter(BMI == 3 & DHH_SEX == 2) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Female)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Female)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Female)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Female)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Female)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Female)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Female)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(bmi1_Female)[rownames(bmi1_Female)==1]="Obesity"

# Inactivity
mets1_Female <-combined %>%
  filter(PACDEE<=1.5 & DHH_SEX == 2) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Female)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Female)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Female)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Female)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Female)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Female)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Female)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(mets1_Female)[rownames(mets1_Female)==1]="Inactivity"

# Smoking
smoke1_Female <-combined %>%
  filter(SMKDSTY == 1 | SMKDSTY == 2) %>%
  filter(Smoke_Status >= 1 & DHH_SEX == 2) %>%
  select(year,WTS_M) %>%
  group_by(year)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/as.numeric(Wpop2001_Female)*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/as.numeric(Wpop2003_Female)*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/as.numeric(Wpop2005_Female)*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/as.numeric(Wpop2007_2008_Female)*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/as.numeric(Wpop2009_2010_Female)*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/as.numeric(Wpop2011_2012_Female)*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/as.numeric(Wpop2013_2014_Female)*100, .keep="unused") %>%
  mutate(change = ((Prev2013 - Prev2001) / Prev2001*100) )
rownames(smoke1_Female)[rownames(smoke1_Female)==1]="Current smoking"

T2_Female <- rbind("", HT1_Female, db1_Female, smoke1_Female, bmi1_Female, mets1_Female)
rownames(T2_Female)[rownames(T2_Female)==1]="FEMALE"


# Combined ----------------------------------------------------------------

T2_ALL <- rbind(T2_both, T2_Male, T2_Female)
