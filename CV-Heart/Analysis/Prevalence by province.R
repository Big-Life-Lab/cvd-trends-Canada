##### Age and sex adjusted prevalence by province #####
# Hypertension
HT_pt <-combined %>% 
  filter(CCC_071 == 1) %>%
  select(year,Province,WTS_M) %>%
  group_by(year,Province)%>%
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

# Diabetes
db_pt <-combined %>%
  filter(CCC_101 == 1) %>%
  select(year,Province,WTS_M) %>%
  group_by(year,Province)%>%
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

# Obesity
bmi_pt <-combined %>%
  filter(BMI == 3) %>%
  select(year,Province,WTS_M) %>%
  group_by(year,Province)%>%
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

# Inactivity
mets_pt <-combined %>%
  filter(PACDEE<=1.5) %>%
  select(year,Province,WTS_M) %>%
  group_by(year,Province)%>%
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

# Smoking
smoke_pt <-combined %>%
  filter(SMKDSTY == 1 | SMKDSTY == 2) %>%
  filter(Smoke_Status >= 1) %>%
  select(year,Province,,WTS_M) %>%
  group_by(year,Province)%>%
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


Tb_pt <- rbind("", HT_pt, "",db_pt, "",smoke_pt, "",bmi_pt, "",mets_pt)
rownames(Tb_pt)[rownames(Tb_pt)==1]="Hypertension"
rownames(Tb_pt)[rownames(Tb_pt)==13]="Diabetes mellitus"
rownames(Tb_pt)[rownames(Tb_pt)==25]="Obesity"
rownames(Tb_pt)[rownames(Tb_pt)==37]="Inactivity"
rownames(Tb_pt)[rownames(Tb_pt)==49]="Smoking"