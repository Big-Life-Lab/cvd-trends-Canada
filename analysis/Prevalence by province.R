##### Age and sex adjusted prevalence by province #####
## Weighted population by year and province
pop_weight1 <- harmonized_combined %>%
  select(year, Province,WTS_M)%>%
  group_by(year, Province)%>%
  summarise(Weighted_pop=sum(WTS_M))%>% 
  spread(year, Weighted_pop)

# Hypertension
HT_pt <-harmonized_combined %>% 
  filter(CCC_071 == 1) %>%
  select(year,Province,WTS_M) %>%
  group_by(year,Province)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight1$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight1$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight1$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight1$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight1$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight1$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight1$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight1$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight1$`2017`*100, .keep="unused") %>%
  mutate(change = ((Prev2017 - Prev2001) / Prev2001*100) )

# Diabetes
db_pt <-harmonized_combined %>%
  filter(CCC_101 == 1) %>%
  select(year,Province,WTS_M) %>%
  group_by(year,Province)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight1$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight1$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight1$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight1$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight1$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight1$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight1$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight1$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight1$`2017`*100, .keep="unused") %>%
  mutate(change = ((Prev2017 - Prev2001) / Prev2001*100) )

# Obesity
bmi_pt <-harmonized_combined %>%
  filter(HWTGBMI_der_cat4 %in% c(3:4)) %>%
  select(year,Province,WTS_M) %>%
  group_by(year,Province)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight1$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight1$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight1$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight1$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight1$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight1$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight1$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight1$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight1$`2017`*100, .keep="unused") %>%
  mutate(change = ((Prev2017 - Prev2001) / Prev2001*100) )

# Inactivity
mets_pt <-harmonized_combined %>%
  filter(activity=="Inactive") %>%
  select(year,Province,WTS_M) %>%
  group_by(year,Province)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight1$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight1$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight1$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight1$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight1$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight1$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight1$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight1$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight1$`2017`*100, .keep="unused") %>%
  mutate(change = ((Prev2017 - Prev2001) / Prev2001*100) )

# Smoking
smoke_pt <-harmonized_combined %>%
  filter(SMKDSTY_cat3 == 1) %>%
  select(year,Province,WTS_M) %>%
  group_by(year,Province)%>%
  summarise(Weight=sum(WTS_M))%>%
  spread(year,Weight) %>%
  mutate(Prev2001 = `2001`/pop_weight1$`2001`*100, .keep="unused") %>%
  mutate(Prev2003 = `2003`/pop_weight1$`2003`*100, .keep="unused") %>%
  mutate(Prev2005 = `2005`/pop_weight1$`2005`*100, .keep="unused") %>%
  mutate(Prev2007 = `2007`/pop_weight1$`2007`*100, .keep="unused") %>%
  mutate(Prev2009 = `2009`/pop_weight1$`2009`*100, .keep="unused") %>%
  mutate(Prev2011 = `2011`/pop_weight1$`2011`*100, .keep="unused") %>%
  mutate(Prev2013 = `2013`/pop_weight1$`2013`*100, .keep="unused") %>%
  mutate(Prev2015 = `2015`/pop_weight1$`2015`*100, .keep="unused") %>%
  mutate(Prev2017 = `2017`/pop_weight1$`2017`*100, .keep="unused") %>%
  mutate(change = ((Prev2017 - Prev2001) / Prev2001*100) )


Tb_pt <- rbind("", HT_pt, "",db_pt, "",smoke_pt, "",bmi_pt, "",mets_pt)
rownames(Tb_pt)[rownames(Tb_pt)==1]="Hypertension"
rownames(Tb_pt)[rownames(Tb_pt)==13]="Diabetes mellitus"
rownames(Tb_pt)[rownames(Tb_pt)==25]="Obesity"
rownames(Tb_pt)[rownames(Tb_pt)==37]="Inactivity"
rownames(Tb_pt)[rownames(Tb_pt)==49]="Smoking"