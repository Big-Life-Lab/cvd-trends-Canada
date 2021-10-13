##### Prevalence by categories of income and BMI #####

# Income ------------------------------------------------------------------
## 2003 (2001 all NA because no household available)

combined_2003 <- combined %>% filter(year == 2003)
# Hypertension
HT_income2003 <-combined_2003 %>% 
  filter(CCC_071 == 1) %>%
  select(Income_Adequency,WTS_M) %>%
  group_by(Income_Adequency)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate(Hypertension = `Weight`/as.numeric(Wpop2003)*100, .keep="unused") 

# Diabetes
db_income2003 <-combined_2003 %>%
  filter(CCC_101 == 1) %>%
  select(Income_Adequency,WTS_M) %>%
  group_by(Income_Adequency)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Diabetes mellitus" = `Weight`/as.numeric(Wpop2003)*100, .keep="unused") 

# Obesity
bmi_income2003 <-combined_2003 %>%
  filter(BMI == 3) %>%
  select(Income_Adequency,WTS_M) %>%
  group_by(Income_Adequency)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Obesity" = `Weight`/as.numeric(Wpop2003)*100, .keep="unused") 

# Inactivity
mets_income2003 <-combined_2003 %>%
  filter(PACDEE<=1.5) %>%
  select(Income_Adequency,WTS_M) %>%
  group_by(Income_Adequency)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Inactivity" = `Weight`/as.numeric(Wpop2003)*100, .keep="unused") 

# Smoking
smoke_income2003 <-combined_2003 %>%
  filter(SMKDSTY == 1 | SMKDSTY == 2) %>%
  filter(Smoke_Status >= 1) %>%
  select(Income_Adequency,WTS_M) %>%
  group_by(Income_Adequency)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Smoking" = `Weight`/as.numeric(Wpop2003)*100, .keep="unused") 

Tb_income2003 <- cbind(HT_income2003, db_income2003[-1], smoke_income2003[-1], bmi_income2003[-1], mets_income2003[-1])

## 2013
combined_2013 <- combined %>% filter(year == 2013)
# Hypertension
HT_income2013 <-combined_2013 %>% 
  filter(CCC_071 == 1) %>%
  select(Income_Adequency,WTS_M) %>%
  group_by(Income_Adequency)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate(Hypertension = `Weight`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

# Diabetes
db_income2013 <-combined_2013 %>%
  filter(CCC_101 == 1) %>%
  select(Income_Adequency,WTS_M) %>%
  group_by(Income_Adequency)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Diabetes mellitus" = `Weight`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

# Obesity
bmi_income2013 <-combined_2013 %>%
  filter(BMI == 3) %>%
  select(Income_Adequency,WTS_M) %>%
  group_by(Income_Adequency)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Obesity" = `Weight`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

# Inactivity
mets_income2013 <-combined_2013 %>%
  filter(PACDEE<=1.5) %>%
  select(Income_Adequency,WTS_M) %>%
  group_by(Income_Adequency)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Inactivity" = `Weight`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

# Smoking
smoke_income2013 <-combined_2013 %>%
  filter(SMKDSTY == 1 | SMKDSTY == 2) %>%
  filter(Smoke_Status >= 1) %>%
  select(Income_Adequency,WTS_M) %>%
  group_by(Income_Adequency)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Smoking" = `Weight`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

Tb_income2013 <- cbind(HT_income2013, db_income2013[-1], smoke_income2013[-1], bmi_income2013[-1], mets_income2013[-1])
Tb_income <- rbind("",Tb_income2003,"",Tb_income2013)
rownames(Tb_income)[rownames(Tb_income)==1]= 2003
rownames(Tb_income)[rownames(Tb_income)==7]= 2013

# BMI --------------------------------------------------------------------
## 2003 (2001 all NA because no household available)

combined_2003 <- combined %>% filter(year == 2003)
# Hypertension
HT_bmi2003 <-combined_2003 %>% 
  filter(CCC_071 == 1) %>%
  select(BMI,WTS_M) %>%
  group_by(BMI)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate(Hypertension = `Weight`/as.numeric(Wpop2003)*100, .keep="unused") 

# Diabetes
db_bmi2003 <-combined_2003 %>%
  filter(CCC_101 == 1) %>%
  select(BMI,WTS_M) %>%
  group_by(BMI)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Diabetes mellitus" = `Weight`/as.numeric(Wpop2003)*100, .keep="unused") 

# Obesity
bmi_bmi2003 <-combined_2003 %>%
  filter(BMI == 3) %>%
  select(BMI,WTS_M) %>%
  group_by(BMI)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Obesity" = `Weight`/as.numeric(Wpop2003)*100, .keep="unused") 

# Inactivity
mets_bmi2003 <-combined_2003 %>%
  filter(PACDEE<=1.5) %>%
  select(BMI,WTS_M) %>%
  group_by(BMI)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Inactivity" = `Weight`/as.numeric(Wpop2003)*100, .keep="unused") 

# Smoking
smoke_bmi2003 <-combined_2003 %>%
  filter(SMKDSTY == 1 | SMKDSTY == 2) %>%
  filter(Smoke_Status >= 1) %>%
  select(BMI,WTS_M) %>%
  group_by(BMI)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Smoking" = `Weight`/as.numeric(Wpop2003)*100, .keep="unused") 

Tb_bmi2003 <- cbind(HT_bmi2003, db_bmi2003[-1], smoke_bmi2003[-1], bmi_bmi2003[-1], mets_bmi2003[-1])

## 2013
combined_2013 <- combined %>% filter(year == 2013)
# Hypertension
HT_bmi2013 <-combined_2013 %>% 
  filter(CCC_071 == 1) %>%
  select(BMI,WTS_M) %>%
  group_by(BMI)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate(Hypertension = `Weight`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

# Diabetes
db_bmi2013 <-combined_2013 %>%
  filter(CCC_101 == 1) %>%
  select(BMI,WTS_M) %>%
  group_by(BMI)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Diabetes mellitus" = `Weight`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

# Obesity
bmi_bmi2013 <-combined_2013 %>%
  filter(BMI == 3) %>%
  select(BMI,WTS_M) %>%
  group_by(BMI)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Obesity" = `Weight`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

# Inactivity
mets_bmi2013 <-combined_2013 %>%
  filter(PACDEE<=1.5) %>%
  select(BMI,WTS_M) %>%
  group_by(BMI)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Inactivity" = `Weight`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

# Smoking
smoke_bmi2013 <-combined_2013 %>%
  filter(SMKDSTY == 1 | SMKDSTY == 2) %>%
  filter(Smoke_Status >= 1) %>%
  select(BMI,WTS_M) %>%
  group_by(BMI)%>%
  summarise(Weight=sum(WTS_M))%>%
  mutate("Smoking" = `Weight`/as.numeric(Wpop2013_2014)*100, .keep="unused") 

Tb_bmi2013 <- cbind(HT_bmi2013, db_bmi2013[-1], smoke_bmi2013[-1], bmi_bmi2013[-1], mets_bmi2013[-1])
Tb_bmi <- rbind("",Tb_bmi2003,"",Tb_bmi2013)
rownames(Tb_bmi)[rownames(Tb_bmi)==1]= 2003
rownames(Tb_bmi)[rownames(Tb_bmi)==6]= 2013

