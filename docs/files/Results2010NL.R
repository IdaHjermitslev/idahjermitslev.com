# Clear environment
rm(list=ls())

# Set working directory
setwd("Z:/Documents/Mainstream Sellouts")

# Load packages
library(tidyverse)
library(haven)
library(lubridate)
library(psych)
library(xtable)
library(stargazer)

data2010 <- read_sav("DPES2010.sav") %>%
  dplyr::select(individual=rnr, gender=v010, age=v012, edu = v410,
                interest=v025, news=v503, member=v066,
                #Euthanasia
                euthanasia_CDA=v130, euthanasia_PVDA=v131, euthanasia_VVD=v132, euthanasia_D66=v134,
                euthanasia_PVV=v133, euthanasia_SP=v135,
                #Income
                income_CDA=v140, income_PVDA=v141, income_VVD=v142, income_D66=v143,
                income_PVV=v144, income_SP=v145,
                #Ethnic
                ethnic_CDA=v170, ethnic_PVDA=v171, ethnic_VVD=v172, ethnic_D66=v173, ethnic_PVV=v174,
                ethnic_SP=v175,
                #Europe
                europe_CDA=v690, europe_PVDA=v691, europe_VVD=v692, europe_D66=v693,
                europe_PVV=v694, europe_SP=v695,
                #General
                general_CDA=v700, general_PVDA=v701, general_VVD=v702, general_D66=v703,
                general_GL=v704, general_SP=v705, general_PVV=v706, 
                euthanasia_self=v136, income_self=v146,  
                ethnic_self=v176, europe_self= v696, general_self=v711)%>%
  mutate(individual = as.character(individual),
         year=2010,
         age = ifelse(age > 900, NA, age),
         gender = ifelse(gender > 900, NA, gender),
         gender = gender - 1,
         edu = ifelse(edu > 900, NA, edu),
         interest = ifelse(interest > 900, NA, interest),
         interest = scales::rescale(interest, to=c(0,1)),
         news = ifelse(news > 900, NA, news),
         news = scales::rescale(news, to=c(0,1)),
         member = ifelse(member > 900, NA, member)) %>%
  # Rescale all variables to 0-1
  mutate_at(vars(euthanasia_CDA:europe_SP), function(x) ifelse(x <= 7, x, NA)) %>%
  mutate_at(vars(euthanasia_CDA:europe_SP), function(x) scales::rescale(x, to=c(0,1)))%>%
  mutate_at(vars(income_CDA:income_SP), function(x) reverse.code(keys=-1,items=x))%>%
  mutate_at(vars(general_CDA:general_PVV), function(x) ifelse(x <= 10, x, NA)) %>%
  mutate_at(vars(general_CDA:general_PVV), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(euthanasia_self:europe_self), function(x) ifelse(x <= 7, x, NA)) %>%
  mutate_at(vars(euthanasia_self:europe_self), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate(general_self = ifelse(general_self <= 10, general_self, NA),
         general_self = scales::rescale(general_self, to=c(0,1)),
         income_self = reverse.code(keys=-1, items = income_self)) %>%
  # Change to a long format
  gather(variable, perPos, euthanasia_CDA:general_PVV) %>%
  # Divide "variable" into issue and party 
  separate(variable, c("issue", "party")) %>%
  # Create a column for respondent's self-placement
  gather(variable, selfPos, euthanasia_self:general_self) %>%
  # Separate "variable" into the issue label and a temporary placeholder with word "self"
  separate(variable, c("selfIssue","temp")) %>%
  dplyr::select(-temp) %>%
  # Make sure that self-placement and party perceptions are on same issue
  filter(issue == selfIssue)

nrow(data2010) #81251

data2012 <- read_sav("DPES2012.sav") %>%
  dplyr::select(individual=RNR, gender=V341, age=V340, edu = V344,
                interest=V014, news=V010,
                #Euthanasia
                euthanasia_CDA=V510, euthanasia_PVDA=V511, euthanasia_VVD=V512, euthanasia_D66=V513,
                euthanasia_PVV=V514, euthanasia_SP=V515,
                #Income
                income_CDA=V100, income_PVDA=V101, income_VVD=V102, income_D66=V103,
                income_PVV=V104, income_SP=V105,
                #Ethnic
                ethnic_CDA=V517, ethnic_PVDA=V518, ethnic_VVD=V519, ethnic_D66=V520, ethnic_PVV=V521,
                ethnic_SP=V522,
                #Europe
                europe_CDA=V107, europe_PVDA=V108, europe_VVD=V109, europe_D66=V110,
                europe_PVV=V111, europe_SP=V112,
                #General
                general_CDA=V120, general_PVDA=V121, general_VVD=V122, general_D66=V123,
                general_GL=V124, general_SP=V125, general_PVV=V126, 
                euthanasia_self=V516, income_self=V106, ethnic_self=V115, europe_self= V113,  general_self=V130)%>%
  mutate(individual = as.character(individual),
         year=2012,
         age = ifelse(age > 900, NA, age),
         gender = ifelse(gender > 900, NA, gender),
         gender = gender - 1,
         edu = ifelse(edu > 900, NA, edu),
         interest = ifelse(interest > 900, NA, interest),
         interest = reverse.code(keys=-1,items=interest),
         interest = scales::rescale(interest, to=c(0,1)),
         news = ifelse(news > 900, NA, news),
         news = scales::rescale(news, to=c(0,1))) %>%
  # Rescale all variables to 0-1
  mutate_at(vars(euthanasia_CDA:europe_SP), function(x) ifelse(x <= 7, x, NA)) %>%
  mutate_at(vars(euthanasia_CDA:europe_SP), function(x) scales::rescale(x, to=c(0,1)))%>%
  mutate_at(vars(income_CDA:income_SP), function(x) reverse.code(keys=-1,items=x))%>%
  mutate_at(vars(general_CDA:general_PVV), function(x) ifelse(x <= 10, x, NA)) %>%
  mutate_at(vars(general_CDA:general_PVV), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(euthanasia_self:europe_self), function(x) ifelse(x <= 7, x, NA)) %>%
  mutate_at(vars(euthanasia_self:europe_self), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate(general_self = ifelse(general_self <= 10, general_self, NA),
         general_self = scales::rescale(general_self, to=c(0,1)),
         income_self = reverse.code(keys=-1, items = income_self)) %>%
  # Change to a long format
  gather(variable, perPos, euthanasia_CDA:general_PVV) %>%
  # Divide "variable" into issue and party 
  separate(variable, c("issue", "party")) %>%
  # Create a column for respondent's self-placement
  gather(variable, selfPos, euthanasia_self:general_self) %>%
  # Separate "variable" into the issue label and a temporary placeholder with word "self"
  separate(variable, c("selfIssue","temp")) %>%
  dplyr::select(-temp) %>%
  # Make sure that self-placement and party perceptions are on same issue
  filter(issue == selfIssue) 

nrow(data2012) #51987

#Stack the two datasets
dataFull <- bind_rows(list(data2010, data2012)) %>%
  dplyr::select(year, individual, age, gender, edu, interest, news, issue, party, perPos, selfPos) %>%
  # Handcode CMP ids
  mutate(cmp = case_when(
           party == "CDA" ~ 22521, 
           party == "PVDA" ~ 22320,
           party == "VVD"  ~ 22420,
           party == "D66"~ 22330,
           party == "GL" ~ 22110,
           party == "PVV" ~ 22722,
           party == "SP" ~ 22220,
           party == "CU" ~ 22526)) %>%
  na.omit()

table(dataFull$year, dataFull$issue)


# CMP categories derived from Wagner 2011
cmp <- read_dta("MARPOR2020.dta") %>%
  mutate(year = floor(date/100),
         general = scales::rescale(rile, to = c(0,1)),
         euthanasia = per603 - per604,
         euthanasia = scales::rescale(euthanasia, to = c(0,1)),
         income = per505- per507-per504-per506,
         income = scales::rescale(income, to = c(0,1)),
         ethnic = per601 + per608 - per602 - per607, 
         ethnic = scales::rescale(ethnic, to = c(0,1)),
         europe = per110 - per108,
         europe = scales::rescale(europe, to = c(0,1))) %>%
  dplyr::select(year, cmp = party, general, euthanasia, income, ethnic, europe) %>% 
  gather(issue, cmpPos, general:europe) 


dataFull <- dataFull %>%
  left_join(cmp) %>%
  mutate(
    # Define dummy variables
    after = ifelse(year > 2010,1,0),
    center_right = ifelse(party == "VVD" | party == "CDA", 1,0),
    radical_right = ifelse(party == "PVV",1,0),
    party_fam = case_when(
      center_right == 1 ~ "center_right",
      radical_right == 1 ~ "radical_right",
      center_right == 0 & radical_right == 0 ~ "all_others"
    ),
    party_fam = as.factor(party_fam))

dataFull %>% 
  group_by(issue, party, year) %>% 
  summarize(avgPer = mean(perPos, na.rm=TRUE), 
            avgCMP = mean(cmpPos, na.rm=TRUE)) %>%
  ungroup() %>%
  View()

descriptives <- dataFull %>%
  group_by(after, issue, party) %>%
  summarise(meanPos = mean(perPos, na.rm=TRUE),
            sdPos = sd(perPos, na.rm=TRUE)) %>%
  ungroup

descriptives %>%
  filter(issue=="income") %>%
  xtable(digits=2)

table(dataFull$issue, dataFull$party)

### ALL parties together CMP

modelEthCMP <- dataFull %>% 
  filter(issue == "ethnic") %>%
  filter(party != "SP") %>%
  lm(perPos ~ cmpPos  + after + party_fam + after*party_fam + selfPos + age + gender + as.factor(edu) + interest + news, data=.)

modelEUCMP <- dataFull %>% 
  filter(issue == "europe") %>%
  lm(perPos ~ cmpPos + after + party_fam + after*party_fam + selfPos + age + gender + as.factor(edu) + interest + news, data=.)

modelEuthCMP <- dataFull %>% 
  filter(issue == "euthanasia") %>%
  lm(perPos ~ cmpPos + after + party_fam + after*party_fam + selfPos + age + gender + as.factor(edu) + interest + news, data=.)

modelIncCMP <- dataFull %>% 
  filter(issue == "income") %>%
  lm(perPos ~ cmpPos + after + party_fam + after*party_fam + selfPos + age + gender + as.factor(edu) + interest + news, data=.)

modelLRCMP <- dataFull %>% 
  filter(issue == "general") %>%
  lm(perPos ~ cmpPos + after + party_fam + after*party_fam + selfPos + age + gender + as.factor(edu) + interest + news, data=.)

stargazer(modelLRCMP, modelEthCMP, modelIncCMP)

stargazer(modelEUCMP, modelEuthCMP)
