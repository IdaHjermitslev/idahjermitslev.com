# Clear environment
rm(list=ls())

# Set working directory
setwd("Z:/Documents/Mainstream Sellouts")

# Load packages
library(tidyverse)
library(haven)
library(lubridate)
library(lfe)
library(scales)
library(stargazer)
library(xtable)

originalPRE <- read_sav("NKO2002.sav") %>%
  dplyr::select(ridn, date = datpre,
                #Pre-election survey 2002
                 #Asylum seekers
                asy_CDA=v0216, asy_PvdA=v0217, asy_VVD=v0218, asy_D66=v0219, asy_LPF=v0220
  )%>%
  mutate(date = ymd(date)) %>%
  mutate_at(vars(asy_CDA:asy_LPF), function(x) ifelse(x <= 7, x, NA))

originalPOST <- read_sav("NKO2002.sav") %>%
  dplyr::select(ridn, date = datpos,
                #Post-election survey 2002
                #Ethnic minorities
                eth_CDA=v0876, eth_PvdA=v0877, eth_VVD=v0878, eth_D66=v0879, eth_GL=v0880, eth_LPF=v0881, 
                #European unification
                eu_CDA=v0883, eu_PvdA=v0884, eu_VVD=v0885, eu_D66=v0886, eu_GL=v0887, eu_LPF=v0888,
                # Left-right
                lr_PvdA=v0906, lr_VVD=v0907, lr_D66=v0908, lr_GL=v0909, lr_CDA=v0910, lr_SGP=v0911, lr_CU=v0912, lr_LN=v0913, lr_SP=v0914, lr_LPF=v0915)%>%
  mutate(date = ymd(date)) %>%
  mutate_at(vars(eth_CDA:eu_LPF), function(x) ifelse(x <= 7, x, NA))%>%
  mutate_at(vars(lr_PvdA:lr_LPF), function(x) ifelse(x <= 11, x, NA))

original2003 <- read_sav("NKO2002.sav") %>%
  dplyr::select(ridn, date=date03,
                #Post-election survey 2003
                #Asylum seekers
                asy_CDA=x0126, asy_PvdA=x0127, asy_VVD=x0128, asy_D66=x0129, asy_LPF=x0130,
                #European unification
                eu_CDA=x0134, eu_PvdA=x0135, eu_VVD=x0136, eu_D66=x0137, eu_LPF=x0138, eu_GL=x0140,
                #Ethnic minorities
                eth_CDA=x0142, eth_PvdA=x0143, eth_VVD=x0144, eth_D66=x0145, eth_LPF=x0146, eth_GL=x0418, 
                #Left-right
                lr_PvdA=x0375, lr_VVD=x0376, lr_D66=x0377, lr_GL=x0378, lr_CDA=x0379, lr_SGP=x0380, lr_CU=x0381, lr_LN=x0382, lr_SP=x0383, lr_LPF=x0384) %>%
  mutate(date = ymd(date)) %>%
  mutate_at(vars(asy_CDA:eth_GL), function(x) ifelse(x <= 7, x, NA))%>%
  mutate_at(vars(lr_PvdA:lr_LPF), function(x) ifelse(x <= 11, x, NA))

# Stack the three sets of survey data on top of each other
dataStack <- full_join(originalPRE, originalPOST)%>%
  full_join(original2003) %>%
  # Rescale all variables to 0-1
  mutate_at(vars(asy_CDA:lr_LPF), function(x) scales::rescale(x, to=c(0,1))) %>%
  # Change to a long format
  gather(variable, perPos, c(asy_CDA:lr_LPF)) %>%
  # Divide "variable" into issue and party 
  separate(variable, c("issue", "party")) %>%
  # Handcode CMP codes for relevant parties
  mutate(year = year(date),
         cmp = case_when(
    party == "CDA" ~ 22521, 
    party == "PvdA" ~ 22320,
    party == "VVD"  ~ 22420,
    party == "D66"~ 22330,
    party == "GL" ~ 22110,
    party == "LPF" ~ 22720,
    party == "SP" ~ 22220,
    party == "SGP" ~ 22952,
    party == "CU" ~ 22526,
    party == "LN" ~ 22430))

# CMP categories derived from Wagner 2011
cmp <- read_dta("MARPOR2020.dta") %>%
  mutate(year = floor(date/100),
         lr = scales::rescale(rile, to = c(0,1)),
         eth = per601 + per608 - per602 - per607,# -per705 - per706,
         eth = scales::rescale(eth, to = c(0,1)),
         asy = eth,
         eu =  per110 - per108,
         eu = scales::rescale(eu, to = c(0,1))) %>%
  dplyr::select(year, cmp = party, lr, eth, asy, eu) %>% 
  gather(issue, cmpPos, lr:eu) 

dataStack <- dataStack %>%
  filter(issue %in% c("asy", "eth", "eu", "lr")) %>%
  left_join(cmp) %>%
  # Code dummy variables
  mutate(
    after = ifelse(date > lubridate::ymd("2002-07-22"),1,0),
    death = ifelse(date > lubridate::ymd("2002-05-06"),1,0),
    center_right = ifelse(party == "VVD" | party == "CDA", 1,0),
    radical_right = ifelse(party == "LPF",1,0),
    party_fam = case_when(
      center_right == 1 ~ "center_right",
      radical_right == 1 ~ "radical_right",
      center_right == 0 & radical_right== 0   ~ "all_others"
    ),
    party_fam = as.factor(party_fam))

descriptives <- dataStack %>%
  group_by(after, issue, party) %>%
  summarise(meanPos = mean(perPos, na.rm=TRUE),
            sdPos = sd(perPos, na.rm=TRUE)) %>%
  ungroup

descriptives %>%
  filter(issue=="eth") %>%
  xtable(digits=2)

## Combined simple
modelAsyAll<- dataStack %>% 
  filter(issue == "asy") %>%
  felm(perPos ~ cmpPos + after + party_fam + after*party_fam |ridn, data=.)

modelAsyAfter <- dataStack %>% 
  filter(issue == "asy" & death == 1) %>%
  felm(perPos ~ cmpPos + after + party_fam + after*party_fam |ridn, data=.)

modelEth <- dataStack %>% 
  filter(issue == "eth") %>%
  felm(perPos ~ cmpPos + after + party_fam + after*party_fam |ridn, data=.)

modelEU <- dataStack %>% 
  filter(issue == "eu") %>%
  felm(perPos ~ cmpPos + after + party_fam + after*party_fam |ridn, data=.)

modelLR <- dataStack %>% 
  filter(issue == "lr" ) %>%
  felm(perPos ~ cmpPos  + after + party_fam + after*party_fam |ridn, data=.)

stargazer(modelLR, modelAsyAll,  modelEth)

stargazer(modelAsyAfter, modelEU)
