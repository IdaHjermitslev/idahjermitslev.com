##### Coding #####

# Clear environment
rm(list=ls())

# Load packages
library(foreign)
library(tidyverse)
library(scales)
library(survey)

# Load original data set
original <- read.spss("Data/soccap2006v1.por",
                      use.value.labels=FALSE) %>%
  as.data.frame(stringsAsFactors = F)

# income recoding
original$INCOME <- NA
original$INCOME[original$YP_2==1] <- 0
original$INCOME[original$YP_2==2] <- 1
original$INCOME[original$YP_3==1] <- 2
original$INCOME[original$YP_3==2] <- 3
original$INCOME[original$YP_3==3] <- 4
original$INCOME[original$YP_3==4] <- 5

# Select the relevant variables 
sccs <- original %>%
  dplyr::select(USEDUP, TOOMUCH, OVERWHLM, CALMLFE,
                POLINT, REGVOTE, VOTEUS, PETITION, RALLY, 
                MARCH, GRPPOL, GRPLAB, POLKNOW,
                TVONE, WRKTIME, COMMUTE, TVHRS,
                GENDER, RELATEN2, 
                RELIMP, LABOR, MARITAL, KIDS, AGE, EDUC, HISPAN,
                RACE, INCOME, OWN, HEALTH, BORNUS,
                FWEIGHT)


# Recode missing values
sccs$HISPAN[which(sccs$HISPAN >= 8)] <- NA
sccs$RACE[which(sccs$RACE >= 8)] <- NA
sccs$EDUC[which(sccs$EDUC >= 8)] <- NA
sccs$LABOR[which(sccs$LABOR >= 8)] <- NA
sccs$MARITAL[which(sccs$MARITAL >= 8)] <- NA
sccs$KIDS[which(sccs$KIDS >= 98)] <- NA
sccs$RELIMP[which(sccs$RELIMP >= 8)] <- NA
sccs$GRPLAB[which(sccs$GRPLAB >= 8)] <- NA
sccs$OWN[which(sccs$OWN >= 8)] <- NA
sccs$HEALTH[which(sccs$HEALTH >= 8)] <- NA
sccs$BORNUS[which(sccs$BORNUS >= 8)] <- NA
sccs$USEDUP[which(sccs$USEDUP >= 8)] <- NA
sccs$TOOMUCH[which(sccs$TOOMUCH >= 8)] <- NA
sccs$CALMLFE[which(sccs$CALMLFE >= 8)] <- NA
sccs$OVERWHLM[which(sccs$OVERWHLM >= 8)] <- NA
sccs$POLINT[which(sccs$POLINT >= 8)] <- NA
sccs$REGVOTE[which(sccs$REGVOTE >= 3)] <- NA
sccs$VOTEUS[which(sccs$VOTEUS >= 3)] <- NA
sccs$PETITION[which(sccs$PETITION >= 8)] <- NA
sccs$RALLY[which(sccs$RALLY >= 8)] <- NA
sccs$MARCH[which(sccs$MARCH >= 8)] <- NA
sccs$GRPPOL[which(sccs$GRPPOL >= 8)] <- NA
sccs$POLKNOW[which(sccs$POLKNOW >= 8)] <- NA
sccs$TVONE[which(sccs$TVONE >= 8)] <- NA
sccs$WRKTIME[which(sccs$WRKTIME >= 98)] <- NA
sccs$COMMUTE[which(sccs$COMMUTE >= 998)] <- NA
sccs$TVHRS[which(sccs$TVHRS >= 98)] <- NA

# If respondent was hispanic, replace "missing" race category with "other"
sccs$RACE[which(sccs$HISPAN == 1)] <- 6

# If respondent was not currently employed, replace "missing" worktime with zero
sccs$WRKTIME <- ifelse(sccs$LABOR !=1 & is.na(sccs$WRKTIME), 0, sccs$WRKTIME)

# If respondent was not currently employed, replace "missing" commuting time with zero
sccs$COMMUTE <- ifelse(sccs$LABOR !=1 & is.na(sccs$COMMUTE), 0, sccs$COMMUTE)

# Recode worktime
sccs$WRKTIME[which(sccs$WRKTIME > 0 & sccs$WRKTIME  <= 20)] <- 1
sccs$WRKTIME[which(sccs$WRKTIME > 20 & sccs$WRKTIME  <= 40)] <- 2
sccs$WRKTIME[which(sccs$WRKTIME > 40 & sccs$WRKTIME  <= 60)] <- 3
sccs$WRKTIME[which(sccs$WRKTIME > 60)] <- 4

# Recode commute time
sccs$COMMUTE[which(sccs$COMMUTE <= 0.5)] <- 0
sccs$COMMUTE[which(sccs$COMMUTE > 0.5 & sccs$COMMUTE  <= 1)] <- 1
sccs$COMMUTE[which(sccs$COMMUTE > 1 & sccs$COMMUTE  <= 2)] <- 2
sccs$COMMUTE[which(sccs$COMMUTE > 2)] <- 3

sccs <- sccs %>%  
  # Dummy for gender
  mutate(GENDER = GENDER - 1) %>%
  # Dummies for white and black race
  mutate(WHITE = if_else(RACE == 1, 1, 0),  
         BLACK = if_else(RACE == 2, 1, 0 )) %>%
  # Dummy for married
  mutate(MARITAL = if_else(MARITAL == 4, 1, 0)) %>%
  # Dummy for kids
  mutate(KIDS = if_else(KIDS > 0, 1, 0)) %>%
  # Dummy for employment
  mutate(LABOR = if_else(LABOR == 1, 1, 0)) %>%
  # Dummy for immigration
  mutate(BORNUS = if_else(BORNUS == 1, 1, 0)) %>%
  # Reverse code calm life variable
  mutate(CALMLFE = abs(CALMLFE-6)) %>%
  # Religious summary measure rescaled to 0-1 (subtract one from relimp to scale 0-1)
  mutate(RELIGION = (RELATEN2 + (RELIMP - 1))/8) %>%
  # Count political activties
  mutate(BEHAVE = rowMeans(cbind(PETITION, MARCH, RALLY, GRPPOL), na.rm=TRUE)) %>% 
  # Make a dependent variable
  mutate(DEPLET = rowMeans(cbind(USEDUP, TOOMUCH, OVERWHLM, CALMLFE), na.rm=TRUE)) %>%
  mutate(TV = (rescale(TVONE) + rescale(TVHRS)) / 2 ) %>%
  mutate(POLINT = rescale(POLINT),
            POLKNOW = rescale(POLKNOW),
            AGE = rescale(AGE),
            EDUC = rescale(EDUC),
            HEALTH = rescale(HEALTH),
            WRKTIME = rescale(WRKTIME),
            COMMUTE = rescale(COMMUTE),
            USEDUP = rescale(USEDUP),
            DEPLET = rescale(DEPLET)) 

##### Analyses #####

# impute income
sccs$INCOME_i <- ifelse(is.na(sccs$INCOME),
                        predict(lm(sccs$INCOME ~ 
                                     AGE + GENDER + BLACK + HISPAN + EDUC
                                   + LABOR + MARITAL + KIDS + 
                                     RELIGION + OWN + HEALTH + WRKTIME, 
                                   data=sccs)),
                        sccs$INCOME)

# Construct a survey design to use survey weights
design <- svydesign(id = ~0, weights = ~FWEIGHT, data = sccs)

# voter turnout
m_vote_1 <- svyglm(VOTEUS ~ USEDUP + AGE + GENDER + BLACK + HISPAN + EDUC
                + INCOME_i + LABOR + MARITAL + KIDS + RELIGION + OWN + HEALTH + WRKTIME
                + COMMUTE + BORNUS, design=design, family=binomial(link="logit"))
write.csv(summary(m_vote_1)$coefficients, "Tables/SCCS_vote_1.csv", row.names = T)

m_vote_2 <- svyglm(VOTEUS ~ DEPLET + AGE + GENDER + BLACK + HISPAN + EDUC
                + INCOME_i + LABOR + MARITAL + KIDS + RELIGION + OWN + HEALTH + WRKTIME
                + COMMUTE + BORNUS, design=design, family=binomial(link="logit"))
write.csv(summary(m_vote_2)$coefficients, "Tables/SCCS_vote_2.csv", row.names = T)

m_vote_3 <- svyglm(VOTEUS ~ USEDUP + AGE + GENDER + BLACK + HISPAN + EDUC
                + INCOME_i + LABOR + GRPLAB + MARITAL + KIDS + RELIGION + OWN + HEALTH + WRKTIME
                + COMMUTE + BORNUS, design=design, family=binomial(link="logit"))
write.csv(summary(m_vote_3)$coefficients, "Tables/SCCS_vote_3.csv", row.names = T)

# interest in politics
m_int_1 <- svyglm(POLINT ~ USEDUP + AGE + GENDER + BLACK + HISPAN + EDUC
                + INCOME_i + LABOR + MARITAL + KIDS + RELIGION + OWN + HEALTH + WRKTIME
                + COMMUTE + BORNUS, design=design)
write.csv(summary(m_int_1)$coefficients, "Tables/SCCS_int_1.csv", row.names = T)

m_int_2 <- svyglm(POLINT ~ DEPLET + AGE + GENDER + BLACK + HISPAN + EDUC
              + INCOME_i + LABOR + MARITAL + KIDS + RELIGION + OWN + HEALTH + WRKTIME
              + COMMUTE + BORNUS, design=design)
write.csv(summary(m_int_2)$coefficients, "Tables/SCCS_int_2.csv", row.names = T)

m_int_3 <- svyglm(POLINT ~ USEDUP + AGE + GENDER + BLACK + HISPAN + EDUC
              + INCOME_i + LABOR + GRPLAB + MARITAL + KIDS + RELIGION + OWN + HEALTH + WRKTIME
              + COMMUTE + BORNUS, design=design)
write.csv(summary(m_int_3)$coefficients, "Tables/SCCS_int_3.csv", row.names = T)

# political activities
m_act_1 <- svyglm(BEHAVE ~ USEDUP + AGE + GENDER + BLACK + HISPAN + EDUC
              + INCOME_i + LABOR + MARITAL + KIDS + RELIGION + OWN + HEALTH + WRKTIME
              + COMMUTE + BORNUS, design=design)
write.csv(summary(m_act_1)$coefficients, "Tables/SCCS_act_1.csv", row.names = T)

m_act_2 <- svyglm(BEHAVE ~ DEPLET + AGE + GENDER + BLACK + HISPAN + EDUC
              + INCOME_i + LABOR + MARITAL + KIDS + RELIGION + OWN + HEALTH + WRKTIME
              + COMMUTE + BORNUS, design=design)
write.csv(summary(m_act_2)$coefficients, "Tables/SCCS_act_2.csv", row.names = T)

m_act_3 <- svyglm(BEHAVE ~ USEDUP + AGE + GENDER + BLACK + HISPAN + EDUC
              + INCOME_i + LABOR + GRPLAB + MARITAL + KIDS + RELIGION + OWN + HEALTH + WRKTIME
              + COMMUTE + BORNUS, design=design)
write.csv(summary(m_act_3)$coefficients, "Tables/SCCS_act_3.csv", row.names = T)

