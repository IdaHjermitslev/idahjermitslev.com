# Clear working environment
rm(list = ls())

# Read packages
library(tidyverse)
library(haven)
library(scales)
library(nnet)
library(stargazer)

# Load data
MinGovData <- read_dta("MinGovData.dta")%>%
  # Remove Stata labels as there is no direct equivalent in R
  zap_labels() %>%
  rename(interest=q1, TV=q2, paper=q3) %>%
  mutate(treatment = case_when(
             splitsample == 1 ~ "high",
             splitsample == 2 ~ "low",
             splitsample == 3 ~ "control" 
           ),
         interest = ifelse(interest==5, NA, interest),
         TV = ifelse(TV==9, NA, TV-1),
         paper = ifelse(paper==9, NA, paper-1),
         female = ifelse(gender == 2, 0, gender),
         age = profile_age2,
         copenhagen = ifelse(urban == 1, 1, 0),
         income = ifelse(household_income > 11, NA, household_income),
         married = ifelse(profile_marital == 2 | profile_marital == 5, 1, 0),
         employed = ifelse(occupation %in% 4:7, 1, 0)
  )

test <- multinom(treatment ~ interest + TV + paper + female + age +
                   copenhagen + employed + income + married , data = MinGovData)
summary(test)

stargazer(test)