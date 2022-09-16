# Clear working environment
rm(list = ls())

# Read packages
library(tidyverse)
library(haven)
library(scales)

# Load data
MinGovData <- read_dta("MinGovData.dta")%>%
  # Remove Stata labels as there is no direct equivalent in R
  zap_labels() %>%
  rename(interest=q1, TV=q2, paper=q3, party=q4,
         pos_general_SD=q11_1, pos_general_RV = q11_2, pos_general_KF = q11_3, pos_general_NB = q11_4, pos_general_SF = q11_5, 
         pos_general_LA = q11_6, pos_general_DF = q11_7, pos_general_V = q11_8, pos_general_EL = q11_9, pos_general_AL = q11_10,
         pos_immigration_SD=q12_1, pos_immigration_RV = q12_2, pos_immigration_KF = q12_3, pos_immigration_NB = q12_4, pos_immigration_SF = q12_5, 
         pos_immigration_LA = q12_6, pos_immigration_DF = q12_7, pos_immigration_V = q12_8, pos_immigration_EL = q12_9, pos_immigration_AL = q12_10,
         pos_economy_SD=q13_1, pos_economy_RV = q13_2, pos_economy_KF = q13_3, pos_economy_NB = q13_4, pos_economy_SF = q13_5, 
         pos_economy_LA = q13_6, pos_economy_DF = q13_7, pos_economy_V = q13_8, pos_economy_EL = q13_9, pos_economy_AL = q13_10,
         resp_general_SD=q14_1, resp_general_RV = q14_2, resp_general_SF = q14_3, resp_general_EL = q14_4, 
         resp_immigration_SD=q15_1, resp_immigration_RV = q15_2, resp_immigration_SF = q15_3, resp_immigration_EL = q15_4, 
         resp_economy_SD=q16_1, resp_economy_RV = q16_2, resp_economy_SF = q16_3, resp_economy_EL = q16_4) %>%
  mutate(
    treatment = case_when(
      splitsample == 1 ~ "high",
      splitsample == 2 ~ "low",
      splitsample == 3 ~ "control" 
    ),
    interest = ifelse(interest==5, NA, interest),
    TV = ifelse(TV==9, NA, TV-1),
    paper = ifelse(paper==9, NA, paper-1),
    party = ifelse(party %in% 10:14, NA, party),
    attention1 = case_when(
      treatment == "high" & q8a == 1 ~  1,
      treatment == "low" & q8b == 2 ~ 1,
      treatment == "high" & q8a != 1 ~ 0,
      treatment == "low" & q8b != 2 ~ 0),
    attention2 = case_when(
      treatment == "high" & q9a == 1 ~  1,
      treatment == "low" & q9b == 2 ~ 1,
      treatment == "high" & q9a != 1 ~ 0,
      treatment == "low" & q9b != 2 ~ 0),
    manipulation = case_when(
      treatment == "high" & q10a < 5 ~ q10a,
      treatment == "low" & q10b < 5 ~ q10b,
      treatment == "control" & q22 < 5 ~ q22),
    manipulation = scales::rescale(manipulation, to=c(0,1)),
    negotiations = case_when(
      treatment == "high" ~ q8a,
      treatment == "low" ~ q8b
    ),
    agreement = case_when(
      treatment == "high" ~ q9a,
      treatment == "low" ~ q9b
    ),
    treatment_time = 
      page_P_q8a_text_timing + page_P_q8a_timing + page_P_q9a_text_timing + page_P_q9a_timing + page_P_q10a_text_timing +
      page_P_q8b_text_timing + page_P_q8b_timing + page_P_q9b_text_timing + page_P_q9b_timing + page_P_q10b_text_timing,
    knowledge1 = ifelse(q17==2, 1, 0),
    knowledge2 = ifelse(q18==1, 1, 0),
    knowledge3 = ifelse(q19==1, 1, 0)
  ) %>%
  mutate_at(vars(resp_general_SD:resp_economy_EL), function(x) ifelse(x == 6, NA, x)) %>%
  mutate_at(vars(resp_general_SD:resp_economy_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  # Dependent variables
  mutate_at(vars(pos_general_SD:pos_economy_EL), function(x) ifelse(x == 12, NA, x-1))%>%
  mutate(
    dist_SDRV = abs(pos_general_SD-pos_general_RV),
    dist_SDSF = abs(pos_general_SD-pos_general_SF),
    dist_SDEL = abs(pos_general_SD-pos_general_EL),
    dist_immigration_SDRV = abs(pos_immigration_SD-pos_immigration_RV),
    dist_immigration_SDSF = abs(pos_immigration_SD-pos_immigration_SF),
    dist_immigration_SDEL = abs(pos_immigration_SD-pos_immigration_EL),
    dist_economy_SDRV = abs(pos_economy_SD-pos_economy_RV),
    dist_economy_SDSF = abs(pos_economy_SD-pos_economy_SF),
    dist_economy_SDEL = abs(pos_economy_SD-pos_economy_EL))%>%
  # Control group  
  rename(gov_SD = q20_1, gov_RV = q20_2, gov_KF = q20_3, gov_NB = q20_4, gov_KRP = q20_5, gov_SF = q20_6, 
         gov_LA = q20_7, gov_KD= q20_8, gov_DF = q20_9, gov_SK = q20_10, gov_V = q20_11, gov_EL = q20_12,
         gov_AL = q20_13, gov_DK = q20_16,
         agg_SD = q21_1, agg_RV = q21_2, agg_KF = q21_3, agg_NB = q21_4,agg_KRP = q21_5, agg_SF = q21_6,
         agg_LA = q21_7, agg_KD = q21_8, agg_DF = q21_9, agg_SK = q21_10, agg_V = q21_11, agg_EL = q21_12, 
         agg_AL = q21_13, agg_DK = q21_16
  )%>%
  select(caseid:party, treatment, pos_general_SD:resp_economy_EL, gov_SD:agg_DK, attention1:dist_economy_SDEL, weight)

save(MinGovData, file="MinGovData")