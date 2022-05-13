# Load packages
library(tidyverse)

cleanDK2018 <- function(original){

expData <- original %>%
  # Parties have weird labels (due to programming of survey). Change that
  mutate(LRparty_8 = LRparty_25,
         LRparty_9 = LRparty_26) %>%
  # Collect party placement in same variable (regardless of treatment)
  mutate(LR_V = ifelse(exp1=="control", LRCtrl_1, LRparty_1),
         LR_LA = ifelse(exp1=="control", LRCtrl_2, LRparty_2),
         LR_RV = ifelse(exp1=="control", LRCtrl_3, LRparty_3),
         LR_DF = ifelse(exp1=="control", LRCtrl_4, LRparty_4),
         LR_AL = ifelse(exp1=="control", LRCtrl_5, LRparty_5),
         LR_SD = ifelse(exp1=="control", LRCtrl_6, LRparty_6),
         LR_EL = ifelse(exp1=="control", LRCtrl_7, LRparty_7),
         LR_K = ifelse(exp1=="control", LRCtrl_8, LRparty_8), 
         LR_SF=ifelse(exp1=="control", LRCtrl_9, LRparty_9),
         RF_V = ifelse(exp1=="control", RFCtrl_1, RFparty_1),
         RF_LA = ifelse(exp1=="control", RFCtrl_2, RFparty_2),
         RF_RV = ifelse(exp1=="control", RFCtrl_3, RFparty_3),
         RF_DF = ifelse(exp1=="control", RFCtrl_4, RFparty_4),
         RF_AL = ifelse(exp1=="control", RFCtrl_5, RFparty_5),
         PS_V = ifelse(exp1=="control", PSCtrl_1, PSparty_1),
         PS_LA = ifelse(exp1=="control", PSCtrl_2, PSparty_2),
         PS_RV = ifelse(exp1=="control", PSCtrl_3, PSparty_3),
         PS_DF = ifelse(exp1=="control", PSCtrl_4, PSparty_4),
         PS_AL = ifelse(exp1=="control", PSCtrl_5, PSparty_5),
         GR_V = ifelse(exp1=="control", GRCtrl_1, GRparty_1),
         GR_LA = ifelse(exp1=="control", GRCtrl_2, GRparty_2),
         GR_RV = ifelse(exp1=="control", GRCtrl_3, GRparty_3),
         GR_DF = ifelse(exp1=="control", GRCtrl_4, GRparty_4),
         GR_AL = ifelse(exp1=="control", GRCtrl_5, GRparty_5),
         LO_V = ifelse(exp1=="control", LOCtrl_1, LOparty_1),
         LO_LA = ifelse(exp1=="control", LOCtrl_2, LOparty_2),
         LO_RV = ifelse(exp1=="control", LOCtrl_3, LOparty_3),
         LO_DF = ifelse(exp1=="control", LOCtrl_4, LOparty_4),
         LO_AL = ifelse(exp1=="control", LOCtrl_5, LOparty_5)) %>%
  # Recode to 0-10
  mutate_at(vars(LR_V:LO_AL), function(x) ifelse(x < 12, x-1, NA)) %>%
  # If respondent skipped question, this should be counted as not answering any of the options
  mutate_at(vars(manipulation_1:manipulation_7), function(x) ifelse(is.na(x), 0, x)) %>%
  # Correct answer is "Venstre" plus answer option 2. All others should be left out. 
  mutate(manipulation_correct = ifelse(manipulation_1+manipulation_2 == 2 &
                                         manipulation_3 + manipulation_4 + manipulation_5 +
                                         manipulation_6 + manipulation_7 == 0, 1,0)) %>%
  mutate(leftPos = case_when(
    direction == 1 & exp2 == "control" ~ leftCtrl,
    direction == 1 & exp2 != "control" ~ leftTreat),
    rightPos = case_when(
      direction == 2 & exp2 == "control" ~ rightCtrl,
      direction == 2 & exp2 != "control" ~ rightTreat)) %>%
  mutate(leftPos = ifelse(leftPos == 12, NA, leftPos-1),
         rightPos = ifelse(rightPos == 12, NA, rightPos-1)) %>%
  mutate(attention_correct = ifelse(attention2 == 2, 1, 0)) %>%
  mutate(exp1 = factor(exp1, levels=c("control", "allerede har dannet", "umuligt kan danne"))) %>%
  # Socio-demographics
  mutate(gender = gender - 1,
         age = ifelse(age == -99, NA, age),
         education = case_when(
           edu1 == 1 ~ "Primary school (<9 years)",
           edu1 == 9 ~ "Primary school (9-10 years)",
           edu1 == 6 ~ "Secondary/highschool",
           edu1 == 10 ~ "Vocational training",
           edu1 == 11 ~ "College (<3 years)",
           edu1 == 3 ~ "Bachelors degree (3-4 years)",
           edu1 == 4 ~ "Masters degree (5+ years)",
           edu1 %in% c(8, 16, 5) ~ "Other"
         ),
         vote = case_when(
           vote == 1 ~ "Liberals",
           vote == 2 ~ "Liberal Alliance",
           vote == 3 ~ "Radikale Venstre",
           vote == 4 ~ "Dansk Folkeparti",
           vote == 5 ~ "The Alternative",
           vote == 6 ~ "Social Democrats",
           vote == 7 ~ "Unity List",
           vote == 8 ~ "Conservatives",
           vote == 9 ~ "Socialist People's Party",
           vote %in% 10:11 ~ "Abstained",
           vote == 12 ~ "Don't recall"
         )) %>%
  # Select only the variables used
  dplyr::select(exp1, exp2, party, direction, manipulation_correct, attention_correct,
         LR_V:LO_AL,
         leftPos, rightPos,
         gender, age, education, vote)
expData
}

