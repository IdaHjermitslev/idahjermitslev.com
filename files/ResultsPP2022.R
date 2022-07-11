# Clear working environment
rm(list = ls())

# Read packages
library(tidyverse)
library(haven)
library(ggplot2)
library(miceadds)
library(xtable)
library(scales)
library(ggrepel)

# Load data
load("MinGovData")


### Descriptive analysis
control_subset <- MinGovData %>% 
  # Rescale political sophistication variables 
  mutate(interest = rescale(interest, to=c(0,1)),
         TV = rescale(TV, to=c(0,1)),
         paper = rescale(paper, to=c(0,1)))%>%
  # Look only at control group  
  filter(treatment == "control") %>%
  # How many get it right?  
  # Parties in government
  mutate(gov_correct = ifelse(gov_SD == 1 & 
                                gov_RV+gov_SF+gov_EL +
                                gov_KF+gov_NB+gov_KRP+gov_LA+gov_KD+gov_DF+
                                gov_SK+gov_V+gov_AL+gov_DK == 0,1,0),
         # Parties in the support agreement
         agg_correct = ifelse(agg_SD+agg_RV+agg_SF+agg_EL == 4 & 
                                agg_KF+agg_NB+agg_KRP+agg_LA+agg_KD+agg_DF+
                                agg_SK+agg_V+agg_AL+agg_DK == 0 |
                                agg_RV+agg_SF+agg_EL == 3 & 
                                agg_KF+agg_NB+agg_KRP+agg_LA+agg_KD+agg_DF+
                                agg_SK+agg_V+agg_AL+agg_DK == 0,1,0),
         # How many get it wrong in a specific way?
         # Includes one support party in government
         gov_w1sup = ifelse(gov_SD+gov_RV == 2 |
                              gov_SD+gov_SF == 2|
                              gov_SD+gov_EL == 2,1,0),
         # Includes all support parties in government
         gov_wsupp = ifelse(gov_SD+gov_RV+gov_SF+gov_EL == 4,1,0),
         # Fail to mention one of the three support parties
         agg_wout1 = ifelse(agg_RV+agg_SF+agg_EL == 2 & 
                              agg_KF+agg_NB+agg_KRP+agg_LA+agg_KD+agg_DF+
                              agg_SK+agg_V+agg_AL+agg_DK == 0,1,0),
         # Can only mention one of the three support parties
         agg_wout2 = ifelse(agg_RV+agg_SF+agg_EL == 1 & 
                              agg_KF+agg_NB+agg_KRP+agg_LA+agg_KD+agg_DF+
                              agg_SK+agg_V+agg_AL+agg_DK == 0,1,0),
         # Includes the Alternative
         agg_wAL = ifelse(agg_SD+agg_RV+agg_SF+agg_EL + agg_AL == 5 & 
                            agg_KF+agg_NB+agg_KRP+agg_LA+agg_KD+agg_DF+
                            agg_SK+agg_V+agg_DK == 0 |
                            agg_RV+agg_SF+agg_EL + agg_AL== 4 & 
                            agg_KF+agg_NB+agg_KRP+agg_LA+agg_KD+agg_DF+
                            agg_SK+agg_V+agg_DK == 0,1,0),
         # Makes no attempt at all
         agg_noattempt = ifelse(agg_DK == 1 &
                                  agg_SD + agg_RV+agg_SF+agg_EL +
                                  agg_KF+agg_NB+agg_KRP+agg_LA+agg_KD+agg_DF+
                                  agg_SK+agg_V+agg_AL == 0,1,0))

sum(control_subset$gov_correct)

sum(control_subset$gov_correct*control_subset$weight)

# Table 1 
control_subset %>%
  group_by(gov_correct) %>%
  summarise(know1 = mean(knowledge1, na.rm=T),
            know2 = mean(knowledge2, na.rm=T),
            know3 = mean(knowledge3, na.rm=T)) %>%
  ungroup()%>%
  t() %>%
  xtable()

control_subset %>%
  group_by(gov_correct) %>%
  summarise(interest_mean = mean(interest, na.rm=T),
            TV_mean = mean(TV, na.rm=T),
            paper_mean = mean(paper, na.rm=T)) %>%
  ungroup()%>%
  t() %>%
  xtable()

sum(control_subset$gov_w1sup)

sum(control_subset$gov_wsupp)

sum(control_subset$agg_correct)

sum(control_subset$agg_noattempt)

## Party placements

mean_placements <- control_subset %>%
  summarise(
    pos_general_SD = mean(pos_general_SD, na.rm=TRUE),
    pos_general_RV = mean(pos_general_RV, na.rm=TRUE),
    pos_general_KF = mean(pos_general_KF, na.rm=TRUE),
    pos_general_NB = mean(pos_general_NB, na.rm=TRUE),
    pos_general_SF = mean(pos_general_SF, na.rm=TRUE),
    pos_general_LA = mean(pos_general_LA, na.rm=TRUE),
    pos_general_DF = mean(pos_general_DF, na.rm=TRUE),
    pos_general_V = mean(pos_general_V, na.rm=TRUE),
    pos_general_EL = mean(pos_general_EL, na.rm=TRUE),
    pos_general_AL = mean(pos_general_AL, na.rm=TRUE),
    pos_immigration_SD = mean(pos_immigration_SD, na.rm=TRUE),
    pos_immigration_RV = mean(pos_immigration_RV, na.rm=TRUE),
    pos_immigration_KF = mean(pos_immigration_KF, na.rm=TRUE),
    pos_immigration_NB = mean(pos_immigration_NB, na.rm=TRUE),
    pos_immigration_SF = mean(pos_immigration_SF, na.rm=TRUE),
    pos_immigration_LA = mean(pos_immigration_LA, na.rm=TRUE),
    pos_immigration_DF = mean(pos_immigration_DF, na.rm=TRUE),
    pos_immigration_V = mean(pos_immigration_V, na.rm=TRUE),
    pos_immigration_EL = mean(pos_immigration_EL, na.rm=TRUE),
    pos_immigration_AL = mean(pos_immigration_AL, na.rm=TRUE),
    pos_economy_SD = mean(pos_economy_SD, na.rm=TRUE),
    pos_economy_RV = mean(pos_economy_RV, na.rm=TRUE),
    pos_economy_KF = mean(pos_economy_KF, na.rm=TRUE),
    pos_economy_NB = mean(pos_economy_NB, na.rm=TRUE),
    pos_economy_SF = mean(pos_economy_SF, na.rm=TRUE),
    pos_economy_LA = mean(pos_economy_LA, na.rm=TRUE),
    pos_economy_DF = mean(pos_economy_DF, na.rm=TRUE),
    pos_economy_V = mean(pos_economy_V, na.rm=TRUE),
    pos_economy_EL = mean(pos_economy_EL, na.rm=TRUE),
    pos_economy_AL = mean(pos_economy_AL, na.rm=TRUE)
  ) %>%
  pivot_longer(cols = pos_general_SD:pos_economy_AL,
               names_to = c("dimension", "party"),
               names_prefix = "pos_",
               names_sep = "_",
               values_to = "position") %>%
  pivot_wider(names_from =  "dimension",
              values_from = "position") %>%
  mutate(signatory= ifelse(party %in% c("SD", "RV", "SF", "EL"),1,0))

ggplot(data= mean_placements, aes(x=general, y=0, label=party, 
                                  colour =as.factor(signatory),
                                  shape =as.factor(signatory)))+
  geom_point()  +
  scale_colour_grey(start=0.4, end=0) +
  theme_classic( ) +
  theme(
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position="none") +
  xlim(c(0,10)) +
  ylab("") +
  xlab("General left-right dimension") +
  geom_text_repel()


ggplot(data= mean_placements, aes(x=economy, y=immigration, label=party, 
                                  colour =as.factor(signatory),
                                  shape =as.factor(signatory)))+
  geom_point() +
  scale_colour_grey(start=0.4, end=0) +
  theme_classic( ) +
  theme(
    legend.position="none") +
  xlim(c(0,10)) +
  ylim(c(0,10)) +
  ylab("Refugees and asylum") +
  xlab("Public revenue and expenses") +
  geom_text_repel()

### Manipulation check: Perceived support

MinGovData %>%
  group_by(treatment) %>%
  summarise(mean = mean(manipulation, na.rm=TRUE),
            sd = sd(manipulation, na.rm=TRUE),
            DK = mean(is.na(manipulation))) %>%
  ungroup %>%
  xtable

sqrt(0.23/ 723 + 0.23 / 714)

test_manipulation <- MinGovData %>%
  filter(treatment != "control") %>%
  t.test(manipulation ~ treatment, data=., var.equal=TRUE, conf.level=0.95)


test_manipulation

test_manipulation$estimate[1]-test_manipulation$estimate[2]

categorical_manipulation <- table(MinGovData$treatment, MinGovData$manipulation)
  
prop.table(categorical_manipulation, margin = 1) %>%
  xtable()

table(MinGovData$manipulation)

summary(categorical_manipulation)

### Experimental results: Perceived responsibility

MinGovData %>%
  group_by(treatment) %>%
  summarise(resp_SD = mean(resp_general_SD, na.rm=TRUE),
            resp_SD_sd = sd(resp_general_SD, na.rm=TRUE),
            resp_RV = mean(resp_general_RV, na.rm=TRUE),
            resp_RV_sd = sd(resp_general_RV, na.rm=TRUE),
            resp_SF = mean(resp_general_SF, na.rm=TRUE),
            resp_SF_sd = sd(resp_general_SF, na.rm=TRUE),
            resp_EL = mean(resp_general_EL, na.rm=TRUE),
            resp_EL_sd = sd(resp_general_EL, na.rm=TRUE)) %>%
  ungroup %>%
  xtable()

test_SD <- MinGovData %>%
  filter(treatment != "control") %>%
  t.test(resp_general_SD ~ treatment, data=., var.equal=TRUE, conf.level=0.95)

test_SD

test_SD$estimate[1]-test_SD$estimate[2]

sqrt(0.191/ 723 + 0.195 / 714)

test_RV <- MinGovData %>%
  filter(treatment != "control") %>%
  t.test(resp_general_RV ~ treatment, data=., var.equal=TRUE, conf.level=0.95)

test_RV

test_RV$estimate[1]-test_RV$estimate[2]

sqrt(0.257/ 723 + 0.254 / 714)

test_SF <- MinGovData %>%
  filter(treatment != "control") %>%
  t.test(resp_general_SF ~ treatment, data=., var.equal=TRUE, conf.level=0.95)

test_SF

test_SF$estimate[1]-test_SF$estimate[2]

sqrt(0.254/ 723 + 0.250 / 714)

test_EL <- MinGovData %>%
  filter(treatment != "control") %>%
  t.test(resp_general_EL ~ treatment, data=., var.equal=TRUE, conf.level=0.95)

test_EL

test_EL$estimate[1]-test_EL$estimate[2]

sqrt(0.287/ 723 + 0.273 / 714)

graphData <- tibble(
  party = c("Soc. Dems", "Rad. Libs", "Soc. People's", "Unity List"),
  Difference = c(test_SD$estimate[1]-test_SD$estimate[2],
                 test_RV$estimate[1]-test_RV$estimate[2],
                 test_SF$estimate[1]-test_SF$estimate[2],
                 test_EL$estimate[1]-test_EL$estimate[2]),
  lower = c(test_SD$conf.int[1],
            test_RV$conf.int[1],
            test_SF$conf.int[1],
            test_EL$conf.int[1]),
  upper = c(test_SD$conf.int[2],
            test_RV$conf.int[2],
            test_SF$conf.int[2],
            test_EL$conf.int[2]))

ggplot(data=graphData, mapping = aes(x=Difference, y=party)) +
  scale_y_discrete(name ="", 
                   limits=c("Unity List", "Soc. People's", "Rad. Libs", "Soc. Dems")) +
  theme_bw()+
  geom_pointrange(aes(xmin = lower, xmax = upper)) +
  geom_vline(xintercept = 0,
             linetype="dashed")

# Categorical analysis
categorical_resp_general_SD <- table(MinGovData$treatment, MinGovData$resp_general_SD)

prop.table(categorical_resp_general_SD, margin = 1) %>%
  xtable() 

table(MinGovData$resp_general_SD)

summary(categorical_resp_general_SD)

categorical_resp_general_RV <- table(MinGovData$treatment, MinGovData$resp_general_RV)

prop.table(categorical_resp_general_RV, margin = 1) %>%
  xtable() 

table(MinGovData$resp_general_RV)

summary(categorical_resp_general_RV)

categorical_resp_general_SF <- table(MinGovData$treatment, MinGovData$resp_general_SF)

prop.table(categorical_resp_general_SF, margin = 1) %>%
  xtable(digits = 3) 

table(MinGovData$resp_general_SF)

summary(categorical_resp_general_SF)

categorical_resp_general_EL <- table(MinGovData$treatment, MinGovData$resp_general_EL)

prop.table(categorical_resp_general_EL, margin = 1) %>%
  xtable(digits = 3) 

table(MinGovData$resp_general_EL)

summary(categorical_resp_general_EL)

# Wishful thinking
control_subset %>%
  group_by(party) %>%
  summarise(resp_SD = mean(resp_general_SD, na.rm=TRUE),
            resp_SD_sd = sd(resp_general_SD, na.rm=TRUE),
            resp_RV = mean(resp_general_RV, na.rm=TRUE),
            resp_RV_sd = sd(resp_general_RV, na.rm=TRUE),
            resp_SF = mean(resp_general_SF, na.rm=TRUE),
            resp_SF_sd = sd(resp_general_SF, na.rm=TRUE),
            resp_EL = mean(resp_general_EL, na.rm=TRUE),
            resp_EL_sd = sd(resp_general_EL, na.rm=TRUE)) %>%
  ungroup %>%
  xtable()

table(control_subset$party)

# Issue specific responsibility

MinGovData %>%
  group_by(treatment) %>%
  summarise(resp_SD = mean(resp_immigration_SD, na.rm=TRUE),
            resp_SD_sd = sd(resp_immigration_SD, na.rm=TRUE),
            resp_RV = mean(resp_immigration_RV, na.rm=TRUE),
            resp_RV_sd = sd(resp_immigration_RV, na.rm=TRUE),
            resp_SF = mean(resp_immigration_SF, na.rm=TRUE),
            resp_SF_sd = sd(resp_immigration_SF, na.rm=TRUE),
            resp_EL = mean(resp_immigration_EL, na.rm=TRUE),
            resp_EL_sd = sd(resp_immigration_EL, na.rm=TRUE)) %>%
  ungroup %>%
  xtable()

MinGovData %>%
  filter(treatment != "control") %>%
  t.test(resp_immigration_SD ~ treatment, data=., var.equal=TRUE, conf.level=0.95)

sqrt(0.193/ 723 + 0.211 / 714)

MinGovData %>%
  filter(treatment != "control") %>%
  t.test(resp_immigration_RV ~ treatment, data=., var.equal=TRUE, conf.level=0.95)

sqrt(0.272/ 723 + 0.276 / 714)

MinGovData %>%
  filter(treatment != "control") %>%
  t.test(resp_immigration_SF ~ treatment, data=., var.equal=TRUE, conf.level=0.95)

sqrt(0.267/ 723 + 0.266 / 714)

MinGovData %>%
  filter(treatment != "control") %>%
  t.test(resp_immigration_EL ~ treatment, data=., var.equal=TRUE, conf.level=0.95)

sqrt(0.304/ 723 + 0.299 / 714)

MinGovData %>%
  group_by(treatment) %>%
  summarise(resp_SD = mean(resp_economy_SD, na.rm=TRUE),
            resp_SD_sd = sd(resp_economy_SD, na.rm=TRUE),
            resp_RV = mean(resp_economy_RV, na.rm=TRUE),
            resp_RV_sd = sd(resp_economy_RV, na.rm=TRUE),
            resp_SF = mean(resp_economy_SF, na.rm=TRUE),
            resp_SF_sd = sd(resp_economy_SF, na.rm=TRUE),
            resp_EL = mean(resp_economy_EL, na.rm=TRUE),
            resp_EL_sd = sd(resp_economy_EL, na.rm=TRUE)) %>%
  ungroup %>%
  xtable()

MinGovData %>%
  filter(treatment != "control") %>%
  t.test(resp_economy_SD ~ treatment, data=., var.equal=TRUE, conf.level=0.95)

sqrt(0.259/ 723 + 0.257 / 714)

MinGovData %>%
  filter(treatment != "control") %>%
  t.test(resp_economy_RV ~ treatment, data=., var.equal=TRUE, conf.level=0.95)

sqrt(0.275/ 723 + 0.268 / 714)

MinGovData %>%
  filter(treatment != "control") %>%
  t.test(resp_economy_SF ~ treatment, data=., var.equal=TRUE, conf.level=0.95)

sqrt(0.306/ 723 + 0.299 / 714)

MinGovData %>%
  filter(treatment != "control") %>%
  t.test(resp_economy_EL ~ treatment, data=., var.equal=TRUE, conf.level=0.95)

sqrt(0.304/ 723 + 0.299 / 714)

