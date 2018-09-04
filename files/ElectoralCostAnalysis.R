# Clear environment
rm(list=ls())

# Disable scientific notation
options(scipen=999)

# Load packages
library(stargazer)
library(tidyverse)
library(lubridate)
library(effects)
library(ggplot2)
library(interplot)

# Set your own working directory
setwd("/Users/idahjermitslev/Documents/Governing Cost/Governing Cost Repository")

# Load data
load(file="GovCostData.Rda") 

#Calculate mean change by country and type
desc.stats <- data %>% group_by(country_name_short) %>%
  summarise(OCI = mean(OCI),
            mean.SP = mean(change[which(single_party == 1)], na.rm=TRUE),
            mean.PM = mean(change[which(pm_coalition == 1)], na.rm=TRUE),
            mean.JM = mean(change[which(junior_member == 1)], na.rm=TRUE)) %>%
  arrange(OCI)


#Listwise deletion
data <- data %>%
  mutate(type = ifelse(junior_member == 1, "JuniorMember",
                       ifelse(pm_coalition == 1, "PrimeMinister",
                              ifelse(single_party == 1, "SingleParty", "Opposition"))),
         type = as.factor(type),
         type = relevel(type, ref = "Opposition"))%>%
  dplyr::select(change, type, junior_member, pm_coalition, single_party, party_id, country_name_short, growth, 
                minority, OCI, pre_seat_share, extremism, post_election_id, election_date)

nrow(data) # From 2612

data <- data %>%
  # Select only parties that passed the electoral threshold in the previous election
  na.omit()

nrow(data) # To 1932

unique(data$party_id) %>% length()

unique(data$election_date) %>% length()

data$year <- year(data$election_date)-1960

# Simple model
model1 <- lm(change ~ type
              + growth 
              + OCI 
              + minority 
              + extremism
              + pre_seat_share + year, data=data)

# Interactive model
model2 <- lm(change ~ type
             + growth 
             + OCI 
             + minority 
             + extremism
             + pre_seat_share
             + year
             + growth*type, data=data)

# Full interactive model
model3 <- lm(change ~ type
             + growth 
             + OCI 
             + minority 
             + extremism
             + pre_seat_share
             + year
             + growth*type
             + OCI*type
             + minority*type
             + extremism*type
             + pre_seat_share*type
             + year*type, data=data)

# Calculate marginal effects of three variables of interest
ae <- allEffects(model3, xlevels=list(extremism=c(0,1,2,3,4,5), 
                                      pre_seat_share = c(0,15,30,45,60), 
                                      growth=c(-7,-3,2,6,10)))

# Create tibble with example data for growth
growth_results <- tibble(
  pred = as.vector(ae$`type:growth`$fit),
  growth = c(rep(-7,4), rep(-3,4), rep(2,4), rep(6,4), rep(10,4)),
  Role = c(rep(c("Opposition","Junior Member","Prime Minister", "Single-party"),5)), 
  lower = as.vector(ae$`type:growth`$lower),
  upper = as.vector(ae$`type:growth`$upper)
)

# Set distance between dots
pd1 <- position_dodge(width=1.2)

# Plot marginal effects
plot1 <- ggplot(growth_results, aes(growth, pred ,shape=Role)) + 
  geom_point(aes(shape=Role),size=2, position=pd1) +
  theme_bw() +
  scale_y_continuous("Predicted change in votes",
                     limits = c(-5.4,15.3)) +
  scale_x_continuous("Percent annual growth in GDP", ,
                     breaks = c(-7,-3,2,6,10)) +
  geom_errorbar(aes(ymin=lower,ymax=upper), width=0.8, position = pd1) +
  geom_hline(aes(yintercept=0), linetype="dashed")

# Create tibble with example data for extremity
extremity_results <- tibble(
  pred = as.vector(ae$`type:extremism`$fit),
  growth = c(rep(0,4), rep(1,4), rep(2,4), rep(3,4), rep(4,4),rep(5,4)),
  Role = c(rep(c("Opposition","Junior Member","Prime Minister", "Single-party"),6)), 
  lower = as.vector(ae$`type:extremism`$lower),
  upper = as.vector(ae$`type:extremism`$upper)
)

# Set distance between dots
pd2 <- position_dodge(width=0.4)

# Plot marginal effects
plot2 <- ggplot(extremity_results, aes(growth, pred ,shape=Role)) + 
  geom_point(aes(shape=Role),size=2, position=pd2) +
  theme_bw() +
  scale_y_continuous("Predicted change in votes",
                     limits = c(-5,15)) +
  scale_x_continuous("Ideological extremity", ,
                     breaks = c(0,1,2,3,4,5)) +
  geom_errorbar(aes(ymin=lower,ymax=upper), width=0.3, position = pd2) +
  geom_hline(aes(yintercept=0), linetype="dashed")

# Create tibble with example data for size
size_results <- tibble(
  pred = as.vector(ae$`type:pre_seat_share`$fit),
  growth = c(rep(0,4), rep(15,4), rep(30,4), rep(45,4), rep(60,4)),
  Role = c(rep(c("Opposition","Junior Member","Prime Minister", "Single-party"),5)), 
  lower = as.vector(ae$`type:pre_seat_share`$lower),
  upper = as.vector(ae$`type:pre_seat_share`$upper)
)

# Set distance between dots
pd3 <- position_dodge(width=5)

# Plot marginal effects
plot3 <- ggplot(size_results, aes(growth, pred ,shape=Role)) + 
  geom_point(aes(shape=Role),size=2, position=pd3) +
  theme_bw() +
  scale_y_continuous("Predicted change in votes",
                     limits = c(-15,15.5)) +
  scale_x_continuous("Party seat share in percentages", ,
                     breaks = c(0,15,30,45,60)) +
  geom_errorbar(aes(ymin=lower,ymax=upper), width=3, position = pd3) +
  geom_hline(aes(yintercept=0), linetype="dashed")

# Handcode the wald test
#### MODEL 1 ####
varmat <- vcov(model1)[c("typeJuniorMember", "typePrimeMinister"),c("typeJuniorMember", "typePrimeMinister")]

varmat

se <- sqrt(0.08221930+0.17235835 -2*0.02079592)

wald.z <- (model1$coefficients["typeJuniorMember"]-model1$coefficients["typePrimeMinister"])/se

2*pnorm(-abs(wald.z)) # significant at 0.10

varmat <- vcov(model1)[c("typeJuniorMember", "typeSingleParty"),c("typeJuniorMember", "typeSingleParty")]

varmat

se <- sqrt(0.08221930+0.24192943 -2*0.01955627)

wald.z <- (model1$coefficients["typeJuniorMember"]-model1$coefficients["typeSingleParty"])/se

2*pnorm(-abs(wald.z)) # no-significant

varmat <- vcov(model1)[c("typePrimeMinister", "typeSingleParty"),c("typePrimeMinister", "typeSingleParty")]

varmat

se <- sqrt(0.17235835 + 0.24192943 -2*0.07559603)

wald.z <- (model1$coefficients["typePrimeMinister"]-model1$coefficients["typeSingleParty"])/se

2*pnorm(-abs(wald.z)) # Significant at 0.01 level

#### MODEL 3 ####
varmat <- vcov(model3)[c("typeJuniorMember:growth", "typePrimeMinister:growth"),c("typeJuniorMember:growth", "typePrimeMinister:growth")]

varmat

se <- sqrt(0.01213915 + 0.02124479 -2*0.00184320)

wald.z <- (model3$coefficients["typeJuniorMember:growth"]-model3$coefficients["typePrimeMinister:growth"])/se

2*pnorm(-abs(wald.z)) # Non-significant

varmat <- vcov(model3)[c("typeJuniorMember:growth", "typeSingleParty:growth"),c("typeJuniorMember:growth", "typeSingleParty:growth")]

varmat

se <- sqrt(0.01213915  + 0.02502507 -2* 0.00184320)

wald.z <- (model3$coefficients["typeJuniorMember:growth"]-model3$coefficients["typeSingleParty:growth"])/se

2*pnorm(-abs(wald.z)) # Significant at 0.05

varmat <- vcov(model3)[c("typePrimeMinister:growth", "typeSingleParty:growth"),c("typePrimeMinister:growth", "typeSingleParty:growth")]

varmat

se <- sqrt(0.02124479  +0.02502507 -2*0.00184320)

wald.z <- (model3$coefficients["typePrimeMinister:growth"]-model3$coefficients["typeSingleParty:growth"])/se

2*pnorm(-abs(wald.z)) # Significant at 0.05

#### MODEL 2 ####
varmat <- vcov(model2)[c("typeJuniorMember:growth", "typePrimeMinister:growth"),c("typeJuniorMember:growth", "typePrimeMinister:growth")]

varmat

se <- sqrt(0.010847489 +0.019850678 -2*0.001667439)

wald.z <- (model2$coefficients["typeJuniorMember:growth"]-model3$coefficients["typePrimeMinister:growth"])/se

2*pnorm(-abs(wald.z)) # nonsignificant

varmat <- vcov(model2)[c("typeJuniorMember:growth", "typeSingleParty:growth"),c("typeJuniorMember:growth", "typeSingleParty:growth")]

varmat

se <- sqrt(0.010847489+0.022419557 -2*0.001637291)

wald.z <- (model2$coefficients["typeJuniorMember:growth"]-model2$coefficients["typeSingleParty:growth"])/se

2*pnorm(-abs(wald.z)) # Non-significant

varmat <- vcov(model2)[c("typePrimeMinister:growth", "typeSingleParty:growth"),c("typePrimeMinister:growth", "typeSingleParty:growth")]

varmat

se <- sqrt(0.019850678+0.022419557 -2*0.001619499)

wald.z <- (model2$coefficients["typePrimeMinister:growth"]-model2$coefficients["typeSingleParty:growth"])/se

2*pnorm(-abs(wald.z)) # Significant at level 0.10

#### MODEL 3 ####
varmat <- vcov(model3)[c("typeJuniorMember:extremism", "typePrimeMinister:extremism"),c("typeJuniorMember:extremism", "typePrimeMinister:extremism")]

varmat

se <- sqrt(0.085353126  +0.249646700 -2*0.007760934) 

wald.z <- (model3$coefficients["typeJuniorMember:extremism"]-model3$coefficients["typePrimeMinister:extremism"])/se

2*pnorm(-abs(wald.z)) # Significant at 0.01

varmat <- vcov(model3)[c("typeJuniorMember:extremism", "typeSingleParty:extremism"),c("typeJuniorMember:extremism", "typeSingleParty:extremism")]

varmat

se <- sqrt( 0.085353126  + 0.178096692 -2*0.007760934)

wald.z <- (model3$coefficients["typeJuniorMember:extremism"]-model3$coefficients["typeSingleParty:extremism"])/se

2*pnorm(-abs(wald.z)) # Non-significant

varmat <- vcov(model3)[c("typePrimeMinister:extremism", "typeSingleParty:extremism"),c("typePrimeMinister:extremism", "typeSingleParty:extremism")]

varmat

se <- sqrt( 0.249646700  + 0.178096692 -2*0.007760934)

wald.z <- (model3$coefficients["typePrimeMinister:extremism"]-model3$coefficients["typeSingleParty:extremism"])/se

2*pnorm(-abs(wald.z)) # Significant at 0.01

varmat <- vcov(model3)[c("typeJuniorMember:pre_seat_share", "typePrimeMinister:pre_seat_share"),c("typeJuniorMember:pre_seat_share", "typePrimeMinister:pre_seat_share")]

varmat

se <- sqrt(0.00075622475   +0.00139869612 -2*0.00008018843) 

wald.z <- (model3$coefficients["typeJuniorMember:pre_seat_share"]-model3$coefficients["typePrimeMinister:pre_seat_share"])/se

2*pnorm(-abs(wald.z)) # nonsignificant

varmat <- vcov(model3)[c("typeJuniorMember:pre_seat_share", "typeSingleParty:pre_seat_share"),c("typeJuniorMember:pre_seat_share", "typeSingleParty:pre_seat_share")]

varmat

se <- sqrt( 0.00075622475  + 0.00274350599 -2*0.00008018843)

wald.z <- (model3$coefficients["typeJuniorMember:pre_seat_share"]-model3$coefficients["typeSingleParty:pre_seat_share"])/se

2*pnorm(-abs(wald.z)) # Significant at 0.10 level

varmat <- vcov(model3)[c("typePrimeMinister:pre_seat_share", "typeSingleParty:pre_seat_share"),c("typePrimeMinister:pre_seat_share", "typeSingleParty:pre_seat_share")]

varmat

se <- sqrt(0.00139869612   + 0.00274350599 -2*0.00008018843)

wald.z <- (model3$coefficients["typePrimeMinister:pre_seat_share"]-model3$coefficients["typeSingleParty:pre_seat_share"])/se

2*pnorm(-abs(wald.z)) # Significant at 0.10 level

varmat <- vcov(model3)[c("typeJuniorMember:minority", "typePrimeMinister:minority"),c("typeJuniorMember:minority", "typePrimeMinister:minority")]

varmat

se <- sqrt(0.34559891   +0.66431864 -2*0.04437946) 

wald.z <- (model3$coefficients["typeJuniorMember:minority"]-model3$coefficients["typePrimeMinister:minority"])/se

2*pnorm(-abs(wald.z)) # significant

varmat <- vcov(model3)[c("typeJuniorMember:minority", "typeSingleParty:minority"),c("typeJuniorMember:minority", "typeSingleParty:minority")]

varmat

se <- sqrt(0.34559891  + 1.03662326 -2*0.04437946)

wald.z <- (model3$coefficients["typeJuniorMember:minority"]-model3$coefficients["typeSingleParty:minority"])/se

2*pnorm(-abs(wald.z)) # Non-significant

varmat <- vcov(model3)[c("typePrimeMinister:minority", "typeSingleParty:minority"),c("typePrimeMinister:minority", "typeSingleParty:minority")]

varmat

se <- sqrt(0.66431864   + 1.03662326 -2*0.04437946)

wald.z <- (model3$coefficients["typePrimeMinister:minority"]-model3$coefficients["typeSingleParty:minority"])/se

2*pnorm(-abs(wald.z)) # Non-significant.