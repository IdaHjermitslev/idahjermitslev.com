# Attach packages
library(readxl)
library(haven)
library(tidyverse)
library(lubridate)
library(fuzzyjoin)

# Load data
cheseesres <- read_excel("cheseesres.xlsx") %>%
  rename(comment_gov_status = `comment to gov_status`) %>%
  mutate(survey_start_date = dmy(survey_start_date),
         #create id
         obs_id = 1:518) %>%
  select(-cmp_id, -parlgov)

italy <- read_excel("italy.xlsx")
portfoliofullname <- read_excel("portfoliofullname.xlsx")
# No country code for italy
portfolio <- bind_rows(portfoliofullname, italy) %>%
  rename(cmp = party_id_cmp, parlgov = party_id_parlgov) %>%
  mutate(cabinet_start_date = ymd(cabinet_start_date),
         cabinet_end_date = ymd(cabinet_end_date))

cmp <- read.csv("MPDataset_MPDS2020b.csv") %>%
  mutate(election_date = dmy(edate)) %>%
  mutate(galtancmp = per104 + per105 + per106 + per107 + per108 + per109 +
           per110 + per201 + per202 + per305 + per416 + per501 + per601 + per602 +
           per603 + per604 + per605 + per606 + per607 + per608 + per705,
         econcmp = per401 + per402 + per403 + per404 + per405 + per406 +
           per407 + per408 + per409 + per410 + per411 + per412 + per413 + per414 +
           per415 + per504 + per505 + per506 + per507 + per701 + per702,
         relative_temp = (econcmp-galtancmp)/(econcmp+galtancmp)
  )

# Calculate the average relative salience in each party system
systemic_salience <- cmp %>%
  group_by(countryname, date) %>%
  summarise(systemic_salience = mean(relative_temp)) %>%
  ungroup()

cmp <- cmp %>%  
  left_join(systemic_salience) %>%
  mutate(relative_salience = relative_temp-systemic_salience) %>%
  select(election_date, cmp = party, rile, parfam,
         galtancmp, econcmp, relative_salience)

# All party data
all_data <- cheseesres %>%
  fuzzy_left_join(cmp,
                  by = c(
                    "cmp" = "cmp",
                    "survey_start_date" = "election_date"
                  ),
                  match_fun = list(`==`, `>`)) %>%
  group_by(obs_id) %>% 
  filter(election_date == max(election_date)) %>%
  select(-cmp.x) %>%
  rename(cmp = cmp.y) %>%
  ungroup()

## This can be taken out if we never use this file anyway)  
write_dta(all_data, "complete_all_FEB23.dta") 
##

gov_data <- all_data %>%
  select(-country) %>%
  filter(gov_status %in%  c(2,3)) %>%
  fuzzy_left_join(portfolio,
                  by = c(
                    "cmp" = "cmp",
                   # "parlgov" = "parlgov",
                    "survey_start_date" = "cabinet_start_date"
                  ),
                  match_fun = list(`==`, `>`)) %>%
  group_by(obs_id) %>%
  filter(cabinet_start_date == max(cabinet_start_date)) %>%
  select(-cmp.x, -election_date.y) %>%
  rename(cmp = cmp.y,
         election_date = election_date.x) %>%
  ungroup()
  
write_dta(gov_data, "complete_gov_FEB23.dta")


