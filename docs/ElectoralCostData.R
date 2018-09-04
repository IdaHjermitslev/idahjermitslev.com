# Clear environment
rm(list=ls())

# Load packages
library(lubridate)
library(tidyverse)
library(Hmisc)
library(readr)

# Set your own working directory
setwd("/Users/idahjermitslev/Documents/Governing Cost/Governing Cost Repository")

# Load data (downloaded directly from ParlGov)
cabinets <- read_csv("view_cabinet.csv")

parties <- read_csv("view_party.csv")

elections <- read_csv("view_election.csv") 

# Handcode a couple of weird cases
elections <- elections %>% 
  mutate(vote_share = ifelse(election_id == 161 & party_id == 773, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 161 & party_id == 1017, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 223 & party_id == 773, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 223 & party_id == 1017, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 583 & party_id == 773, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 583 & party_id == 1017, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 290 & party_id == 659, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 290 & party_id == 1547, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 490 & party_id == 659, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 490 & party_id == 1547, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 551 & party_id == 809, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 551 & party_id == 596, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 551 & party_id == 373, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 551 & party_id == 840, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 551 & party_id == 627, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 551 & party_id == 350, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 551 & party_id == 910, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 146 & party_id == 809, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 146 & party_id == 596, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 146 & party_id == 373, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 146 & party_id == 1436, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 146 & party_id == 1321, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 146 & party_id == 1475, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 146 & party_id == 910, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 146 & party_id == 896, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 146 & party_id == 99, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 248 & party_id == 1273, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 248 & party_id == 514, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 248 & party_id == 251, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 248 & party_id == 107, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 248 & party_id == 1772, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 248 & party_id == 1281, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 281 & party_id == 1273, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 281 & party_id == 514, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 281 & party_id == 251, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 281 & party_id == 107, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 281 & party_id == 622, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 281 & party_id == 565, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 281 & party_id == 1281, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 598 & party_id == 514, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 598 & party_id == 1281, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 598 & party_id == 1535, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 480 & party_id == 514, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 480 & party_id == 889, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 480 & party_id == 1535, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 90 & party_id == 514, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 90 & party_id == 1535, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 216 & party_id == 514, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 216 & party_id == 1535, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 352 & party_id == 514, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 352 & party_id == 1535, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 657 & party_id == 514, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 657 & party_id == 1535, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 210 & party_id == 514, seats/seats_total*100, vote_share),
         vote_share = ifelse(election_id == 210 & party_id == 1535, seats/seats_total*100, vote_share))%>%
  filter(party_id!=1496 & party_id !=1023 &
         party_id!=67 & party_id!=1048 & party_id!= 1543 & party_id != 11
         & party_id != 1295)

# Collect info about seat share prior to government
vote_addition <- elections %>%
  # Deselect elections to the european parliament
  filter(election_type == "parliament")%>%
  # Calculate seat share in percentages
  mutate(seat_share = seats/seats_total*100) %>%
  # Define elections as pre-elections (i.e. t-1)
  dplyr::select(pre_election_id = election_id, pre_vote_share = vote_share,
        pre_seat_share = seat_share, party_id)

# Collect info about government characteristics
gov_addition <- cabinets %>%
  dplyr::select(pre_election_id = election_id, previous_cabinet_id = cabinet_id,
         cabinet_party, prime_minister, party_id)

# Collect info about party characteristics
fam_addition <- parties %>%
  dplyr::select(family_name, party_id, left_right) %>%
  mutate(family_name = replace(family_name, family_name == "no family"|
                                 family_name == "electoral alliance" |
                                 family_name == "to be coded", NA))

# Combine with info about the seat share after governing
data <- elections %>%
  # Deselect elections to the european parliament 
  filter(election_type == "parliament")%>%
  # Calculate seat share in percentages
  mutate(seat_share = seats/seats_total*100,
  # Convert election_date to a better format       
         election_date = ymd(election_date)) %>%
  dplyr::select(country_name_short, election_date, post_vote_share = vote_share, post_seat_share = seat_share,
         party_name_short, party_name_english, party_id, post_election_id = election_id,
         previous_cabinet_id, pre_election_id = previous_parliament_election_id) %>%
  # Sory out missing data
  na.omit() %>%
  # Combine info about post-government election with info about the pre-government election
  left_join(vote_addition) %>%
  # Combine with government characteristics
  left_join(gov_addition) %>%
  # Combine with party characteristics
  left_join(fam_addition)

data <- data %>%
# Select only the relevant countries
  filter(country_name_short == "AUS" |
           country_name_short == "AUT" |
           country_name_short =="BEL" |
           country_name_short == "CAN" |
           country_name_short =="DEU" |
           country_name_short == "DNK" |
           country_name_short =="ESP" |
           country_name_short == "FIN" |
           country_name_short == "FRA" |
           country_name_short == "GBR" |
           country_name_short == "GRC" |
           country_name_short =="IRL" |
           country_name_short ==  "ITA"|
           country_name_short == "JPN"|
           country_name_short =="LUX"|
           country_name_short == "NLD"|
           country_name_short =="NOR"|
           country_name_short =="NZL"|
           country_name_short =="PRT"|
           country_name_short == "SWE") %>%
# Select only the relevant years
  filter(year(election_date) > 1959 & year(election_date) < 2016) %>%
# Parties with missing values for cabinet and prime minister gets a zero
  mutate(prime_minister = ifelse(is.na(prime_minister),0,prime_minister),
         cabinet_party = ifelse(is.na(cabinet_party),0,cabinet_party))

data <- data %>%
# Calculate the absolute distance between parties and mid-point of the scale
  mutate(extremism = abs(left_right - 5))

# Create the Garritzman data 
country_name_short <- c("AUS" , "AUT" ,"BEL" , "CAN" , "DEU" , "DNK" ,
                        "ESP" , "FIN" , "FRA" , "GBR" , "GRC" ,"IRL" , 
                        "ITA", "JPN","LUX","NLD","NOR","NZL","PRT", "SWE")

strom <- c(NA, NA, 4, 3, NA, 3, 3, 3, 4, 1, NA, 1, 4, NA, NA, 2, 5, NA, 4, 4)

CSI <- c(0.49, 0.64, 0.44, 0.53, 0.63, 0.5, 0.4, 0.92, 0.2, 0, 0.4, 0.2, 0.56,
         0.53, 0.52, 0.53, 0.84, 0.48, 0.57, 0.88)

OCI <- c(0.61, 0.83, 0.77, 0.62, 0.61, 0.79, 0.41, 0.97, 0.29, 0.22, 0.24, 0.29,
         0.58, 0.62, 0.56, 0.73, 0.9, 0.60, 0.57, 0.96)

OAI <- c(0.66, 0.42, 0.31,  0.78, 0.83, 0.81,0.36, 0.53, 0.33, 0.94, 0.5, 0.89,
         0.44, 0.32, 0.5, 0.14, 0.72, 0.78, 0, 0.75)

opp.influence <- tibble(country_name_short, strom, CSI, OCI, OAI)

# Merge data with Garritzmans measures
data <- opp.influence %>%
  dplyr::select(country_name_short, CSI, OCI, OAI) %>%
  right_join(data)

# Load data from the world bank
growth <- 
  read.csv("growthData.csv") %>% .[1:34,]

# Rename columns according to years
names(growth)[5:60] <- as.character(1960:2015)

# Recode missing variables
growth[growth == ".."] <- NA

# Recode country codes as characters 
growth$Country.Code <- as.character(growth$Country.Code)

# Create placeholders
data$growth <- NA

for (ii in 1:nrow(data)) {
  # Select relevant year
  time <- year(data$election_date)[ii] %>% 
    as.character()
  # Select relevant country
  country <- data$country_name_short[ii]
  data$growth[ii] <- 
    # Find the observations such that country matches row and year matches column
    growth[which(growth$Country.Code == country),which(names(growth) == time)] %>% 
    as.character() %>% 
    as.numeric()
}

# Create placeholders for coalition and minority goverment
data$coalition <- NA
data$minority <- NA

# If there are any parties in the system that are cabinet parties but not PM's enter a 1
for (ii in 1:nrow(data)){
  data$coalition[ii] <- if_else(any(
    data$prime_minister[which(data$previous_cabinet_id == data$previous_cabinet_id[ii])] == 0 &
      data$cabinet_party[which(data$previous_cabinet_id == data$previous_cabinet_id[ii])] == 1,
    na.rm=TRUE), 1, 0)
}

# If the sum of cabinet parties seats is less than 50, give entire system a 1
for (ii in 1:nrow(data)){
  data$minority[ii] <- if_else(
    sum(data$pre_seat_share[which(data$previous_cabinet_id == data$previous_cabinet_id[ii] &
                                    data$cabinet_party == 1)]) < 50,1,0) 
}

data <- data %>%
  mutate(change = post_vote_share-pre_vote_share) %>%
  mutate(junior_member = ifelse(cabinet_party == 1 & prime_minister == 0,1,0)) %>%
  mutate(pm_coalition = ifelse(coalition == 1 & prime_minister == 1,1,0)) %>%
  mutate(single_party = ifelse (coalition == 0 & prime_minister == 1,1,0)) 

nrow(data)

summary(data)

# Save data file to working directory
save(data, file="GovCostData.Rda")