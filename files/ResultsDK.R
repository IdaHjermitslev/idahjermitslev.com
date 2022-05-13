# Clear environment
rm(list=ls())

# Set working directory
setwd("Z:/Documents/Mainstream Sellouts")

# Load packages
library(tidyverse)
library(haven)
library(scales)
library(lubridate)
library(psych)
library(gsynth)
library(panelView)
library(xtable)

dnes94 <- read_csv("ElectionStudy1994.csv") %>%
# Recode the month survey responses were collected  
  mutate(date = case_when(
    v480 != 100 ~ ymd(paste(1994,10,v480)),
    v481 != 100 ~ ymd(paste(1994,11,v481)),
    v482 != 100 ~ ymd(paste(1994,12,v482))
  )) %>%
  mutate(date = floor_date(date, unit="week")) %>%
  dplyr::select(date,
                general_SD = v321, general_K = v322, general_V = v323, general_RV = v324, general_SF = v325, general_FP = v326, 
                general_KD = v327, general_CD = v328, general_EL = v329,
                asylum_SD = v331, asylum_K = v332, asylum_V = v333, asylum_RV = v334, asylum_SF = v335, asylum_FP = v336, 
                asylum_KD = v337, asylum_CD = v338, asylum_EL = v339,
                spending_SD = v341, spending_K = v342, spending_V = v343, spending_RV = v344, spending_SF = v345, spending_FP = v346, 
                spending_KD = v347, spending_CD = v348, spending_EL = v349,
                green_SD = v351, green_K = v352, green_V = v353, green_RV = v354, green_SF = v355, green_FP = v356, 
                green_KD = v357, green_CD = v358, green_EL = v359,
                crime_SD = v371, crime_K = v372, crime_V = v373, crime_RV = v374, crime_SF =v375, crime_FP = v376, crime_KD = v377, 
                crime_CD = v378, crime_EL=v379
                )  %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(general_SD:general_EL), function(x) ifelse(x <= 10, x, NA)) %>%
  mutate_at(vars(general_SD:general_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(asylum_SD:green_EL), function(x) reverse.code(keys=-1,items=x)) %>%
  gather(variable, perPos, general_SD:crime_EL) %>%
  separate(variable, c("issue", "party"))

# Create a separate dataset with issue self-placement of party supporters
id94 <- read_csv("ElectionStudy1994.csv") %>%
  mutate(date = case_when(
    v480 != 100 ~ ymd(paste(1994,10,v480)),
    v481 != 100 ~ ymd(paste(1994,11,v481)),
    v482 != 100 ~ ymd(paste(1994,12,v482))
  )) %>%
  mutate(date = floor_date(date, unit="week")) %>%
  dplyr::select(date,
                supporter = v116, #same as identification
                general_self = v330,
                asylum_self = v340,
                spending_self = v350,
                green_self = v360,
                crime_self = v380) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate(general_self = ifelse(general_self <= 10, general_self, NA),
         general_self = scales::rescale(general_self, to=c(0,1)),
         party = case_when(
           supporter == 1 ~ "SD",
           supporter == 2 ~ "RV",
           supporter == 3 ~ "K",
           supporter == 4 ~ "CD",
           supporter == 5 ~ "SF",
           supporter == 6 ~ "KD",
           supporter == 7 ~ "V",
           supporter == 8 ~ "FP",
           supporter == 9 ~ "EL"
         )
         )

# For each party calculate the mean positions of supporters
member94 <- id94 %>%
  group_by(party) %>%
  summarise(
    general_member = mean(general_self, na.rm=T),
    asylum_member = mean(asylum_self, na.rm=T),
    spending_member = mean(spending_self, na.rm=T),
    green_member = mean(green_self, na.rm=T),
    crime_member = mean(crime_self, na.rm=T)
  ) %>%
  ungroup

# Add average supporter position to the main data set
dnes94 <- dnes94 %>%
  left_join(member94)

nrow(dnes94) # 90945

#Data is collected between 1998-03-11 and 1998-06-30. Divide into weekly data
startDate <- ymd("1998-03-11")
endDate <- ymd("1998-06-30")
myDates <-seq(from = startDate, to = endDate, by = "weeks")
datesTemp = rep(myDates, each=ceiling(2001/length(myDates)))[1:2001]

dnes98 <- read_csv("ElectionStudy1998.csv") %>%
  dplyr::select(
    general_SD = v175, general_RV = v176, general_K = v177, general_CD = v178, general_SF = v179, general_DF = v180, 
                general_KD = v181, general_V = v182, general_FP = v183, general_EL = v184,
                asylum_SD = v185, asylum_RV = v186, asylum_K = v187, asylum_CD = v188, asylum_SF = v189, asylum_DF = v190, asylum_KD = v191, 
                asylum_V = v192, asylum_FP = v193, asylum_EL = v194,
                spending_SD = v196, spending_RV = v197, spending_K = v198, spending_CD = v199, spending_SF = v200, spending_DF = v201, 
                spending_KD = v202, spending_V = v203, spending_FP = v204, spending_EL = v205,
                green_SD = v207, green_RV = v208, green_K = v209, green_CD = v210, green_SF = v211, green_DF = v212, 
                green_KD = v213, green_V = v214, green_FP = v215, green_EL = v216,
                crime_SD = v229, crime_RV = v230, crime_K = v231, crime_CD = v232, crime_SF =v233, crime_DF = v234, crime_KD = v235, 
                crime_V = v236, crime_FP = v237, crime_EL=v238)  %>%
  mutate(date = datesTemp) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(general_SD:general_EL), function(x) ifelse(x <= 10, x, NA)) %>%
  mutate_at(vars(general_SD:general_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) reverse.code(keys=-1,items=x)) %>%
  gather(variable, perPos, general_SD:crime_EL) %>%
  separate(variable, c("issue", "party")) 

# Create a separate dataset with issue self-placement of party supporters
id98 <- read_csv("ElectionStudy1998.csv") %>%
  dplyr::select(supporter = v60,
                general_self = v174,
                asylum_self = v195,
                spending_self = v206,
                green_self = v217,
                crime_self = v239) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate(date = datesTemp,
    general_self = ifelse(general_self <= 10, general_self, NA),
         general_self = scales::rescale(general_self, to=c(0,1)),
         party = case_when(
           supporter == 1 ~ "SD",
           supporter == 2 ~ "RV",
           supporter == 3 ~ "K",
           supporter == 4 ~ "CD",
           supporter == 5 ~ "SF",
           supporter == 6 ~ "DF",
           supporter == 7 ~ "KD",
           supporter == 9 ~ "V",
           supporter == 10 ~ "FP",
           supporter == 11 ~ "EL"
         )
  )


# For each party calculate the mean positions of supporters
member98 <- id98 %>%
  group_by(party) %>%
  summarise(
    general_member = mean(general_self, na.rm=T),
    asylum_member = mean(asylum_self, na.rm=T),
    spending_member = mean(spending_self, na.rm=T),
    green_member = mean(green_self, na.rm=T),
    crime_member = mean(crime_self, na.rm=T)
  ) %>%
  ungroup

# Add average supporter position to the main data set
dnes98 <- dnes98 %>%
  left_join(member98) 
                 
nrow(dnes98) # 100050

# Recode the date survey responses were collected  
dnes01 <- read_csv("ElectionStudy2001.csv")  %>%
  mutate(month = ifelse(v5==12, 12, paste0(0,v5)),
         day = ifelse(v6>9, v6, paste0(0,v6)),
         date = ymd(paste(v4,month,day))) %>%
  mutate(date = floor_date(date, unit="week")) %>%
  dplyr::select(date,
                general_SD = v188, general_RV = v189, general_K = v190, general_SF = v191, general_DF = v192, 
                general_KD = v193, general_V = v194, general_EL = v195,
                asylum_SD = v196, asylum_RV = v197, asylum_K = v198, asylum_SF = v199, asylum_DF = v200, asylum_KD = v201, 
                asylum_V = v202, asylum_EL = v203,
                spending_SD = v205, spending_RV = v206, spending_K = v207, spending_SF = v208, spending_DF = v209, 
                spending_KD = v210, spending_V = v211, spending_EL = v212,
                green_SD = v214, green_RV = v215, green_K = v216, green_SF = v217, green_DF = v218, 
                green_KD = v219, green_V = v220, green_EL = v221,
                crime_SD = v223, crime_RV = v224, crime_K = v225, crime_SF = v226, crime_DF = v227, 
                crime_KD = v228, crime_V = v229, crime_EL = v230)  %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(general_SD:general_EL), function(x) ifelse(x <= 10, x, NA)) %>%
  mutate_at(vars(general_SD:general_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) reverse.code(keys=-1,items=x)) %>%
  gather(variable, perPos, general_SD:crime_EL) %>%
  separate(variable, c("issue", "party")) 

# Create a separate dataset with issue self-placement of party supporters
id01 <- read_csv("ElectionStudy2001.csv")  %>%
  mutate(month = ifelse(v5==12, 12, paste0(0,v5)),
         day = ifelse(v6>9, v6, paste0(0,v6)),
         date = ymd(paste(v4,month,day))) %>%
  mutate(date = floor_date(date, unit="week")) %>%
  dplyr::select(date,
                supporter = v50,
                general_self = v187,
                asylum_self = v204,
                spending_self = v213,
                green_self = v222,
                crime_self = v231) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate(general_self = ifelse(general_self <= 10, general_self, NA),
         general_self = scales::rescale(general_self, to=c(0,1)),
         party = case_when(
           supporter == 1 ~ "SD",
           supporter == 2 ~ "RV",
           supporter == 3 ~ "K",
           supporter == 4 ~ "SF",
           supporter == 5 ~ "DF",
           supporter == 6 ~ "KD",
           supporter == 7 ~ "V",
           supporter == 8 ~ "EL"
         )
  )

# For each party calculate the mean positions of supporters
member01 <- id01 %>%
  group_by(party) %>%
  summarise(
    general_member = mean(general_self, na.rm=T),
    asylum_member = mean(asylum_self, na.rm=T),
    spending_member = mean(spending_self, na.rm=T),
    green_member = mean(green_self, na.rm=T),
    crime_member = mean(crime_self, na.rm=T)
  ) %>%
  ungroup

# Add average supporter position to the main data set
dnes01 <- dnes01 %>%
  left_join(member01) 

nrow(dnes01) # 81040

# Data is collected between 2005-02-05 and 2005-08-31
startDate <- ymd("2005-02-05")
endDate <- ymd("2005-08-31")
myDates <-seq(from = startDate, to = endDate, by = "weeks")
datesTemp = rep(myDates, each=ceiling(2264/length(myDates)))[1:2264]

dnes05 <- read_csv("ElectionStudy2005.csv") %>%
  dplyr::select(
    general_SD = v253, general_RV = v254, general_K = v255, general_SF = v256, general_DF = v257, 
                general_V = v258, general_EL = v259,
                asylum_SD = v260, asylum_RV = v261, asylum_K = v262, asylum_SF = v263, asylum_DF = v264, 
                asylum_V = v265, asylum_EL = v266,
                spending_SD = v268, spending_RV = v269, spending_K = v270, spending_SF = v271, spending_DF = v272, 
                spending_V = v273, spending_EL = v274,
                green_SD = v276, green_RV = v277, green_K = v278, green_SF = v279, green_DF = v280, 
                green_V = v281, green_EL = v282,
                crime_SD = v284, crime_RV = v285, crime_K = v286, crime_SF = v287, crime_DF = v288, 
                crime_V = v289, crime_EL = v290)  %>%
  mutate(date = datesTemp) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(general_SD:general_EL), function(x) ifelse(x <= 10, x, NA)) %>%
  mutate_at(vars(general_SD:general_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) reverse.code(keys=-1,items=x)) %>%
  gather(variable, perPos, general_SD:crime_EL) %>%
  separate(variable, c("issue", "party")) 

# Create a separate dataset with issue self-placement of party supporters
id05 <- read_csv("ElectionStudy2005.csv") %>%
  dplyr::select(
    supporter = v117,
                general_self = v252,
                asylum_self = v267,
                spending_self = v275,
                green_self = v283,
                crime_self = v291) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate(date = datesTemp,
         general_self = ifelse(general_self <= 10, general_self, NA),
         general_self = scales::rescale(general_self, to=c(0,1)),
         party = case_when(
           supporter == 1 ~ "SD",
           supporter == 2 ~ "RV",
           supporter == 3 ~ "K",
           supporter == 4 ~ "CD",
           supporter == 5 ~ "SF",
           supporter == 8 ~ "DF",
           supporter == 6 ~ "KD",
           supporter == 9 ~ "V",
           supporter == 10 ~ "EL"
         )
  )

# For each party calculate the mean positions of supporters
member05 <- id05 %>%
  group_by(party, #date
           ) %>%
  summarise(
    general_member = mean(general_self, na.rm=T),
    asylum_member = mean(asylum_self, na.rm=T),
    spending_member = mean(spending_self, na.rm=T),
    green_member = mean(green_self, na.rm=T),
    crime_member = mean(crime_self, na.rm=T)
  ) %>%
  ungroup

# Add average supporter position to the main data set
dnes05 <- dnes05 %>%
  left_join(member05) 

nrow(dnes05) # 79240

# Data is collected between 2007-11-07 and 2008-06-12
startDate <- ymd("2007-11-07")
endDate <- ymd("2008-06-12")
myDates <-seq(from = startDate, to = endDate, by = "weeks")
datesTemp = rep(myDates, each=ceiling(4018/length(myDates)))[1:4018]

dnes07 <- read_csv("ElectionStudy2007.csv") %>%
  dplyr::select(
    general_SD = v292, general_RV = v293, general_K = v294, general_SF = v295, general_KD = v296, general_DF = v297, 
                general_V = v298, general_LA = v299, general_EL = v300,
                asylum_SD = v301, asylum_RV = v302, asylum_K = v303, asylum_SF = v304, asylum_DF = v305, 
                asylum_V = v306, asylum_LA = v307, asylum_EL = v308,
                spending_SD = v310, spending_RV = v311, spending_K = v312, spending_SF = v313, spending_DF = v314, 
                spending_V = v315, spending_LA = v316, spending_EL = v317,
                green_SD = v319, green_RV = v320, green_K = v321, green_SF = v322, green_DF = v323, 
                green_V = v324, green_LA = v325, green_EL = v326,
                crime_SD = v328, crime_RV = v329, crime_K = v330, crime_SF = v331, crime_DF = v332, 
                crime_V = v333, crime_LA = v334, crime_EL = v335)  %>%
  mutate(date = datesTemp) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(general_SD:general_EL), function(x) ifelse(x <= 10, x, NA)) %>%
  mutate_at(vars(general_SD:general_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) reverse.code(keys=-1,items=x)) %>%
  gather(variable, perPos, general_SD:crime_EL) %>%
  separate(variable, c("issue", "party")) 

# Create a separate dataset with issue self-placement of party supporters
id07 <- read_csv("ElectionStudy2007.csv") %>%
  dplyr::select(
    supporter = v118,
    general_self = v291,
    asylum_self = v309,
    spending_self = v318,
    green_self = v327,
    crime_self = v336) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate(date = datesTemp,
         general_self = ifelse(general_self <= 10, general_self, NA),
         general_self = scales::rescale(general_self, to=c(0,1)),
         party = case_when(
           supporter == 1 ~ "SD",
           supporter == 2 ~ "RV",
           supporter == 3 ~ "K",
           supporter == 4 ~ "SF",
           supporter == 6 ~ "DF",
           supporter == 5 ~ "KD",
           supporter == 7 ~ "LA",
           supporter == 8 ~ "V",
           supporter == 9 ~ "EL"
         )
  )

# For each party calculate the mean positions of supporters
member07 <- id07 %>%
  group_by(party) %>%
  summarise(
    general_member = mean(general_self, na.rm=T),
    asylum_member = mean(asylum_self, na.rm=T),
    spending_member = mean(spending_self, na.rm=T),
    green_member = mean(green_self, na.rm=T),
    crime_member = mean(crime_self, na.rm=T)
  ) %>%
  ungroup

# Add average supporter position to the main data set
dnes07 <- dnes07 %>%
  left_join(member07) 

nrow(dnes07) # 164738

# Data is collected between 2011-09-14 and 2011-12-16
startDate <- ymd("2011-09-14")
endDate <- ymd("2011-12-16")
myDates <-seq(from = startDate, to = endDate, by = "weeks")
datesTemp = rep(myDates, each=ceiling(2078/length(myDates)))[1:2078]

dnes11 <- read_csv("ElectionStudy2011.csv") %>%
  mutate(crime_SD = ifelse(v263 <= 5, v263, v272),
         crime_RV = ifelse(v264 <= 5, v264, v273),
         crime_K = ifelse(v265 <= 5, v265, v274),
         crime_SF = ifelse(v266 <= 5, v266, v275),
         crime_LA = ifelse(v267 <= 5, v267, v276),
         crime_DF = ifelse(v268 <= 5, v268, v277),
         crime_V = ifelse(v269 <= 5, v269, v278),
         crime_EL = ifelse(v270 <= 5, v270, v279)) %>%
   dplyr::select(
                general_SD = v228, general_RV = v229, general_K = v230, general_SF = v231, general_LA = v232, general_DF = v233, 
                general_V = v234, general_EL = v235,
                asylum_SD = v236, asylum_RV = v237, asylum_K = v238, asylum_SF = v239, asylum_LA = v240, asylum_DF = v241, 
                asylum_V = v242, asylum_EL = v243,
                spending_SD = v245, spending_RV = v246, spending_K = v247, spending_SF = v248, spending_LA = v249, spending_DF = v250, 
                spending_V = v251, spending_EL = v252,
                green_SD = v254, green_RV = v255, green_K = v256, green_SF = v257, green_LA = v258, green_DF = v259, 
                green_V = v260, green_EL = v261,
                crime_SD, crime_RV, crime_K, crime_SF, crime_LA, crime_DF, 
                crime_V, crime_EL
                )  %>%
  mutate(date = datesTemp) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(general_SD:general_EL), function(x) ifelse(x <= 10, x, NA)) %>%
  mutate_at(vars(general_SD:general_EL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(asylum_SD:crime_EL), function(x) reverse.code(keys=-1,items=x)) %>%
  gather(variable, perPos, general_SD:crime_EL) %>%
  separate(variable, c("issue", "party")) 

# Create a separate dataset with issue self-placement of party supporters
id11 <- read_csv("ElectionStudy2011.csv") %>%
  mutate(crime_self = ifelse(v271 <= 5, v271, v280)) %>%
  dplyr::select(
    supporter = v85,
    general_self = v227,
    asylum_self = v244,
    spending_self = v253,
    green_self = v262,
    crime_self) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate(date = datesTemp,
         general_self = ifelse(general_self <= 10, general_self, NA),
         general_self = scales::rescale(general_self, to=c(0,1)),
         party = case_when(
           supporter == 1 ~ "SD",
           supporter == 2 ~ "RV",
           supporter == 3 ~ "K",
           supporter == 4 ~ "SF",
           supporter == 7 ~ "DF",
           supporter == 6 ~ "KD",
           supporter == 5 ~ "LA",
           supporter == 8 ~ "V",
           supporter == 9 ~ "EL"
         )
  )

# For each party calculate the mean positions of supporters
member11 <- id11 %>%
  group_by(party) %>%
  summarise(
    general_member = mean(general_self, na.rm=T),
    asylum_member = mean(asylum_self, na.rm=T),
    spending_member = mean(spending_self, na.rm=T),
    green_member = mean(green_self, na.rm=T),
    crime_member = mean(crime_self, na.rm=T)
  ) %>%
  ungroup

# Add average supporter position to the main data set
dnes11 <- dnes11 %>%
  left_join(member11) 

nrow(dnes11) #83120

# Data is collected between 2015-06-19 and 2015-10-27
startDate <- ymd("2015-06-19")
endDate <- ymd("2015-10-27")
myDates <-seq(from = startDate, to = endDate, by = "weeks")
datesTemp = rep(myDates, each=ceiling(4210/length(myDates)))[1:4120]  

dnes15 <- read_csv("ElectionStudy2015.csv") %>%
  dplyr::select(general_SD = q70b, general_RV = q70c, general_K = q70d, general_SF = q70e, general_LA = q70f, general_DF = q70g, 
                general_V = q70h, general_EL = q70i, general_AL = q70j,
                asylum_SD = q71a, asylum_RV = q71b, asylum_K = q71c, asylum_SF = q71d, asylum_LA = q71e, asylum_DF = q71f, 
                asylum_V = q71g, asylum_EL = q71h, asylum_AL = q71i,
                spending_SD = q72a, spending_RV = q72b, spending_K = q72c, spending_SF = q72d, spending_LA = q72e, spending_DF = q72f, 
                spending_V = q72g, spending_EL = q72h, spending_AL = q72i,
                green_SD = q73a, green_RV = q73b, green_K = q73c, green_SF = q73d, green_LA = q73e, green_DF = q73f, 
                green_V = q73g, green_EL = q73h, green_AL = q73i,
                crime_SD = q74a, crime_RV = q74b, crime_K = q74c, crime_SF = q74d, crime_LA = q74e, crime_DF = q74f, 
                crime_V = q74g, crime_EL = q74h, crime_AL = q74i)  %>%
  mutate(date = datesTemp) %>%
  mutate_at(vars(asylum_SD:crime_AL), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_SD:crime_AL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(general_SD:general_AL), function(x) ifelse(x <= 10, x, NA)) %>%
  mutate_at(vars(general_SD:general_AL), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate_at(vars(asylum_SD:crime_AL), function(x) reverse.code(keys=-1,items=x)) %>%
  gather(variable, perPos, general_SD:crime_AL) %>%
  separate(variable, c("issue", "party")) 

# Create a separate dataset with issue self-placement of party supporters
id15 <- read_csv("ElectionStudy2015.csv") %>%
  dplyr::select(
    supporter = q24,
    general_self = q70a,
    asylum_self = q71j,
    spending_self = q72j,
    green_self = q73j,
    crime_self = q74j) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) ifelse(x <= 5, x, NA)) %>%
  mutate_at(vars(asylum_self:crime_self), function(x) scales::rescale(x, to=c(0,1))) %>%
  mutate(date = datesTemp,
         general_self = ifelse(general_self <= 10, general_self, NA),
         general_self = scales::rescale(general_self, to=c(0,1)),
         party = case_when(
           supporter == 1 ~ "SD",
           supporter == 2 ~ "RV",
           supporter == 3 ~ "K",
           supporter == 4 ~ "SF",
           supporter == 7 ~ "DF",
           supporter == 6 ~ "KD",
           supporter == 5 ~ "LA",
           supporter == 8 ~ "V",
           supporter == 9 ~ "EL",
           supporter == 10 ~ "AL"
         )
  )

# For each party calculate the mean positions of supporters
member15 <- id15 %>%
  group_by(party) %>%
  summarise(
    general_member = mean(general_self, na.rm=T),
    asylum_member = mean(asylum_self, na.rm=T),
    spending_member = mean(spending_self, na.rm=T),
    green_member = mean(green_self, na.rm=T),
    crime_member = mean(crime_self, na.rm=T)
  ) %>%
  ungroup

# Add average supporter position to the main data set
dnes15 <- dnes15 %>%
  left_join(member15) 

nrow(dnes15) #185400

# Stack the seven datasets on top of each other
dataFull <- bind_rows(list(dnes94, dnes98, dnes01, dnes05, dnes07, dnes11, dnes15), .id="year") %>%
  mutate(year = ifelse(year == 1, 1994,
                       ifelse(year == 2, 1998,
                              ifelse(year == 3, 2001,
                                     ifelse(year == 4, 2005,
                                            ifelse(year == 5, 2007,
                                                   ifelse(year == 6, 2011,
                                                          ifelse(year == 7, 2015, NA)))))))) 

nrow(dataFull) #784533

# Handcode CMP and CHES id's for each party
dataFull <- dataFull %>% 
  mutate(cmp = ifelse(party == "SD", 13320, 
                      ifelse(party == "K", 13620,
                             ifelse(party == "V", 13420,
                                    ifelse(party == "RV", 13410,
                                           ifelse(party == "SF", 13230,
                                                  ifelse(party == "FP", 13951,
                                                         ifelse(party == "KD", 13520,
                                                                ifelse(party == "CD", 13330,
                                                                       ifelse(party == "EL", 13229,
                                                                              ifelse(party == "DF", 13720,
                                                                                     ifelse(party == "LA", 13001,
                                                                                            ifelse(party == "AL", 13110, NA)))))))))))),
         ches = case_when(
           party == "SD"~ 201,
           party == "K" ~ 203,
           party == "V" ~ 211,
           party == "RV" ~ 202,
           party == "SF" ~ 206,
           party == "EL" ~ 213,
           party == "FP" ~ 212,
           party == "KD" ~ 210,
           party == "CD" ~ 204,
           party == "DF" ~ 215,
           party == "LA" ~ 218
         ),
         
  # Match election years with the closest wave of the CHES
         CHESy = case_when(
           year < 2001 ~ 1999,
           year == 2001 ~ 2002,
           year == 2005 ~ 2006,
           year == 2007 ~ 2006,
           year == 2011 ~ 2010,
           year == 2015 ~ 2014
         ))

descriptives <- dataFull %>%
  group_by(year, issue, party) %>%
  summarise(meanPos = mean(perPos, na.rm=TRUE),
            sdPos = sd(perPos, na.rm=TRUE)) %>%
  ungroup

descriptives %>%
  filter(issue=="spending") %>%
  xtable(digits=2)

# Add CMP variables to create proxies for party positions on relevant issues
cmp <- read.csv("MARPOR2019.csv") %>%
  mutate(year = floor(date/100),
         absseat = absseat,
         CMPgeneral = scales::rescale(rile, to = c(0,1)),
         CMPcrime = scales::rescale(per605, to = c(0,1)), 
         CMPasylum = per601 + per608 - per602 - per607,
         CMPasylum = scales::rescale(CMPasylum, to = c(0,1)),
         CMPspending = per505- per507-per504-per506,
         CMPspending = scales::rescale(CMPspending, to = c(0,1)),
         CMPgreen = per410 - per416 - per501,
         CMPgreen = scales::rescale(CMPgreen, to = c(0,1))
  ) %>%
  dplyr::select(year, cmp = party, absseat, CMPgeneral,  CMPcrime, CMPasylum,
                CMPspending, CMPgreen)

ches <- read_dta("CHES.dta")%>%
  filter(party_id %in% 201:218) %>%
  dplyr::select(ches=party_id, CHESy = year, lrgen, lrecon, galtan) %>%
  mutate_at(vars(lrgen:galtan), function(x) as.numeric(x)) %>%
  mutate_at(vars(lrgen:galtan), function(x) scales::rescale(x, to=c(0,1)))

# Merge the CMP and CHES data with the election study
# Take weekly averages
dataAvg <- dataFull %>%
  group_by(date, year, CHESy, party, cmp, ches, issue, 
           general_member, asylum_member, spending_member, green_member, crime_member) %>%
  summarise(meanPos = mean(perPos, na.rm=TRUE)) %>%
  ungroup %>%
  left_join(cmp) %>%
  left_join(ches) %>%
  spread(key=issue, value=meanPos) %>%
  as.data.frame()

# Extract only the relevant variables to assess positions towards asylum and put data in the correct format
dataAsylum <- dataAvg %>%
  dplyr::select(asylum, 
                CMPasylum, CMPgeneral, absseat, galtan, lrgen, asylum_member, party, date) %>%
  na.omit() %>%
  mutate(
    treatment = ifelse(party %in% c("V","K") & date >= ymd("2001-11-27"),1,0),
    dateUnix = as.numeric(as.POSIXct(date))
  ) %>%
 filter(party %in% c( "K", "V", "RV", "SD", "SF", "EL", "KD"))  %>%
  as.data.frame()

# Examine the data stucture (not necessary to reproduce results)
panelView(asylum ~ treatment, data = dataAsylum,  index = c("party","dateUnix"), pre.post = TRUE) 
panelView(asylum ~ treatment, data = dataAsylum,  index = c("party","dateUnix"), type = "outcome") 


# Use the GSCM and create synthetic controls
outAsylum <- gsynth(asylum ~ treatment + CMPasylum + CMPgeneral + galtan + lrgen + absseat + asylum_member, data = dataAsylum, index = c("party","dateUnix"), 
              force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", EM=TRUE,
              nboots = 1000, parallel = FALSE)

# Look at ATT averaged across time and locate coefficient estimates
print(outAsylum)

# Plot ATT
plot(outAsylum, theme.bw = TRUE, main = "", xlab ="") +
  xlab("Time relative to treatment")


# Look at counter-factual versus treated average 
plot(outAsylum, type = "counterfactual", raw = "all", theme.bw = TRUE, main = "Asylum and refugees") 

# Specify the correct dates to create a nice-looking plot
dates <- as.numeric(as.POSIXct(c("1998-03-11", "2002-02-13", "2005-02-05", "2007-11-07", "2011-09-14")))
# Move the shaded area so it starts at election date and NOT the last observation pre-treatment
xp <- plot(outAsylum, type = "counterfactual", raw = "all", theme.bw = TRUE, main = "", xlab="Date", shade.post = FALSE) +
  annotate("rect", xmin=1006815600, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.3) +
  scale_x_continuous(breaks = dates, labels=c("1998-03-11", "2002-02-13", "2005-02-05", "2007-11-07", "2011-09-14")) 

xp$layers <- xp$layers[2:3]
xp

#Extract coefficients
xtable(outAsylum$est.beta)

# Extract only the relevant variables
dataCrime <- dataAvg %>%
  dplyr::select(crime, CMPcrime, CMPgeneral, absseat, galtan, lrgen, crime_member, party, date) %>%
  na.omit() %>%
  mutate(
    treatment = ifelse(party %in% c("V","K") & date >= ymd("2001-11-27"),1,0),
    dateUnix = as.numeric(as.POSIXct(date))
  ) %>%
  filter(party %in% c( "K", "V", "RV", "SD", "SF", "EL", "KD"))  %>%
  as.data.frame()

# Examine the data stucture (not necessary to reproduce results)
panelView(crime ~ treatment, data = dataCrime,  index = c("party","dateUnix"), pre.post = TRUE) 
panelView(crime ~ treatment, data = dataCrime,  index = c("party","dateUnix"), type = "outcome") 


# Use the GSCM and create synthetic controls
outCrime <- gsynth(crime ~  ~ treatment + CMPcrime + CMPgeneral + galtan + lrgen + absseat + crime_member, data = dataCrime, index = c("party","dateUnix"), 
                    force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", EM=TRUE,
                    nboots = 1000, parallel = FALSE)

print(outCrime)

plot(outCrime, theme.bw = TRUE, main = "Law and order ATT", xlab ="")

# Look at counter-factual versus treated average 
# Specify the correct dates to create a nice-looking plot
# Move the shaded area so it starts at election date and NOT the last observation pre-treatment
dates <- as.numeric(as.POSIXct(c("1998-03-11", "2002-02-13", "2005-02-05", "2007-11-07", "2011-09-14")))
xp <- plot(outCrime, type = "counterfactual", raw = "all", theme.bw = TRUE, main = "Law and order", xlab="Date", shade.post = FALSE) +
  annotate("rect", xmin=1006815600, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.3) +
  scale_x_continuous(breaks = dates, labels=c("1998-03-11", "2002-02-13", "2005-02-05", "2007-11-07", "2011-09-14")) 

xp$layers <- xp$layers[2:3]
xp

# Extract only the relevant variables
dataGreen <- dataAvg %>%
  dplyr::select(green, CMPgreen, CMPgeneral, absseat, galtan, lrgen, party, green_member, date) %>%
  na.omit() %>%
  mutate(
    treatment = ifelse(party %in% c("V","K") & date >= ymd("2001-11-27"),1,0),
    dateUnix = as.numeric(as.POSIXct(date))
  ) %>%
  filter(party %in% c( "K", "V", "RV", "SD", "SF", "EL", "KD"))  %>%
  as.data.frame()

panelView(green ~ treatment, data = dataGreen,  index = c("party","dateUnix"), pre.post = TRUE) 
panelView(green ~ treatment, data = dataGreen,  index = c("party","dateUnix"), type = "outcome") 

outGreen <- gsynth(green ~ treatment + CMPgreen + CMPgeneral + galtan + lrgen + absseat + green_member, data = dataGreen, index = c("party","dateUnix"), 
                   force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", EM=TRUE,
                   nboots = 1000, parallel = FALSE)

print(outGreen)

plot(outGreen, theme.bw = TRUE, main = "Environment ATT", xlab ="")

# Look at counter-factual versus treated average 
# Specify the correct dates to create a nice-looking plot
# Move the shaded area so it starts at election date and NOT the last observation pre-treatment
dates <- as.numeric(as.POSIXct(c("1998-03-11", "2002-02-13", "2005-02-05", "2007-11-07", "2011-09-14")))
xp <- plot(outGreen, type = "counterfactual", raw = "all", theme.bw = TRUE, main = "Environment", xlab="Date", shade.post = FALSE) +
  annotate("rect", xmin=1006815600, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.3) +
  scale_x_continuous(breaks = dates, labels=c("1998-03-11", "2002-02-13", "2005-02-05", "2007-11-07", "2011-09-14")) 

xp$layers <- xp$layers[2:3]
xp

# Extract only the relevant variables
dataSpending <- dataAvg %>%
  dplyr::select(spending, CMPspending, CMPgeneral, absseat, lrecon, lrgen, party, date, spending_member) %>%
  na.omit() %>%
  mutate(
    treatment = ifelse(party %in% c("V","K") & date >= ymd("2001-11-27"),1,0),
    dateUnix = as.numeric(as.POSIXct(date))
  ) %>%
  filter(party %in% c( "K", "V", "RV", "SD", "SF", "EL", "KD"))  %>%
  as.data.frame()

panelView(spending ~ treatment, data = dataSpending,  index = c("party","dateUnix"), pre.post = TRUE) 
panelView(spending ~ treatment, data = dataSpending,  index = c("party","dateUnix"), type = "outcome") 


outSpending <- gsynth(spending ~ treatment + CMPspending + CMPgeneral + lrecon + lrgen + absseat + spending_member, data = dataSpending, index = c("party","dateUnix"), 
                      force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", EM=TRUE,
                      nboots = 1000, parallel = FALSE)

print(outSpending)

plot(outSpending, theme.bw = TRUE, main = "Public spending")

# Look at counter-factual versus treated average 
# Specify the correct dates to create a nice-looking plot
# Move the shaded area so it starts at election date and NOT the last observation pre-treatment
dates <- as.numeric(as.POSIXct(c("1998-03-11", "2002-02-13", "2005-02-05", "2007-11-07", "2011-09-14")))
xp <- plot(outSpending, type = "counterfactual", raw = "all", theme.bw = TRUE, main = "", xlab="Date", shade.post = FALSE) +
  annotate("rect", xmin=1006815600, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.3) +
  scale_x_continuous(breaks = dates, labels=c("1998-03-11", "2002-02-13", "2005-02-05", "2007-11-07", "2011-09-14")) 

xp$layers <- xp$layers[2:3]
xp

# Extract only the relevant variables
dataGeneral <- dataAvg %>%
  dplyr::select(general, CMPgeneral, absseat, lrgen, party, date, general_member) %>%
  na.omit() %>%
  mutate(
    treatment = ifelse(party %in% c("V","K") & date >= ymd("2001-11-27"),1,0),
    dateUnix = as.numeric(as.POSIXct(date))
  ) %>%
  filter(party %in% c( "K", "V", "RV", "SD", "SF", "EL", "KD"))  %>%
  as.data.frame()

panelView(general ~ treatment, data = dataGeneral,  index = c("party","dateUnix"), pre.post = TRUE) 
panelView(general ~ treatment, data = dataGeneral,  index = c("party","dateUnix"), type = "outcome") 


outGeneral <- gsynth(general ~ treatment + CMPgeneral + lrgen + absseat + general_member, data = dataGeneral, index = c("party","dateUnix"), 
                     force = "two-way", CV = TRUE, r = c(0, 5), se = TRUE, inference = "parametric", EM=TRUE,
                     nboots = 1000, parallel = FALSE)

print(outGeneral)

plot(outGeneral, theme.bw = TRUE, main = "General left/right ATT", axis.adjust = TRUE)


plot(outGeneral, type = "counterfactual", raw = "none", main="")

# Look at counter-factual versus treated average 
# Specify the correct dates to create a nice-looking plot
# Move the shaded area so it starts at election date and NOT the last observation pre-treatment
dates <- as.numeric(as.POSIXct(c("1998-03-11", "2002-02-13", "2005-02-05", "2007-11-07", "2011-09-14")))
xp <- plot(outGeneral, type = "counterfactual", raw = "all", theme.bw = TRUE, main = "", xlab="Date", shade.post = FALSE) +
  annotate("rect", xmin=1006815600, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.3) +
  scale_x_continuous(breaks = dates, labels=c("1998-03-11", "2002-02-13", "2005-02-05", "2007-11-07", "2011-09-14"))


xp$layers <- xp$layers[2:3]
xp
 