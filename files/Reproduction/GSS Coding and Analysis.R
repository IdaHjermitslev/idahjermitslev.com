##### Coding #####

# Clear environment
rm(list=ls())

# read raw GSS data
gss <- read.csv("Data/GSS.csv", header = T, stringsAsFactors = F)
table(gss$year)


## fatigue-related vars 

# fatigue
gss$fatigue <- as.numeric(ifelse(gss$fatigue %in% 1:5, gss$fatigue, NA))
gss$fatigue01 <- (gss$fatigue - 1) / 4
table(gss$fatigue01)

# too tired when coming home from work to do chores
gss$tiredhme <- 5 - as.numeric(ifelse(gss$tiredhme %in% 1:4, gss$tiredhme, NA))
gss$tiredhm1 <- 5 - as.numeric(ifelse(gss$tiredhm1 %in% 1:4, gss$tiredhm1, NA))
gss$tiredhme01 <- (gss$tiredhme - 1) / 3
gss$tiredhm101 <- (gss$tiredhm1 - 1) / 3
table(gss$tiredhme01)
table(gss$tiredhm101)

# too tired at work because of household work
gss$tiredwrk <- 5 - as.numeric(ifelse(gss$tiredwrk %in% 1:4, gss$tiredwrk, NA))
gss$tiredwk1 <- 5 - as.numeric(ifelse(gss$tiredwk1 %in% 1:4, gss$tiredwk1, NA))
gss$tiredwrk01 <- (gss$tiredwrk - 1) / 3
gss$tiredwk101 <- (gss$tiredwk1 - 1) / 3
table(gss$tiredwrk01)
table(gss$tiredwk101)

# tired
gss$tired0201 <- (gss$tiredhme01 + gss$tiredwrk01) / 2
gss$tired1201 <- (gss$tiredhm101 + gss$tiredwk101) / 2

# trouble going to sleep or staying asleep last 12 months
gss$slpprblm <- 5 - as.numeric(ifelse(gss$slpprblm %in% 1:4, gss$slpprblm, NA))
gss$slpprblm01 <- (gss$slpprblm - 1) / 3
table(gss$slpprblm01)

# how much of time during past week was sleep restless
gss$cesd2 <- as.numeric(ifelse(gss$cesd2 %in% 1:4, gss$cesd2, NA))
gss$cesd201 <- (gss$cesd2 - 1) / 3
table(gss$cesd201)

# how often do you come home from work exhausted
gss$xhaustn <- 6 - as.numeric(ifelse(gss$xhaustn %in% 1:5, gss$xhaustn, NA))
gss$xhaustn01 <- (gss$xhaustn - 1) / 4
table(gss$xhaustn01)


## quality of working conditions (all coded in "bad" direction)

# required to work extra hours
gss$mustwork01 <- as.numeric(ifelse(gss$mustwork == 1, 1, ifelse(gss$mustwork == 2, 0, NA)))
table(gss$mustwork01)

# must work fast (reverse)
gss$workfast <- 5 - as.numeric(ifelse(gss$workfast %in% 1:4, gss$workfast, NA))
gss$workfast01 <- (gss$workfast - 1) / 3
table(gss$workfast01)

# get to do a number of different things on my job
gss$workdiff <- as.numeric(ifelse(gss$workdiff %in% 1:4, gss$workdiff, NA))
gss$workdiff01 <- (gss$workdiff - 1) / 3
table(gss$workdiff01)

# too much work to do everything well
gss$overwork <- 5 - as.numeric(ifelse(gss$overwork %in% 1:4, gss$overwork, NA))
gss$overwork01 <- (gss$overwork - 1) / 3
table(gss$overwork01)

# treated with respect
gss$respect <- as.numeric(ifelse(gss$respect %in% 1:4, gss$respect, NA))
gss$respect01 <- (gss$respect - 1) / 3
table(gss$respect01)

# trust management
gss$trustman <- as.numeric(ifelse(gss$trustman %in% 1:4, gss$trustman, NA))
gss$trustman01 <- (gss$trustman - 1) / 3
table(gss$trustman01)

# safety of workers is high priority
gss$safetywk <- as.numeric(ifelse(gss$safetywk %in% 1:4, gss$safetywk, NA))
gss$safetywk01 <- (gss$safetywk - 1) / 3
table(gss$safetywk01)

# no shortcuts when worker safety is at stake
gss$safefrst <- as.numeric(ifelse(gss$safefrst %in% 1:4, gss$safefrst, NA))
gss$safefrst01 <- (gss$safefrst - 1) / 3
table(gss$safefrst01)

# employees and management work together for safety
gss$teamsafe <- as.numeric(ifelse(gss$teamsafe %in% 1:4, gss$teamsafe, NA))
gss$teamsafe01 <- (gss$teamsafe - 1) / 3
table(gss$teamsafe01)

# safety and health conditions are good
gss$safehlth <- as.numeric(ifelse(gss$safehlth %in% 1:4, gss$safehlth, NA))
gss$safehlth01 <- (gss$safehlth - 1) / 3
table(gss$safehlth01)

# proud to work for employer
gss$proudemp <- as.numeric(ifelse(gss$proudemp %in% 1:4, gss$proudemp, NA))
gss$proudemp01 <- (gss$proudemp - 1) / 3
table(gss$proudemp01)

# conditions allow me to be productive
gss$prodctiv <- as.numeric(ifelse(gss$prodctiv %in% 1:4, gss$prodctiv, NA))
gss$prodctiv01 <- (gss$prodctiv - 1) / 3
table(gss$prodctiv01)

# how often find work stressful (reverse)
gss$stress <- 6 - as.numeric(ifelse(gss$stress %in% 1:5, gss$stress, NA))
gss$stress01 <- (gss$stress - 1) / 4
table(gss$stress01)

# overall how satisfied with job
gss$satjob <- as.numeric(ifelse(gss$satjob %in% 1:4, gss$satjob, NA))
gss$satjob01 <- (gss$satjob - 1) / 3
table(gss$satjob01)

# workplace is run in smooth and efficient manner
gss$wksmooth <- as.numeric(ifelse(gss$wksmooth %in% 1:4, gss$wksmooth, NA))
gss$wksmooth01 <- (gss$wksmooth - 1) / 3
table(gss$wksmooth01)

# how hard to take off work for family matters
gss$famwkoff <- 5 - as.numeric(ifelse(gss$famwkoff %in% 1:4, gss$famwkoff, NA))
gss$famwkoff01 <- (gss$famwkoff - 1) / 3
table(gss$famwkoff01)

# how often family interfere with demands of work
gss$famvswk <- 5 - as.numeric(ifelse(gss$famvswk %in% 1:4, gss$famvswk, NA))
gss$famvswk01 <- (gss$famvswk - 1) / 3
table(gss$famvswk01)

# how often too few workers to get job done
gss$toofewwk <- 5 - as.numeric(ifelse(gss$toofewwk %in% 1:4, gss$toofewwk, NA))
gss$toofewwk01 <- (gss$toofewwk - 1) / 3
table(gss$toofewwk01)

# chances of promotion are good
gss$promteok <- as.numeric(ifelse(gss$promteok %in% 1:4, gss$promteok, NA))
gss$promteok01 <- (gss$promteok - 1) / 3
table(gss$promteok01)

# opportunity to develop own special abilities
gss$opdevel <- as.numeric(ifelse(gss$opdevel %in% 1:4, gss$opdevel, NA))
gss$opdevel01 <- (gss$opdevel - 1) / 3
table(gss$opdevel01)

# feceive enough help and equipment to get job done
gss$hlpequip <- as.numeric(ifelse(gss$hlpequip %in% 1:4, gss$hlpequip, NA))
gss$hlpequip01 <- (gss$hlpequip - 1) / 3
table(gss$hlpequip01)

# have enough info to get job done
gss$haveinfo <- as.numeric(ifelse(gss$haveinfo %in% 1:4, gss$haveinfo, NA))
gss$haveinfo01 <- (gss$haveinfo - 1) / 3
table(gss$haveinfo01)

# given freedom to decide how to do work
gss$wkfreedm <- as.numeric(ifelse(gss$wkfreedm %in% 1:4, gss$wkfreedm, NA))
gss$wkfreedm01 <- (gss$wkfreedm - 1) / 3
table(gss$wkfreedm01)

# fringe benefits are good
gss$fringeok <- as.numeric(ifelse(gss$fringeok %in% 1:4, gss$fringeok, NA))
gss$fringeok01 <- (gss$fringeok - 1) / 3
table(gss$fringeok01)

# supervisor cares about welfare of those working under
gss$supcares <- as.numeric(ifelse(gss$supcares %in% 1:4, gss$supcares, NA))
gss$supcares01 <- (gss$supcares - 1) / 3
table(gss$supcares01)

# free from conflicting demands of others
gss$condemnd <- as.numeric(ifelse(gss$condemnd %in% 1:4, gss$condemnd, NA))
gss$condemnd01 <- (gss$condemnd - 1) / 3
table(gss$condemnd01)

# promotions are handled fairly
gss$promtefr <- as.numeric(ifelse(gss$promtefr %in% 1:4, gss$promtefr, NA))
gss$promtefr01 <- (gss$promtefr - 1) / 3
table(gss$promtefr01)

# coworkers take personal interest in me
gss$cowrkint <- as.numeric(ifelse(gss$cowrkint %in% 1:4, gss$cowrkint, NA))
gss$cowrkint01 <- (gss$cowrkint - 1) / 3
table(gss$cowrkint01)

# job security is good
gss$jobsecok <- as.numeric(ifelse(gss$jobsecok %in% 1:4, gss$jobsecok, NA))
gss$jobsecok01 <- (gss$jobsecok - 1) / 3
table(gss$jobsecok01)

# supervisor is helpful
gss$suphelp <- as.numeric(ifelse(gss$suphelp %in% 1:4, gss$suphelp, NA))
gss$suphelp01 <- (gss$suphelp - 1) / 3
table(gss$suphelp01)

# have enough time to get the job done
gss$wrktime <- as.numeric(ifelse(gss$wrktime %in% 1:4, gss$wrktime, NA))
gss$wrktime01 <- (gss$wrktime - 1) / 3
table(gss$wrktime01)

# coworkers can be relied upon
gss$cowrkhlp <- as.numeric(ifelse(gss$cowrkhlp %in% 1:4, gss$cowrkhlp, NA))
gss$cowrkhlp01 <- (gss$cowrkhlp - 1) / 3
table(gss$cowrkhlp01)

# relations between management and employees
gss$manvsemp <- as.numeric(ifelse(gss$manvsemp %in% 1:5, gss$manvsemp, NA))
gss$manvsemp01 <- (gss$manvsemp - 1) / 4
table(gss$manvsemp01)

# how fair is what you earn (high = less than deserve)
gss$fairearn <- 6 - as.numeric(ifelse(gss$fairearn %in% 1:5, gss$fairearn, NA))
gss$fairearn01 <- (gss$fairearn - 1) / 4
table(gss$fairearn01)

# access to stress reduction programs at work
gss$strredpg01 <- as.numeric(ifelse(gss$strredpg == 1, 1, ifelse(gss$strredpg == 2, 0, NA)))
table(gss$strredpg01)

# after avg work day, how many hours to relax/do things you enjoy
gss$hrsrelax <- as.numeric(ifelse(gss$hrsrelax %in% 0:24, gss$hrsrelax, NA))
gss$hrsrelax01 <- (gss$hrsrelax) / 24
table(gss$hrsrelax01)
hist(gss$hrsrelax01)

work_vars <- c("hrsrelax01","strredpg01","fairearn01","manvsemp01",
               "cowrkhlp01","wrktime01","suphelp01","jobsecok01","cowrkint01",
               "promtefr01","condemnd01","supcares01","fringeok01","wkfreedm01",
               "haveinfo01","hlpequip01","opdevel01","promteok01","toofewwk01",
               "famvswk01","famwkoff01","wksmooth01","satjob01","stress01",
               "prodctiv01","proudemp01","safehlth01","teamsafe01","safefrst01",
               "safetywk01","trustman01","respect01","overwork01","workdiff01",
               "workfast01","mustwork01","misswork01","hlthdays01","physhlth01",
               "mntlhlth01","depress01","hrs01","wrkslf01","moredays01",
               "wrkhome01","partteam01","knowschd01","supervis01","usetech01",
               "empinput01","secondwk01","yearsjob01")
cor(gss[,work_vars], use = "pairwise.complete.obs")
screeplot(prcomp(cov(gss[,work_vars], 
                     use = "pairwise.complete.obs")))
out <- factanal(covmat = cov(gss[,work_vars], 
                      use = "pairwise.complete.obs"),
         factors = 1, rotation = "promax")
print(out, cutoff = .3)


## personal well-being

# last 30 days, how many days miss work b/c of mental/physical health?
gss$misswork <- as.numeric(ifelse(gss$misswork %in% 0:30, gss$misswork, NA))
gss$misswork01 <- (gss$misswork) / 30
table(gss$misswork01)

# last 30 days, how many mental/physical health? prevent doing typical things
gss$hlthdays <- as.numeric(ifelse(gss$hlthdays %in% 0:30, gss$hlthdays, NA))
gss$hlthdays01 <- (gss$hlthdays) / 30
table(gss$hlthdays01)
hist(gss$hlthdays01)

# general health evaluation (high = good)
gss$health01 <- (4 - as.numeric(ifelse(gss$health %in% 1:4, gss$health, NA))) / 3
gss$health101 <- (5 - as.numeric(ifelse(gss$health1 %in% 1:5, gss$health1, NA))) / 4
gss$genhealth01 <- ifelse(is.na(gss$health101), gss$health01, gss$health101)
gss$genhealth <- gss$genhealth01 * 12
table(gss$genhealth)

# last 30 days, physical health not good
gss$physhlth <- as.numeric(ifelse(gss$physhlth %in% 0:30, gss$physhlth, NA))
gss$physhlth01 <- (gss$physhlth) / 30
table(gss$physhlth01)
hist(gss$physhlth01)

# last 30 days, physical health not good
gss$mntlhlth <- as.numeric(ifelse(gss$mntlhlth %in% 0:30, gss$mntlhlth, NA))
gss$mntlhlth01 <- (gss$mntlhlth) / 30
table(gss$mntlhlth01)
hist(gss$mntlhlth01)

# health official ever said you have depression
gss$depress01 <- as.numeric(ifelse(gss$depress == 1, 1, ifelse(gss$depress == 2, 0, NA)))
table(gss$depress01)


## nature of employment situation

# working full or part time
gss$working <- ifelse(gss$wrkstat %in% 1:2, 1, 0)
table(gss$working)

# unemployed
gss$unemp <- ifelse(gss$wrkstat == 4, 1, 0)
table(gss$unemp) / sum(table(gss$unemp))

# hours worked last week
gss$hrs1 <- as.numeric(ifelse(gss$hrs1 %in% 0:89, gss$hrs1, ifelse(gss$hrs1 == ".i", 0, NA)))
gss$hrs1[gss$hrs1 > 0 & gss$hrs1 <= 20] <- 1
gss$hrs1[gss$hrs1 > 20 & gss$hrs1 <= 40] <- 2
gss$hrs1[gss$hrs1 > 40 & gss$hrs1 <= 60] <- 3
gss$hrs1[gss$hrs1 > 60] <- 4
gss$hrs101 <- gss$hrs1 / 4
table(gss$hrs1)
hist(gss$hrs1)

# self-employed
gss$wrkslf01 <- as.numeric(ifelse(gss$wrkslf == 1, 1, ifelse(gss$wrkslf == 2, 0, NA)))
table(gss$wrkslf01)

# days per month work beyond normal hours
gss$moredays <- as.numeric(ifelse(gss$moredays %in% 0:30, gss$moredays, NA))
gss$moredays01 <- (gss$moredays) / 30
table(gss$moredays01)

# how often work at home
gss$wrkhome <- as.numeric(ifelse(gss$wrkhome %in% 1:6, gss$wrkhome, NA))
gss$wrkhome01 <- (gss$wrkhome - 1) / 5
table(gss$wrkhome01)

# work as team (1) or mostly alone (0)
gss$partteam01 <- as.numeric(ifelse(gss$partteam == 1, 1, ifelse(gss$partteam == 2, 0, NA)))
table(gss$partteam01)

# how far in advance know schedule (higher = more time)
gss$knowschd <- as.numeric(ifelse(gss$knowschd %in% 1:4, gss$knowschd, NA))
gss$knowschd01 <- (gss$knowschd - 1) / 3
table(gss$knowschd01)

# supervises others
gss$supervis01 <- as.numeric(ifelse(gss$supervis == 1, 1, ifelse(gss$supervis == 2, 0, NA)))
table(gss$supervis01)

# % of time use tech
gss$usetech <- as.numeric(ifelse(gss$usetech %in% 0:100, gss$usetech, NA))
gss$usetech01 <- (gss$usetech) / 100
table(gss$usetech01)
hist(gss$usetech01)

# personally involved in group decision making
gss$empinput01 <- as.numeric(ifelse(gss$empinput == 1, 1, ifelse(gss$empinput == 2, 0, NA)))
table(gss$empinput01)

# years worked at current job
gss$yearsjob <- as.numeric(ifelse(gss$yearsjob %in% 0:67, gss$yearsjob, NA))
gss$yearsjob01 <- (gss$yearsjob) / 67
table(gss$yearsjob01)
hist(gss$yearsjob01)

# how much of accumulated skills used at current job (high = more)
gss$useskill <- as.numeric(ifelse(gss$useskill %in% 1:4, gss$useskill, NA))
gss$useskill01 <- (gss$useskill - 1) / 3
table(gss$useskill01)

# has 2nd job
gss$secondwk01 <- as.numeric(ifelse(gss$secondwk == 1, 1, ifelse(gss$secondwk == 2, 0, NA)))
table(gss$secondwk01)


## demographics and personal vars

# class identification (higher = upper)
gss$class <- as.numeric(ifelse(gss$class %in% 1:4, gss$class, NA))
gss$class01 <- (gss$class - 1) / 3
table(gss$class01)

# married
gss$married <- as.numeric(ifelse(gss$marital == 1, 1, 0))
table(gss$married)

# have children
gss$childs <- as.numeric(ifelse(gss$childs %in% 1:8, 1, 0))
table(gss$childs)

# age (89 = 89 or older)
gss$age <- as.numeric(ifelse(gss$age %in% 18:89, gss$age, NA))
gss$age01 <- (gss$age - 18) / (89-18)
table(gss$age01)
hist(gss$age01)

# educational attainment
gss$educ <- as.numeric(ifelse(gss$degree %in% 0:4, gss$degree, NA))
gss$educ01 <- (gss$educ) / 4
table(gss$educ01)

# sex (1 = male)
gss$sex <- as.numeric(ifelse(gss$sex == 1, 1, ifelse(gss$sex == 2, 0, NA)))
table(gss$sex)

# black
gss$black <- ifelse(gss$race == 2, 1, 0)
table(gss$black)

# hispanic (only 2000 and after)
gss$hisp <- ifelse(gss$hispanic %in% 2:50, 1, 0)
table(gss$hisp)

# born in US
gss$born <- as.numeric(ifelse(gss$born == 1, 1, ifelse(gss$born == 2, 0, NA)))
table(gss$born)

# any household members under 6 yrs old
gss$babies <- as.numeric(ifelse(gss$babies %in% 0:6, gss$babies, NA))
gss$babies01 <- ifelse(gss$babies > 0, 1, 0)
table(gss$babies01)

# household income
gss$income86 <- (as.numeric(ifelse(gss$income86 %in% 1:20, gss$income86, NA)) - 1)
gss$income91 <- (as.numeric(ifelse(gss$income91 %in% 1:21, gss$income91, NA)) - 1)
gss$income98 <- (as.numeric(ifelse(gss$income98 %in% 1:23, gss$income98, NA)) - 1)
gss$income06 <- (as.numeric(ifelse(gss$income06 %in% 1:25, gss$income06, NA)) - 1)
gss$income16 <- (as.numeric(ifelse(gss$income16 %in% 1:26, gss$income16, NA)) - 1)
gss$income8601 <- gss$income86 / 19
gss$income9801 <- gss$income98 / 22
gss$income0601 <- gss$income06 / 24
gss$income1601 <- gss$income16 / 25

# self or spouse in union
gss$union <- ifelse(gss$union %in% 1:3, 1, 0)
table(gss$union)

# freq attend religious services
gss$attend <- as.numeric(ifelse(gss$attend %in% 0:8, gss$attend, NA))
gss$attend01 <- gss$attend / 8
table(gss$attend01)

# how often pray
gss$pray <- 7 - as.numeric(ifelse(gss$pray %in% 1:6, gss$pray, NA))
gss$pray01 <- (gss$pray - 1) / 5
table(gss$pray01)

# religiosity
gss$relig01 <- rowMeans(gss[,c("attend01","pray01")], na.rm = T)
table(gss$relig01)
hist(gss$relig01)

# verbal ability score
gss$wordsum <- as.numeric(ifelse(gss$wordsum %in% 0:10, gss$wordsum, NA))
gss$wordsum01 <- (gss$wordsum) / 10
table(gss$wordsum01)


## social and political vars

# political parties, groups, or associations in past 12 months (reverse)
gss$partpart <- 6 - as.numeric(ifelse(gss$partpart %in% 1:5, gss$partpart, NA))
gss$partpart01 <- (gss$partpart - 1) / 4
table(gss$partpart01)

# Voted, among eligible (all Pres from 1988-2016)
gss$vote88 <- ifelse(gss$vote88 == 1, 1, ifelse(gss$vote88 == 2, 0, NA))
gss$vote92 <- ifelse(gss$vote92 == 1, 1, ifelse(gss$vote92 == 2, 0, NA))
gss$vote96 <- ifelse(gss$vote96 == 1, 1, ifelse(gss$vote96 == 2, 0, NA))
gss$vote00 <- ifelse(gss$vote00 == 1, 1, ifelse(gss$vote00 == 2, 0, NA))
gss$vote04 <- ifelse(gss$vote04 == 1, 1, ifelse(gss$vote04 == 2, 0, NA))
gss$vote08 <- ifelse(gss$vote08 == 1, 1, ifelse(gss$vote08 == 2, 0, NA))
gss$vote12 <- ifelse(gss$vote12 == 1, 1, ifelse(gss$vote12 == 2, 0, NA))
gss$vote16 <- ifelse(gss$vote16 == 1, 1, ifelse(gss$vote16 == 2, 0, NA))
gss$voted <- NA
gss$voted[gss$year==1989] <- gss$vote88[gss$year==1989]
gss$voted[gss$year==1998] <- gss$vote96[gss$year==1998]
gss$voted[gss$year==2002] <- gss$vote00[gss$year==2002]
gss$voted[gss$year==2006] <- gss$vote04[gss$year==2006]
gss$voted[gss$year==2010] <- gss$vote08[gss$year==2010]
gss$voted[gss$year==2012] <- gss$vote12[gss$year==2012]
gss$voted[gss$year==2014] <- gss$vote12[gss$year==2014]
gss$voted[gss$year==2016] <- gss$vote16[gss$year==2016]
gss$voted[gss$year==2018] <- gss$vote16[gss$year==2018]


## interest in various issues (reverse)

# international and foreign policy
gss$intintl <- 4 - as.numeric(ifelse(gss$intintl %in% 1:3, gss$intintl, NA))
gss$intintl01 <- (gss$intintl - 1) / 2
table(gss$intintl01)

# agricultural and farm
gss$intfarm <- 4 - as.numeric(ifelse(gss$intfarm %in% 1:3, gss$intfarm, NA))
gss$intfarm01 <- (gss$intfarm - 1) / 2
table(gss$intfarm01)

# local school
gss$inteduc <- 4 - as.numeric(ifelse(gss$inteduc %in% 1:3, gss$inteduc, NA))
gss$inteduc01 <- (gss$inteduc - 1) / 2
table(gss$inteduc01)

# scientific discoveries
gss$intsci <- 4 - as.numeric(ifelse(gss$intsci %in% 1:3, gss$intsci, NA))
gss$intsci01 <- (gss$intsci - 1) / 2
table(gss$intsci01)

# economics and business
gss$intecon <- 4 - as.numeric(ifelse(gss$intecon %in% 1:3, gss$intecon, NA))
gss$intecon01 <- (gss$intecon - 1) / 2
table(gss$intecon01)

# new inventions and technologies
gss$inttech <- 4 - as.numeric(ifelse(gss$inttech %in% 1:3, gss$inttech, NA))
gss$inttech01 <- (gss$inttech - 1) / 2
table(gss$inttech01)

# new medical discoveries
gss$intmed <- 4 - as.numeric(ifelse(gss$intmed %in% 1:3, gss$intmed, NA))
gss$intmed01 <- (gss$intmed - 1) / 2
table(gss$intmed01)

# space exploration
gss$intspace <- 4 - as.numeric(ifelse(gss$intspace %in% 1:3, gss$intspace, NA))
gss$intspace01 <- (gss$intspace - 1) / 2
table(gss$intspace01)

# environmental pollution
gss$intenvir <- 4 - as.numeric(ifelse(gss$intenvir %in% 1:3, gss$intenvir, NA))
gss$intenvir01 <- (gss$intenvir - 1) / 2
table(gss$intenvir01)

# military and defense
gss$intmil <- 4 - as.numeric(ifelse(gss$intmil %in% 1:3, gss$intmil, NA))
gss$intmil01 <- (gss$intmil - 1) / 2
table(gss$intmil01)

# cor structure
int_cormat <- cor(cbind(gss[,c("intintl","intfarm","inteduc","intsci",
                               "intecon","inttech","intmed","intspace",
                               "intenvir","intmil")]), use = "pairwise.complete.obs")
screeplot(prcomp(int_cormat))
factanal(covmat = int_cormat, factors = 1)

# interest in issues scale
gss$int_issues01 <- rowMeans(gss[,c("intintl01","intfarm01","inteduc01","intsci01",
                                    "intecon01","inttech01","intmed01","intspace01",
                                    "intenvir01","intmil01")])
hist(gss$int_issues01)

# hours of TV on a typical day
table(gss$tvhours)
gss$tvhours <- as.numeric(ifelse(gss$tvhours %in% 0:24, gss$tvhours, NA))
gss$tvhours01 <- gss$tvhours / 24
hist(as.numeric(gss$tvhours))
summary(gss$tvhours)

# how often read newspaper (reverse)
gss$news <- 6 - as.numeric(ifelse(gss$news %in% 1:5, gss$news, NA))
gss$news01 <- (gss$news - 1) / 4
table(gss$news01)

# how often last 30 days visit website for political info
gss$pol30 <- as.numeric(ifelse(gss$pol30 %in% 1:4, gss$pol30, NA))
gss$pol3001 <- (gss$pol30 - 1) / 3
table(gss$pol3001)

# last 12 months used internet to find political info
gss$polinf12 <- as.numeric(ifelse(gss$polinf12 == 1, 1, ifelse(gss$polinf12 == 2, 0, NA)))
table(gss$polinf12)

# how often last 12 months discussed politics with other people
gss$poldisgn <- as.numeric(ifelse(gss$poldisgn %in% 0:3, gss$poldisgn, NA))
gss$poldisgn01 <- (gss$poldisgn) / 3
table(gss$poldisgn01)

# how often last 12 months looked for info about politics 
gss$polinfgn <- as.numeric(ifelse(gss$polinfgn %in% 0:3, gss$polinfgn, NA))
gss$polinfgn01 <- (gss$polinfgn) / 3
table(gss$polinfgn01)

# how often last 12 months looked up views of pol candidate
gss$polcangn <- as.numeric(ifelse(gss$polcangn %in% 0:3, gss$polcangn, NA))
gss$polcangn01 <- (gss$polcangn) / 3
table(gss$polcangn01)

# how interested in politics
gss$polint <- 6 - as.numeric(ifelse(gss$polint %in% 1:5, gss$polint, NA))
gss$polint01 <- (gss$polint - 1) / 4
table(gss$polint01)
gss$polint1 <- 5 - as.numeric(ifelse(gss$polint1 %in% 1:4, gss$polint1, NA))
gss$polint101 <- (gss$polint1 - 1) / 3
table(gss$polint101)
gss$polinterest01 <- rowMeans(gss[,c("polint01","polint101")], na.rm = T)
table(gss$polinterest01)

# disagree most people are better informed about politics
gss$poleff15 <- as.numeric(ifelse(gss$poleff15 %in% 1:5, gss$poleff15, NA))
gss$poleff1501 <- (gss$poleff15 - 1) / 4
table(gss$poleff1501)
gss$poleff20 <- as.numeric(ifelse(gss$poleff20 %in% 1:5, gss$poleff20, NA))
gss$poleff2001 <- (gss$poleff20 - 1) / 4
table(gss$poleff2001)
gss$btrinfrmd01 <- rowMeans(gss[,c("poleff1501","poleff2001")], na.rm = T)
table(gss$btrinfrmd01)

# last 12 months participated in political party, group, or association
gss$grppol <- 5 - as.numeric(ifelse(gss$grppol %in% 1:4, gss$grppol, NA))
gss$grppol01 <- (gss$grppol - 1) / 3
table(gss$grppol01)

# last 12 months volunteered for political activities
gss$volwkpol <- as.numeric(ifelse(gss$volwkpol %in% 1:4, gss$volwkpol, NA))
gss$volwkpol01 <- (gss$volwkpol - 1) / 3
table(gss$volwkpol01)

# recency of attendance at political rally
gss$attrally <- 5 - as.numeric(ifelse(gss$attrally %in% 1:4, gss$attrally, NA))
gss$attrally01 <- (gss$attrally - 1) / 3
table(gss$attrally01)

# recency of contact politician
gss$cntctgov <- 5 - as.numeric(ifelse(gss$cntctgov %in% 1:4, gss$cntctgov, NA))
gss$cntctgov01 <- (gss$cntctgov - 1) / 3
table(gss$cntctgov01)

# recency of joining internet political forum
gss$interpol <- 5 - as.numeric(ifelse(gss$interpol %in% 1:4, gss$interpol, NA))
gss$interpol01 <- (gss$interpol - 1) / 3
table(gss$interpol01)

# how often discuss politics with fam, friends, and coworkers
gss$discpol <- 5 - as.numeric(ifelse(gss$discpol %in% 1:4, gss$discpol, NA))
gss$discpol01 <- (gss$discpol - 1) / 3
table(gss$discpol01)

# recency of expressing political views on internet
gss$polinter <- 5 - as.numeric(ifelse(gss$polinter %in% 1:4, gss$polinter, NA))
gss$polinter01 <- (gss$polinter - 1) / 3
table(gss$polinter01)

# how often use media for political news
gss$polnews <- 8 - as.numeric(ifelse(gss$polnews %in% 1:7, gss$polnews, NA))
gss$polnews01 <- (gss$polnews - 1) / 6
table(gss$polnews01)

# good understanding of important issues
gss$poleff13 <- 6 - as.numeric(ifelse(gss$poleff13 %in% 1:5, gss$poleff13, NA))
gss$poleff1301 <- (gss$poleff13 - 1) / 4
table(gss$poleff1301)
gss$poleff19 <- 6 - as.numeric(ifelse(gss$poleff19 %in% 1:5, gss$poleff19, NA))
gss$poleff1901 <- (gss$poleff19 - 1) / 4
table(gss$poleff1901)
gss$gdundrstnd01 <- rowMeans(gss[,c("poleff1301","poleff1901")], na.rm = T)
table(gss$gdundrstnd01)

# recency of signing a petition
gss$signdpet <- 5 - as.numeric(ifelse(gss$signdpet %in% 1:4, gss$signdpet, NA))
gss$signdpet01 <- (gss$signdpet - 1) / 3
table(gss$signdpet01)

# recency of boycotted products
gss$avoidbuy <- 5 - as.numeric(ifelse(gss$avoidbuy %in% 1:4, gss$avoidbuy, NA))
gss$avoidbuy01 <- (gss$avoidbuy - 1) / 3
table(gss$avoidbuy01)

# recency of took part in demonstration
gss$joindem <- 5 - as.numeric(ifelse(gss$joindem %in% 1:4, gss$joindem, NA))
gss$joindem01 <- (gss$joindem - 1) / 3
table(gss$joindem01)

# recency of donated money to social/political activity
gss$polfunds <- 5 - as.numeric(ifelse(gss$polfunds %in% 1:4, gss$polfunds, NA))
gss$polfunds01 <- (gss$polfunds - 1) / 3
table(gss$polfunds01)

# recency of contacted media to express views
gss$usemedia <- 5 - as.numeric(ifelse(gss$usemedia %in% 1:4, gss$usemedia, NA))
gss$usemedia01 <- (gss$usemedia - 1) / 3
table(gss$usemedia01)

# how often try to persuade, fam, friends, coworkers
gss$chngeoth <- 5 - as.numeric(ifelse(gss$chngeoth %in% 1:4, gss$chngeoth, NA))
gss$chngeoth01 <- (gss$chngeoth - 1) / 3
table(gss$chngeoth01)


##### 1989 Analyses #####

# impute income
gss$income8601_i[gss$year==1989] <- ifelse(is.na(gss$income8601[gss$year==1989]),
                                           predict(lm(gss$income8601[gss$year==1989] ~ 
                                                        age01 + sex + black + wordsum01 +
                                                        educ01 + working + hrs101 + class + union +
                                                        married + childs + babies01 + 
                                                        born + relig01, data=subset(gss, year==1989))),
                                           gss$income8601[gss$year==1989]) 

# voter turnout
m_vote89_1 <- glm(voted ~ xhaustn01 +
                    age01 + sex + black + educ01 + income8601_i + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==1989 & working == 1), family=binomial(link="logit"))
write.csv(summary(m_vote89_1)$coefficients, "Tables/GSS_vote89_1.csv", row.names = T)

m_vote89_2 <- glm(voted ~ xhaustn01 + genhealth01 + wordsum01 +
                  age01 + sex + black + educ01 + income8601_i + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==1989 & working == 1), family=binomial(link="logit"))
write.csv(summary(m_vote89_2)$coefficients, "Tables/GSS_vote89_2.csv", row.names = T)

# news
m_news89_1 <- lm(news01 ~ xhaustn01 +
                   age01 + sex + black + educ01 + income8601_i + hrs101 +
                   class + union + married + childs + babies01 + born + relig01, 
                 data=subset(gss, year==1989 & working == 1))
write.csv(summary(m_news89_1)$coefficients, "Tables/GSS_news89_1.csv", row.names = T)

m_news89_2 <- lm(news01 ~ xhaustn01 + genhealth01 + wordsum01 +
                   age01 + sex + black + educ01 + income8601_i + hrs101 +
                   class + married + childs + babies01 + born + relig01, 
                 data=subset(gss, year==1989 & working == 1))
write.csv(summary(m_news89_2)$coefficients, "Tables/GSS_news89_2.csv", row.names = T)


##### 1998 Analyses #####

# impute income
gss$income9801_i[gss$year==1998] <- ifelse(is.na(gss$income9801[gss$year==1998]),
                                           predict(lm(gss$income9801[gss$year==1998] ~ 
                                                        age01 + sex + black +
                                                        educ01 + working + hrs101 + class + union +
                                                        married + childs + babies01 + 
                                                        born + relig01, data=subset(gss, year==1998))),
                                           gss$income9801[gss$year==1998]) 

# voter turnout
m_vote98_1 <- glm(voted ~ xhaustn01 + genhealth01 + wordsum01 +
                    age01 + sex + black + educ01 + income9801_i + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==1998 & working == 1), family=binomial(link="logit"))
write.csv(summary(m_vote98_1)$coefficients, "Tables/GSS_vote98_1.csv", row.names = T)

# news
m_news98_2 <- lm(news01 ~ xhaustn01 + genhealth01 + wordsum01 +
                   age01 + sex + black + educ01 + income9801_i + hrs101 +
                   class + married + childs + babies01 + born + relig01, 
                 data=subset(gss, year==1998 & working == 1))
write.csv(summary(m_vote98_2)$coefficients, "Tables/GSS_vote98_2.csv", row.names = T)


##### 2002 Analyses #####

# impute income
gss$income0201_i[gss$year==2002] <- ifelse(is.na(gss$income9801[gss$year==2002]),
                                           predict(lm(gss$income9801[gss$year==2002] ~ 
                                                        age01 + sex + black + hisp +
                                                        educ01 + working + hrs101 + class + union +
                                                        married + childs + babies01 + 
                                                        born + relig01, data=subset(gss, year==2002))),
                                           gss$income9801[gss$year==2002]) 

# political activity index
gss$polact01 <- rowMeans(gss[,c("pol3001","polinf12","poldisgn01","polinfgn01","polcangn01")], na.rm = T)

# voter turnout
m_vote02_1 <- glm(voted ~ tiredwrk01 + genhealth01 +
                    age01 + sex + black + hisp + educ01 + income0201_i + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==2002 & working == 1), family=binomial(link="logit"))
write.csv(summary(m_vote02_1)$coefficients, "Tables/GSS_vote02_1.csv", row.names = T)

# political activities
m_act02_1 <- lm(polact01 ~ tiredwrk01 + genhealth01 +
                    age01 + sex + black + hisp + educ01 + income0201_i + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==2002 & working == 1))
write.csv(summary(m_act02_1)$coefficients, "Tables/GSS_act02_1.csv", row.names = T)


##### 2006 Analyses #####

# impute income
gss$income0601_i[gss$year==2006] <- ifelse(is.na(gss$income0601[gss$year==2006]),
                                           predict(lm(gss$income0601[gss$year==2006] ~ 
                                                        age01 + sex + black + hisp +
                                                        educ01 + working + hrs101 + class + union +
                                                        married + childs + babies01 + 
                                                        born + relig01, data=subset(gss, year==2006))),
                                           gss$income0601[gss$year==2006]) 

# voter turnout
m_vote06_1 <- glm(voted ~ xhaustn01 + genhealth01 +
                    age01 + sex + black + hisp + educ01 + income0601_i + hrs101 +
                    union + married + childs + babies01 + relig01 , 
                  data=subset(gss, year==2006 & working == 1), family=binomial(link="logit"))
write.csv(summary(m_vote06_1)$coefficients, "Tables/GSS_vote06_1.csv", row.names = T)

# news
m_news06_1 <- lm(news01 ~ xhaustn01 + genhealth01 +
                   age01 + sex + black + hisp + educ01 + income0601_i + hrs101 +
                   union + married + childs + babies01 + relig01 , 
                 data=subset(gss, year==2006 & working == 1))
write.csv(summary(m_news06_1)$coefficients, "Tables/GSS_news06_1.csv", row.names = T)

# interest in politics
m_int06_1 <- lm(polint01 ~ xhaustn01 + genhealth01 +
                  age01 + sex + black + hisp + educ01 + income0601_i + hrs101 +
                  union + married + childs + babies01 + relig01 , 
                data=subset(gss, year==2006 & working == 1))
write.csv(summary(m_int06_1)$coefficients, "Tables/GSS_int06_1.csv", row.names = T)


##### 2010 Analyses #####

# impute income
gss$income1001_i[gss$year==2010] <- ifelse(is.na(gss$income0601[gss$year==2010]),
                                           predict(lm(gss$income0601[gss$year==2010] ~ 
                                                        age01 + sex + black + hisp +
                                                        educ01 + working + hrs101 + class + union +
                                                        married + childs + babies01 + 
                                                        born + relig01, data=subset(gss, year==2010))),
                                           gss$income0601[gss$year==2010]) 

# voter turnout
m_vote10_1 <- glm(voted ~ slpprblm01 + genhealth01 +
                    age01 + sex + black + educ01 + income1001_i + working + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==2010), family=binomial(link="logit"))
write.csv(summary(m_vote10_1)$coefficients, "Tables/GSS_vote10_1.csv", row.names = T)

m_vote10_2 <- glm(voted ~ slpprblm01 + genhealth01 + wordsum01 +
                    age01 + sex + black + educ01 + income1001_i + working + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==2010), family=binomial(link="logit"))
write.csv(summary(m_vote10_2)$coefficients, "Tables/GSS_vote10_2.csv", row.names = T)

# news
m_news10_1 <- lm(news01 ~ slpprblm01 + genhealth01 + wordsum01 +
                   age01 + sex + black + educ01 + income1001_i + working + hrs101 +
                   class + union + married + childs + babies01 + born + relig01, 
                 data=subset(gss, year==2010))
write.csv(summary(m_news10_1)$coefficients, "Tables/GSS_news10_1.csv", row.names = T)

# interest in issues
m_iss10_1 <- lm(int_issues01 ~ slpprblm01 + genhealth01 +
                  age01 + sex + black + educ01 + income1001_i + working + hrs101 +
                  class + union + married + childs + babies01 + born + relig01, 
                data=subset(gss, year==2010))
write.csv(summary(m_iss10_1)$coefficients, "Tables/GSS_iss10_1.csv", row.names = T)

m_iss10_2 <- lm(int_issues01 ~ slpprblm01 + genhealth01 + wordsum01 +
                  age01 + sex + black + educ01 + income1001_i + working + hrs101 +
                  class + married + childs + babies01 + born + relig01, 
                data=subset(gss, year==2010))
write.csv(summary(m_iss10_2)$coefficients, "Tables/GSS_iss10_2.csv", row.names = T)

##### 2012 Analyses #####

# impute income
gss$income1201_i[gss$year==2012] <- ifelse(is.na(gss$income0601[gss$year==2012]),
                                           predict(lm(gss$income0601[gss$year==2012] ~ 
                                                        age01 + sex + black + hisp +
                                                        educ01 + working + hrs101 + class + union +
                                                        married + childs + babies01 + 
                                                        born + relig01, data=subset(gss, year==2012))),
                                           gss$income0601[gss$year==2012]) 

# interest in issues
m_iss12_1 <- lm(int_issues01 ~ tiredwk101 + wordsum01 +
                  age01 + sex + black + hisp + educ01 + income1201_i + hrs101 +
                  class + union + married + childs + babies01 + born + relig01, 
                data=subset(gss, year==2012 & working == 1))
write.csv(summary(m_iss12_1)$coefficients, "Tables/GSS_iss12_1.csv", row.names = T)

# news
m_news12_1 <- lm(news01 ~ tiredwk101 + wordsum01 +
                   age01 + sex + black + hisp + educ01 + income1201_i + hrs101 +
                   class + union + married + childs + babies01 + born + relig01, 
                 data=subset(gss, year==2012 & working == 1))
write.csv(summary(m_news12_1)$coefficients, "Tables/GSS_news12_1.csv", row.names = T)

m_news12_2 <- lm(news01 ~ tiredwk101 + wordsum01 + genhealth01 +
                   age01 + sex + black + hisp + educ01 + income1201_i + hrs101 +
                   class + married + childs + babies01 + born + relig01, 
                 data=subset(gss, year==2012 & working == 1))
write.csv(summary(m_news12_2)$coefficients, "Tables/GSS_news12_2.csv", row.names = T)


##### 2014 Analyses #####

# impute income
gss$income1401_i[gss$year==2014] <- ifelse(is.na(gss$income0601[gss$year==2014]),
                                           predict(lm(gss$income0601[gss$year==2014] ~ 
                                                        age01 + sex + black + hisp +
                                                        educ01 + working + hrs101 + class + union +
                                                        married + childs + babies01 + 
                                                        born + relig01, data=subset(gss, year==2014))),
                                           gss$income0601[gss$year==2014]) 

# news
gss$news1401 <- rowMeans(gss[,c("news01","polnews01")])

# political activities
gss$polact1401 <- rowMeans(gss[,c("attrally01","cntctgov01","discpol01",
                                  "polinter01","signdpet01","avoidbuy01",
                                  "joindem01","polfunds01","usemedia01",
                                  "chngeoth01")])

# voter turnout
m_vote14_1 <- glm(voted ~ slpprblm01 + genhealth01 +
                    age01 + sex + black + educ01 + income1401_i + working + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==2014), family=binomial(link="logit"))
write.csv(summary(m_vote14_1)$coefficients, "Tables/GSS_vote14_1.csv", row.names = T)

m_vote14_2 <- glm(voted ~ slpprblm01 + genhealth01 + wordsum01 +
                    age01 + sex + black + educ01 + income1401_i + working + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==2014), family=binomial(link="logit"))
write.csv(summary(m_vote14_2)$coefficients, "Tables/GSS_vote14_2.csv", row.names = T)

# interest in politics
m_int14_1 <- lm(polint101 ~ slpprblm01 + genhealth01 + wordsum01 +
                age01 + sex + black + educ01 + income1401_i + working + hrs101 +
                  class + union + married + childs + babies01 + born + relig01, 
                data=subset(gss, year==2014))
write.csv(summary(m_int14_1)$coefficients, "Tables/GSS_int14_1.csv", row.names = T)

# news
m_news14_1 <- lm(news1401 ~ slpprblm01 + genhealth01 + wordsum01 +
                   age01 + sex + black + educ01 + income1401_i + working + hrs101 +
                   class + union + married + childs + babies01 + born + relig01, 
                 data=subset(gss, year==2014))
write.csv(summary(m_news14_1)$coefficients, "Tables/GSS_news14_1.csv", row.names = T)

# interest in issues
m_iss14_1 <- lm(int_issues01 ~ slpprblm01 + genhealth01 +
                  age01 + sex + black + educ01 + income1401_i + working + hrs101 +
                  class + union + married + childs + babies01 + born + relig01, 
                data=subset(gss, year==2014))
write.csv(summary(m_iss14_1)$coefficients, "Tables/GSS_iss14_1.csv", row.names = T)

m_iss14_2 <- lm(int_issues01 ~ slpprblm01 + genhealth01 + wordsum01 +
                  age01 + sex + black + educ01 + income1401_i + working + hrs101 +
                  class + union + married + childs + babies01 + born + relig01, 
                data=subset(gss, year==2014))
write.csv(summary(m_iss14_2)$coefficients, "Tables/GSS_iss14_2.csv", row.names = T)

# political activities
m_act14_1 <- lm(polact1401 ~ slpprblm01 + genhealth01 + 
                  age01 + sex + black + educ01 + income1401_i + working + hrs101 +
                  class + union + married + childs + babies01 + born + relig01, 
                data=subset(gss, year==2014))
write.csv(summary(m_act14_1)$coefficients, "Tables/GSS_act14_1.csv", row.names = T)

m_act14_2 <- lm(polact1401 ~ slpprblm01 + genhealth01 + wordsum01 +
                  age01 + sex + black + educ01 + income1401_i + working + hrs101 +
                  class + union + married + childs + babies01 + born + relig01, 
                data=subset(gss, year==2014))
write.csv(summary(m_act14_2)$coefficients, "Tables/GSS_act14_2.csv", row.names = T)


##### 2016 Analyses #####

# impute income
gss$income1601_i[gss$year==2016] <- ifelse(is.na(gss$income1601[gss$year==2016]),
                                           predict(lm(gss$income1601[gss$year==2016] ~ 
                                                        age01 + sex + black + hisp +
                                                        educ01 + working + hrs101 + class + union +
                                                        married + childs + babies01 + 
                                                        born + relig01, data=subset(gss, year==2016))),
                                           gss$income1601[gss$year==2016]) 

# news
m_news16_1 <- lm(news01 ~ xhaustn01 + wordsum01 +
                   age01 + sex + black + hisp + educ01 + income1601_i + hrs101 +
                   class + union + married + childs + babies01 + born + relig01, 
                 data=subset(gss, year==2016 & working == 1))
write.csv(summary(m_news16_1)$coefficients, "Tables/GSS_news16_1.csv", row.names = T)


##### 2018 Analyses #####

# impute income
gss$income1801_i[gss$year==2018] <- ifelse(is.na(gss$income1601[gss$year==2018]),
                                           predict(lm(gss$income1601[gss$year==2018] ~ 
                                                        age01 + sex + black + hisp +
                                                        educ01 + working + hrs101 + class + union +
                                                        married + childs + babies01 + 
                                                        born + relig01, data=subset(gss, year==2018))),
                                           gss$income1601[gss$year==2018])

# voter turnout
m_vote18_1 <- glm(voted ~ fatigue01 +
                    age01 + sex + black + hisp + educ01 + income1801_i + working + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==2018), family=binomial(link="logit"))
write.csv(summary(m_vote18_1)$coefficients, "Tables/GSS_vote18_1.csv", row.names = T)

m_vote18_2 <- glm(voted ~ fatigue01 + slpprblm01 + genhealth01 + wordsum01 +
                    age01 + sex + black + hisp + educ01 + income1801_i + working + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==2018), family=binomial(link="logit"))
write.csv(summary(m_vote18_2)$coefficients, "Tables/GSS_vote18_2.csv", row.names = T)

# interest in issues
m_iss18_1 <- lm(int_issues01 ~ fatigue01 +
                  age01 + sex + black + hisp + educ01 + income1801_i + working + hrs101 +
                  class + union + married + childs + babies01 + born + relig01, 
                data=subset(gss, year==2018))
write.csv(summary(m_iss18_1)$coefficients, "Tables/GSS_iss18_1.csv", row.names = T)

m_iss18_2 <- glm(int_issues01 ~ fatigue01 + slpprblm01 + genhealth01 + wordsum01 +
                   age01 + sex + black + hisp + educ01 + income1801_i + working + hrs101 +
                   class + union + married + childs + babies01 + born + relig01, 
                 data=subset(gss, year==2018))
write.csv(summary(m_iss18_2)$coefficients, "Tables/GSS_vote18_2.csv", row.names = T)

# news consumption
m_news18_1 <- lm(news01 ~ fatigue01 +
                   age01 + sex + black + hisp + educ01 + income1801_i + working + hrs101 +
                   class + union + married + childs + babies01 + born + relig01, 
                 data=subset(gss, year==2018))
write.csv(summary(m_news18_1)$coefficients, "Tables/GSS_news18_1.csv", row.names = T)

m_news18_2 <- glm(news01 ~ fatigue01 + slpprblm01 + genhealth01 + wordsum01 +
                    age01 + sex + black + hisp + educ01 + income1801_i + working + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==2018))
write.csv(summary(m_news18_2)$coefficients, "Tables/GSS_news18_2.csv", row.names = T)

# participate in parties, groups
m_part18_1 <- lm(partpart01 ~ fatigue01 +
                   age01 + sex + black + hisp + educ01 + income1801_i + working + hrs101 +
                   class + union + married + childs + babies01 + born + relig01, 
                 data=subset(gss, year==2018))
write.csv(summary(m_part18_1)$coefficients, "Tables/GSS_part18_1.csv", row.names = T)

m_part18_2 <- glm(partpart01 ~ fatigue01 + slpprblm01 + genhealth01 + wordsum01 +
                    age01 + sex + black + hisp + educ01 + income1801_i + working + hrs101 +
                    class + union + married + childs + babies01 + born + relig01, 
                  data=subset(gss, year==2018))
write.csv(summary(m_part18_2)$coefficients, "Tables/GSS_part18_2.csv", row.names = T)

## Pooled analysis (polled across years by IV) 

# Pool incomes (including imputed incomes)
gss$pooled_income[gss$year==1989] <- gss$income8601_i[gss$year==1989] 
gss$pooled_income[gss$year==1998] <- gss$income9801_i[gss$year==1998]
gss$pooled_income[gss$year==2006] <- gss$income0601_i[gss$year==2006]
gss$pooled_income[gss$year==2010] <- gss$income1001_i[gss$year==2010]
gss$pooled_income[gss$year==2014] <- gss$income1401_i[gss$year==2014] 
gss$pooled_income[gss$year==2016] <- gss$income1601_i[gss$year==2016]
gss$pooled_income[gss$year==2018] <- gss$income1801_i[gss$year==2018]

### Exhausted

# voted
pooled_voting_exhaustion <- glm(voted ~ xhaustn01 + genhealth01 +
                                  age01 + sex + black + educ01 + pooled_income + hrs101 +
                                  union + married + childs + babies01 + relig01 + as.factor(year) , 
                                data=subset(gss, year %in% c(1989, 1998, 2006) & working == 1), family=binomial(link="logit"))

write.csv(summary(pooled_voting_exhaustion)$coefficients, "Tables/GSS_polled_voting_exhaustion.csv", row.names = T)


# news
pooled_news_exhaustion <- lm(news01 ~ xhaustn01 +
                               age01 + sex + black + educ01 + pooled_income + hrs101 +
                               union + married + childs + babies01 + relig01 +
                               as.factor(year), 
                             data=subset(gss, year %in% c(1989, 1998, 2006, 2016) & working == 1))

write.csv(summary(pooled_news_exhaustion)$coefficients, "Tables/GSS_polled_news_exhaustion.csv", row.names = T)

### Sleep problems
# voter turnout
pooled_voting_sleep<- glm(voted ~ slpprblm01 + genhealth01 + wordsum01 +
                            age01 + sex + black + educ01 + pooled_income + working + hrs101 +
                            class + union + married + childs + babies01 + born + relig01 +
                            as.factor(year), 
                          data=subset(gss, year %in% c(2010,2014,2018)), family=binomial(link="logit"))

write.csv(summary(pooled_voting_sleep)$coefficients, "Tables/GSS_pooled_voting_sleep.csv", row.names = T)



# interest in issues
pooled_issues <- lm(int_issues01 ~ slpprblm01 + genhealth01 + wordsum01 +
                      age01 + sex + black + educ01 + pooled_income + working + hrs101 +
                      class + married + childs + babies01 + born + relig01 +
                      as.factor(year), 
                    data=subset(gss, year %in% c(2010,2014,2018)))

write.csv(summary(pooled_issues)$coefficients, "Tables/GSS_pooled_issues_sleep.csv", row.names = T)

# news consumption
pooled_news_sleep <- lm(news01 ~ slpprblm01 + genhealth01 + wordsum01 +
                          age01 + sex + black + educ01 + pooled_income + working + hrs101 +
                          class + union + married + childs + babies01 + born + relig01  +
                          as.factor(year), 
                        data=subset(gss, year %in% c(2010,2014,2018)))

write.csv(summary(pooled_news_sleep)$coefficients, "Tables/GSS_pooled_news_sleep.csv", row.names = T)









