##### Data cleaning #####

# Read in Lucid raw data
lucid_raw <- read.csv("Data/Fatigue+Replication_November+11,+2020_14.54.csv", header = T, stringsAsFactors = F)
summary(lucid_raw)

# Read-in Lucid reports
lucid_report_1 <- read.csv("Data/DataAnalysis_8909577_Cognitive_Fatigue_Fall2020_2020-11-11_1318.csv", header = T)
lucid_report_2 <- read.csv("Data/DataAnalysis_8962551_Fatigue_Replication_2_2020-11-11_1319.csv", header = T)
colnames(lucid_report_1)[1] <- colnames(lucid_report_2)[1] <- "rid"
lucid_report_1$rid <- tolower(lucid_report_1$rid) # Qualtrics rid is lower-case
lucid_report_2$rid <- tolower(lucid_report_2$rid)

# Merge reports with Lucid data
lucid_merge_1 <- merge(lucid_raw, lucid_report_1[,c("rid","PID","ResponseStatus","ClientStatus")], by = "rid", all.x = T)
lucid_merge_2 <- merge(lucid_raw, lucid_report_2[,c("rid","PID","ResponseStatus","ClientStatus")], by = "rid", all.x = T)
lucid <- rbind(lucid_merge_1, lucid_merge_2)

# Remove experimenter test cases
lucid <- subset(lucid, PID != "test")

# Write dataset that includes incomplete cases
write.csv(lucid, "Data/Lucid_All.csv", row.names = F)

# Check for duplicate RIDs and remove them
dups <- which(duplicated(lucid$rid))
dups_rid <- lucid[dups,"rid"]
for (i in 1:length(dups_rid)){
  lucid <- subset(lucid, rid != dups_rid[i])
}
anyDuplicated(lucid)

# Keep only completes
lucid <- subset(lucid, ResponseStatus == "Complete")
table(lucid$ResponseStatus) # 1,735 completes

##### Variable coding and transformation ####

# Political engagement (avg of interest and knowledge)
lucid$kn1 <- ifelse(lucid$know1 == 1, 1, ifelse(is.na(lucid$know1), NA, 0))
lucid$kn2 <- ifelse(lucid$know2 == 3, 1, ifelse(is.na(lucid$know2), NA, 0))
lucid$kn3 <- ifelse(lucid$know3 == 3, 1, ifelse(is.na(lucid$know3), NA, 0))
lucid$kn4 <- ifelse(lucid$know4 == 2, 1, ifelse(is.na(lucid$know4), NA, 0))
lucid$kn5 <- ifelse(lucid$know5 == 2, 1, ifelse(is.na(lucid$know5), NA, 0))
lucid$kn01 <- (lucid$kn1 + lucid$kn2 + lucid$kn3 + lucid$kn4 + lucid$kn5)/5
lucid$int01 <- (lucid$interest - 1)/3
lucid$eng01 <- rowMeans(lucid[,c("kn01","int01")])

# Gender
lucid$male <- ifelse(lucid$gender == 1, 1, 0)

# Black or African-American
lucid$black <- ifelse(is.na(lucid$race_2), 0, 1)

# Hispanic or Latino
lucid$hisp <- ifelse(is.na(lucid$race_3), 0, 1)

# Educational attainment
lucid$educ01 <- (lucid$educ - 1)/5

# Unemployed
lucid$unemp <- ifelse(lucid$employ == 3, 1, 0)

# Age
lucid$age <- 2020 - lucid$born
lucid$age01 <- (lucid$age - 18)/(max(lucid$age, na.rm = T) - 18)

# Income
lucid$income01 <- (lucid$income - 1)/12

# Partisanship
lucid$pid <- rowSums(lucid[,c("dstr","rstr","lean")], na.rm = T)
lucid$pid[lucid$pid == 0] <- NA
lucid$pid01 <- (lucid$pid - 1)/6

# Partisan strength
lucid$pidstr01 <- abs(lucid$pid - 4)/3

# Ideological identity
lucid$ideo01 <- (lucid$ideo - 1)/6

# Ideo strength
lucid$ideostr01 <- abs(lucid$ideo - 4)/3

# Experiment indicator
lucid$exp <- ifelse(
  lucid$treatment == "count_control" | lucid$treatment == "count_treat", "symbol", 
  ifelse(
    lucid$treatment == "write_control" | lucid$treatment == "write_treat", "sentence", 
    NA))
table(lucid$exp)

# Treatment indicator
lucid$fatigue <- ifelse(
  lucid$treatment == "count_control" | lucid$treatment == "write_control", "control", 
  ifelse(
    lucid$treatment == "count_treat" | lucid$treatment == "write_treat", "treated", 
    NA))
table(lucid$fatigue)

# Manipulation check sentence writing
lucid$SC_MC_fatigue_r <- 8 - lucid$SC_MC_fatigue
lucid$manip01 <- ifelse(lucid$exp == "sentence", (5 - rowMeans(lucid[,c("SW_MC_1","SW_MC_2")], na.rm = T))/4,
                      (rowMeans(lucid[,c("SC_MC_1","SC_MC_2","SC_MC_3","SC_MC_fatigue_r")], na.rm = T) - 1)/6)
cor(lucid[,c("SC_MC_4","SC_MC_1","SC_MC_2","SC_MC_3","SC_MC_fatigue_r")], use = "complete.obs")
cor(lucid[,c("SW_MC_1","SW_MC_2")], use = "complete.obs")

# Media choice DV (in original vars, 1=chose A, 2=chose B)
lucid$choice_A_1 <- ifelse(lucid$exp == "sentence", 2-lucid$omchoice_1, 2-lucid$nmchoice_1)
lucid$choice_A_2 <- ifelse(lucid$exp == "sentence", 2-lucid$omchoice_2, 2-lucid$nmchoice_2)
lucid$choice_A_3 <- ifelse(lucid$exp == "sentence", 2-lucid$omchoice_3, 2-lucid$nmchoice_3)
lucid$choice_A_4 <- ifelse(lucid$exp == "sentence", 2-lucid$omchoice_4, 2-lucid$nmchoice_4)
lucid$choice_A_5 <- ifelse(lucid$exp == "sentence", 2-lucid$omchoice_5, 2-lucid$nmchoice_5)
lucid$choice_B_1 <- ifelse(lucid$exp == "sentence", lucid$omchoice_1-1, lucid$nmchoice_1-1)
lucid$choice_B_2 <- ifelse(lucid$exp == "sentence", lucid$omchoice_2-1, lucid$nmchoice_2-1)
lucid$choice_B_3 <- ifelse(lucid$exp == "sentence", lucid$omchoice_3-1, lucid$nmchoice_3-1)
lucid$choice_B_4 <- ifelse(lucid$exp == "sentence", lucid$omchoice_4-1, lucid$nmchoice_4-1)
lucid$choice_B_5 <- ifelse(lucid$exp == "sentence", lucid$omchoice_5-1, lucid$nmchoice_5-1)

# Indicator for politics genre
lucid$politics_A_1 <- ifelse(grepl("Politics", lucid$traits1a) == T, 1, 0)
lucid$politics_A_2 <- ifelse(grepl("Politics", lucid$traits2a) == T, 1, 0)
lucid$politics_A_3 <- ifelse(grepl("Politics", lucid$traits3a) == T, 1, 0)
lucid$politics_A_4 <- ifelse(grepl("Politics", lucid$traits4a) == T, 1, 0)
lucid$politics_A_5 <- ifelse(grepl("Politics", lucid$traits5a) == T, 1, 0)
lucid$politics_B_1 <- ifelse(grepl("Politics", lucid$traits1b) == T, 1, 0)
lucid$politics_B_2 <- ifelse(grepl("Politics", lucid$traits2b) == T, 1, 0)
lucid$politics_B_3 <- ifelse(grepl("Politics", lucid$traits3b) == T, 1, 0)
lucid$politics_B_4 <- ifelse(grepl("Politics", lucid$traits4b) == T, 1, 0)
lucid$politics_B_5 <- ifelse(grepl("Politics", lucid$traits5b) == T, 1, 0)

# Indicator for hard political topic
lucid$hard_A_1 <- ifelse(grepl("Federal budget deficit", lucid$traits1a) == T |
                         grepl("Reforming tax code", lucid$traits1a) == T |
                         grepl("Energy policy", lucid$traits1a) == T |
                         grepl("Trump vs Biden on reforming the tax code", lucid$traits1a) == T |
                         grepl("Trump vs Biden on energy policy", lucid$traits1a) == T |
                         grepl("Trump vs Biden on foreign policy", lucid$traits1a) == T, 1, 0)
lucid$hard_A_2 <- ifelse(grepl("Federal budget deficit", lucid$traits2a) == T |
                           grepl("Reforming tax code", lucid$traits2a) == T |
                           grepl("Energy policy", lucid$traits2a) == T |
                           grepl("Trump vs Biden on reforming the tax code", lucid$traits2a) == T |
                           grepl("Trump vs Biden on energy policy", lucid$traits2a) == T |
                           grepl("Trump vs Biden on foreign policy", lucid$traits2a) == T, 1, 0)
lucid$hard_A_3 <- ifelse(grepl("Federal budget deficit", lucid$traits3a) == T |
                           grepl("Reforming tax code", lucid$traits3a) == T |
                           grepl("Energy policy", lucid$traits3a) == T |
                           grepl("Trump vs Biden on reforming the tax code", lucid$traits3a) == T |
                           grepl("Trump vs Biden on energy policy", lucid$traits3a) == T |
                           grepl("Trump vs Biden on foreign policy", lucid$traits3a) == T, 1, 0)
lucid$hard_A_4 <- ifelse(grepl("Federal budget deficit", lucid$traits4a) == T |
                           grepl("Reforming tax code", lucid$traits4a) == T |
                           grepl("Energy policy", lucid$traits4a) == T |
                           grepl("Trump vs Biden on reforming the tax code", lucid$traits4a) == T |
                           grepl("Trump vs Biden on energy policy", lucid$traits4a) == T |
                           grepl("Trump vs Biden on foreign policy", lucid$traits4a) == T, 1, 0)
lucid$hard_A_5 <- ifelse(grepl("Federal budget deficit", lucid$traits5a) == T |
                           grepl("Reforming tax code", lucid$traits5a) == T |
                           grepl("Energy policy", lucid$traits5a) == T |
                           grepl("Trump vs Biden on reforming the tax code", lucid$traits5a) == T |
                           grepl("Trump vs Biden on energy policy", lucid$traits5a) == T |
                           grepl("Trump vs Biden on foreign policy", lucid$traits5a) == T, 1, 0)
lucid$hard_B_1 <- ifelse(grepl("Federal budget deficit", lucid$traits1b) == T |
                           grepl("Reforming tax code", lucid$traits1b) == T |
                           grepl("Energy policy", lucid$traits1b) == T |
                           grepl("Trump vs Biden on reforming the tax code", lucid$traits1b) == T |
                           grepl("Trump vs Biden on energy policy", lucid$traits1b) == T |
                           grepl("Trump vs Biden on foreign policy", lucid$traits1b) == T, 1, 0)
lucid$hard_B_2 <- ifelse(grepl("Federal budget deficit", lucid$traits2b) == T |
                           grepl("Reforming tax code", lucid$traits2b) == T |
                           grepl("Energy policy", lucid$traits2b) == T |
                           grepl("Trump vs Biden on reforming the tax code", lucid$traits2b) == T |
                           grepl("Trump vs Biden on energy policy", lucid$traits2b) == T |
                           grepl("Trump vs Biden on foreign policy", lucid$traits2b) == T, 1, 0)
lucid$hard_B_3 <- ifelse(grepl("Federal budget deficit", lucid$traits3b) == T |
                           grepl("Reforming tax code", lucid$traits3b) == T |
                           grepl("Energy policy", lucid$traits3b) == T |
                           grepl("Trump vs Biden on reforming the tax code", lucid$traits3b) == T |
                           grepl("Trump vs Biden on energy policy", lucid$traits3b) == T |
                           grepl("Trump vs Biden on foreign policy", lucid$traits3b) == T, 1, 0)
lucid$hard_B_4 <- ifelse(grepl("Federal budget deficit", lucid$traits4b) == T |
                           grepl("Reforming tax code", lucid$traits4b) == T |
                           grepl("Energy policy", lucid$traits4b) == T |
                           grepl("Trump vs Biden on reforming the tax code", lucid$traits4b) == T |
                           grepl("Trump vs Biden on energy policy", lucid$traits4b) == T |
                           grepl("Trump vs Biden on foreign policy", lucid$traits4b) == T, 1, 0)
lucid$hard_B_5 <- ifelse(grepl("Federal budget deficit", lucid$traits5b) == T |
                           grepl("Reforming tax code", lucid$traits5b) == T |
                           grepl("Energy policy", lucid$traits5b) == T |
                           grepl("Trump vs Biden on reforming the tax code", lucid$traits5b) == T |
                           grepl("Trump vs Biden on energy policy", lucid$traits5b) == T |
                           grepl("Trump vs Biden on foreign policy", lucid$traits5b) == T, 1, 0)

# Indicator for easy political topic
lucid$easy_A_1 <- ifelse(grepl("Gay marriage", lucid$traits1a) == T |
                           grepl("2020 Presidential race", lucid$traits1a) == T |
                           grepl("Congressional scandals", lucid$traits1a) == T |
                           grepl("Outrageous tweets by Trump and Biden", lucid$traits1a) == T |
                           grepl("Celebrity endorsements of Trump and Biden", lucid$traits1a) == T |
                           grepl("Election jokes from late night TV", lucid$traits1a) == T, 1, 0)
lucid$easy_A_2 <- ifelse(grepl("Gay marriage", lucid$traits2a) == T |
                           grepl("2020 Presidential race", lucid$traits2a) == T |
                           grepl("Congressional scandals", lucid$traits2a) == T |
                           grepl("Outrageous tweets by Trump and Biden", lucid$traits2a) == T |
                           grepl("Celebrity endorsements of Trump and Biden", lucid$traits2a) == T |
                           grepl("Election jokes from late night TV", lucid$traits2a) == T, 1, 0)
lucid$easy_A_3 <- ifelse(grepl("Gay marriage", lucid$traits3a) == T |
                           grepl("2020 Presidential race", lucid$traits3a) == T |
                           grepl("Congressional scandals", lucid$traits3a) == T |
                           grepl("Outrageous tweets by Trump and Biden", lucid$traits3a) == T |
                           grepl("Celebrity endorsements of Trump and Biden", lucid$traits3a) == T |
                           grepl("Election jokes from late night TV", lucid$traits3a) == T, 1, 0)
lucid$easy_A_4 <- ifelse(grepl("Gay marriage", lucid$traits4a) == T |
                           grepl("2020 Presidential race", lucid$traits4a) == T |
                           grepl("Congressional scandals", lucid$traits4a) == T |
                           grepl("Outrageous tweets by Trump and Biden", lucid$traits4a) == T |
                           grepl("Celebrity endorsements of Trump and Biden", lucid$traits4a) == T |
                           grepl("Election jokes from late night TV", lucid$traits4a) == T, 1, 0)
lucid$easy_A_5 <- ifelse(grepl("Gay marriage", lucid$traits5a) == T |
                           grepl("2020 Presidential race", lucid$traits5a) == T |
                           grepl("Congressional scandals", lucid$traits5a) == T |
                           grepl("Outrageous tweets by Trump and Biden", lucid$traits5a) == T |
                           grepl("Celebrity endorsements of Trump and Biden", lucid$traits5a) == T |
                           grepl("Election jokes from late night TV", lucid$traits5a) == T, 1, 0)
lucid$easy_B_1 <- ifelse(grepl("Gay marriage", lucid$traits1b) == T |
                           grepl("2020 Presidential race", lucid$traits1b) == T |
                           grepl("Congressional scandals", lucid$traits1b) == T |
                           grepl("Outrageous tweets by Trump and Biden", lucid$traits1b) == T |
                           grepl("Celebrity endorsements of Trump and Biden", lucid$traits1b) == T |
                           grepl("Election jokes from late night TV", lucid$traits1b) == T, 1, 0)
lucid$easy_B_2 <- ifelse(grepl("Gay marriage", lucid$traits2b) == T |
                           grepl("2020 Presidential race", lucid$traits2b) == T |
                           grepl("Congressional scandals", lucid$traits2b) == T |
                           grepl("Outrageous tweets by Trump and Biden", lucid$traits2b) == T |
                           grepl("Celebrity endorsements of Trump and Biden", lucid$traits2b) == T |
                           grepl("Election jokes from late night TV", lucid$traits2b) == T, 1, 0)
lucid$easy_B_3 <- ifelse(grepl("Gay marriage", lucid$traits3b) == T |
                           grepl("2020 Presidential race", lucid$traits3b) == T |
                           grepl("Congressional scandals", lucid$traits3b) == T |
                           grepl("Outrageous tweets by Trump and Biden", lucid$traits3b) == T |
                           grepl("Celebrity endorsements of Trump and Biden", lucid$traits3b) == T |
                           grepl("Election jokes from late night TV", lucid$traits3b) == T, 1, 0)
lucid$easy_B_4 <- ifelse(grepl("Gay marriage", lucid$traits4b) == T |
                           grepl("2020 Presidential race", lucid$traits4b) == T |
                           grepl("Congressional scandals", lucid$traits4b) == T |
                           grepl("Outrageous tweets by Trump and Biden", lucid$traits4b) == T |
                           grepl("Celebrity endorsements of Trump and Biden", lucid$traits4b) == T |
                           grepl("Election jokes from late night TV", lucid$traits4b) == T, 1, 0)
lucid$easy_B_5 <- ifelse(grepl("Gay marriage", lucid$traits5b) == T |
                           grepl("2020 Presidential race", lucid$traits5b) == T |
                           grepl("Congressional scandals", lucid$traits5b) == T |
                           grepl("Outrageous tweets by Trump and Biden", lucid$traits5b) == T |
                           grepl("Celebrity endorsements of Trump and Biden", lucid$traits5b) == T |
                           grepl("Election jokes from late night TV", lucid$traits5b) == T, 1, 0)

# Indicator for print vs video
lucid$print_A_1 <- ifelse(grepl("Text", lucid$traits1a) == T | grepl("Print article", lucid$traits1a) == T, 1, 0)
lucid$print_A_2 <- ifelse(grepl("Text", lucid$traits2a) == T | grepl("Print article", lucid$traits2a) == T, 1, 0)
lucid$print_A_3 <- ifelse(grepl("Text", lucid$traits3a) == T | grepl("Print article", lucid$traits3a) == T, 1, 0)
lucid$print_A_4 <- ifelse(grepl("Text", lucid$traits4a) == T | grepl("Print article", lucid$traits4a) == T, 1, 0)
lucid$print_A_5 <- ifelse(grepl("Text", lucid$traits5a) == T | grepl("Print article", lucid$traits5a) == T, 1, 0)
lucid$print_B_1 <- ifelse(grepl("Text", lucid$traits1b) == T | grepl("Print article", lucid$traits1b) == T, 1, 0)
lucid$print_B_2 <- ifelse(grepl("Text", lucid$traits2b) == T | grepl("Print article", lucid$traits2b) == T, 1, 0)
lucid$print_B_3 <- ifelse(grepl("Text", lucid$traits3b) == T | grepl("Print article", lucid$traits3b) == T, 1, 0)
lucid$print_B_4 <- ifelse(grepl("Text", lucid$traits4b) == T | grepl("Print article", lucid$traits4b) == T, 1, 0)
lucid$print_B_5 <- ifelse(grepl("Text", lucid$traits5b) == T | grepl("Print article", lucid$traits5b) == T, 1, 0)

# Indicator for 5min vs 1min
lucid$length_A_1 <- ifelse(grepl("5 minutes", lucid$traits1a) == T, 1, 0)
lucid$length_A_2 <- ifelse(grepl("5 minutes", lucid$traits2a) == T, 1, 0)
lucid$length_A_3 <- ifelse(grepl("5 minutes", lucid$traits3a) == T, 1, 0)
lucid$length_A_4 <- ifelse(grepl("5 minutes", lucid$traits4a) == T, 1, 0)
lucid$length_A_5 <- ifelse(grepl("5 minutes", lucid$traits5a) == T, 1, 0)
lucid$length_B_1 <- ifelse(grepl("5 minutes", lucid$traits1b) == T, 1, 0)
lucid$length_B_2 <- ifelse(grepl("5 minutes", lucid$traits2b) == T, 1, 0)
lucid$length_B_3 <- ifelse(grepl("5 minutes", lucid$traits3b) == T, 1, 0)
lucid$length_B_4 <- ifelse(grepl("5 minutes", lucid$traits4b) == T, 1, 0)
lucid$length_B_5 <- ifelse(grepl("5 minutes", lucid$traits5b) == T, 1, 0)

# Indicator for style (sentence writing only)
lucid$infoseg_A_1 <- ifelse(grepl("An informational segment about recent news", lucid$traits1a) == T, 1, 0)
lucid$infoseg_A_2 <- ifelse(grepl("An informational segment about recent news", lucid$traits2a) == T, 1, 0)
lucid$infoseg_A_3 <- ifelse(grepl("An informational segment about recent news", lucid$traits3a) == T, 1, 0)
lucid$infoseg_A_4 <- ifelse(grepl("An informational segment about recent news", lucid$traits4a) == T, 1, 0)
lucid$infoseg_A_5 <- ifelse(grepl("An informational segment about recent news", lucid$traits5a) == T, 1, 0)
lucid$infoseg_B_1 <- ifelse(grepl("An informational segment about recent news", lucid$traits1b) == T, 1, 0)
lucid$infoseg_B_2 <- ifelse(grepl("An informational segment about recent news", lucid$traits2b) == T, 1, 0)
lucid$infoseg_B_3 <- ifelse(grepl("An informational segment about recent news", lucid$traits3b) == T, 1, 0)
lucid$infoseg_B_4 <- ifelse(grepl("An informational segment about recent news", lucid$traits4b) == T, 1, 0)
lucid$infoseg_B_5 <- ifelse(grepl("An informational segment about recent news", lucid$traits5b) == T, 1, 0)

# Export cleaned and coded data in wide format
write.csv(lucid, "Data/Lucid_Coded.csv", row.names = F)

##### Incentive resolution (emails deleted after running this code) #####

# # symbol counting
# lucid$email[which(lucid$Correct_trials == max(lucid$Correct_trials, na.rm = T))]
# set.seed(8)
# sample(lucid$email[which(lucid$Correct_trials == max(lucid$Correct_trials, na.rm = T)-1)], 1)
# 
# # video
# lucid$VQ_1 <- ifelse(lucid$VQ_1 == 1, 1, 0)
# lucid$VQ_2 <- ifelse(lucid$VQ_2 == 2, 1, 0)
# lucid$VQ_3 <- ifelse(lucid$VQ_3 == 1, 1, 0)
# lucid$VQ_4 <- ifelse(lucid$VQ_4 == 2, 1, 0)
# lucid$video <- rowSums(lucid[,c("VQ_1","VQ_2","VQ_3","VQ_4")])
# set.seed(8)
# sample(lucid$email[which(lucid$video == 4)], 5)
# 
# # sentence control
# set.seed(8)
# sample(lucid$email[which(lucid$SW_control_6 != "")], 5)
# 
# # sentence treat
# set.seed(8)
# sample(lucid$email[which(lucid$SW_treat_6 != "")], 5)

##### Pre-Registered Analyses #####

# Clear environment
rm(list=ls())

# OLS function w/cluster SEs
ols <- function(form, data, robust=FALSE, cluster=NULL,digits=3){
  r1 <- lm(form, data)
  if(length(cluster)!=0){
    data <- na.omit(data[,c(colnames(r1$model),cluster)])
    r1 <- lm(form, data)
  }
  X <- model.matrix(r1)
  n <- dim(X)[1]
  k <- dim(X)[2]
  if(robust==FALSE & length(cluster)==0){
    vcv <- solve(crossprod(X)) * as.numeric(crossprod(resid(r1))/(n-k))
    se <- sqrt(diag(vcv))
    res <- cbind(coef(r1),se)
  }
  if(robust==TRUE){
    u <- matrix(resid(r1))
    meat1 <- t(X) %*% diag(diag(crossprod(t(u)))) %*% X
    dfc <- n/(n-k)
    vcv <- dfc*(solve(crossprod(X)) %*% meat1 %*% solve(crossprod(X)))
    se <- sqrt(diag(vcv))
    res <- cbind(coef(r1),se)
  }
  if(length(cluster)!=0){
    clus <- cbind(X,data[,cluster],resid(r1))
    colnames(clus)[(dim(clus)[2]-1):dim(clus)[2]] <- c(cluster,"resid")
    m <- dim(table(clus[,cluster]))
    dfc <- (m/(m-1))*((n-1)/(n-k))
    uclust  <- apply(resid(r1)*X,2, function(x) tapply(x, clus[,cluster], sum))
    vcv <- solve(crossprod(X)) %*% (t(uclust) %*% uclust) %*% solve(crossprod(X))*dfc
    se <- sqrt(diag(vcv))   
    res <- cbind(coef(r1),se)
  }
  res <- cbind(res,res[,1]/res[,2],(1-pnorm(abs(res[,1]/res[,2])))*2)
  res1 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res)),nrow=dim(res)[1])
  rownames(res1) <- rownames(res)
  colnames(res1) <- c("Estimate","Std. Error","t value","Pr(>|t|)")
  out <- list(table = res1, vcv = vcv, N = n)
  return(out)
}

# Import data
lucid <- read.csv("Data/Lucid_Coded.csv")

# Reshape to long over choices
lucid_long_temp <- reshape(lucid, direction = "long",
                           varying = list(c("choice_A_1","choice_A_2","choice_A_3","choice_A_4","choice_A_5"),
                                          c("choice_B_1","choice_B_2","choice_B_3","choice_B_4","choice_B_5"),
                                          c("politics_A_1","politics_A_2","politics_A_3","politics_A_4","politics_A_5"),
                                          c("politics_B_1","politics_B_2","politics_B_3","politics_B_4","politics_B_5"),
                                          c("infoseg_A_1","infoseg_A_2","infoseg_A_3","infoseg_A_4","infoseg_A_5"),
                                          c("infoseg_B_1","infoseg_B_2","infoseg_B_3","infoseg_B_4","infoseg_B_5"),
                                          c("print_A_1","print_A_2","print_A_3","print_A_4","print_A_5"),
                                          c("print_B_1","print_B_2","print_B_3","print_B_4","print_B_5"),
                                          c("length_A_1","length_A_2","length_A_3","length_A_4","length_A_5"),
                                          c("length_B_1","length_B_2","length_B_3","length_B_4","length_B_5"),
                                          c("hard_A_1","hard_A_2","hard_A_3","hard_A_4","hard_A_5"),
                                          c("hard_B_1","hard_B_2","hard_B_3","hard_B_4","hard_B_5"),
                                          c("easy_A_1","easy_A_2","easy_A_3","easy_A_4","easy_A_5"),
                                          c("easy_B_1","easy_B_2","easy_B_3","easy_B_4","easy_B_5")),
                           v.names = c("choice_A","choice_B","politics_A","politics_B",
                                       "infoseg_A","infoseg_B","print_A","print_B",
                                       "length_A","length_B","hard_A","hard_B","easy_A","easy_B"))

# Reshape to long over alternatives
colnames(lucid_long_temp)[which(colnames(lucid_long_temp)=="id")] <- "id_temp"
colnames(lucid_long_temp)[which(colnames(lucid_long_temp)=="time")] <- "time_temp"
lucid_long <- reshape(lucid_long_temp, direction = "long",
                      varying = list(c("choice_A","choice_B"),
                                     c("politics_A","politics_B"),
                                     c("infoseg_A","infoseg_B"),
                                     c("print_A","print_B"),
                                     c("length_A","length_B"),
                                     c("hard_A","hard_B"),
                                     c("easy_A","easy_B")),
                      v.names = c("choice","politics","infoseg",
                                  "print","length","hard","easy"))

# export
write.csv(lucid_long, "Data/Lucid Long Format.csv", row.names = F, na = "")

### Models ###

# Manipulation checks
summary(lm(manip01 ~ fatigue, data=subset(lucid, exp == "sentence"))) # B0 = 0.46, B1 = 0.34, R2 = 0.32
summary(lm(manip01 ~ fatigue, data=subset(lucid, exp == "symbol"))) # B0 = 0.18, B1 = 0.36, R2 = 0.42
summary(lm((SC_MC_4-1)/6 ~ fatigue, data=subset(lucid, exp == "symbol"))) # B0 = 0.24, B1 = 0.04, R2 = 0.01

# Balance checks
table(lucid$exp, lucid$fatigue)/rowSums(table(lucid$exp, lucid$fatigue))
lucid$fatigue_num <- ifelse(is.na(lucid$fatigue), NA, ifelse(lucid$fatigue=="control", 0, 1))
table(lucid$exp, lucid$fatigue_num)/rowSums(table(lucid$exp, lucid$fatigue_num))
m_sentence <- glm(fatigue_num ~ age01 + male + black + hisp + educ01 + income01 + unemp + pid01 + ideo01 + pidstr01 + ideostr01 + eng01, 
           data=subset(lucid, exp == "sentence"), family=binomial(link="logit"))
write.csv(summary(m_sentence)$coefficients, "Tables/Balance_1.csv", row.names = T)
pchisq(m_sentence$null.deviance - m_sentence$deviance, m_sentence$df.null - m_sentence$df.residual, lower.tail = F) # LRtest
m_symbol <- glm(fatigue_num ~ age01 + male + black + hisp + educ01 + income01 + unemp + pid01 + ideo01 + pidstr01 + ideostr01 + eng01, 
           data=subset(lucid, exp == "symbol"), family=binomial(link="logit"))
write.csv(summary(m_symbol)$coefficients, "Tables/Balance_2.csv", row.names = T)
pchisq(m_symbol$null.deviance - m_symbol$deviance, m_symbol$df.null - m_symbol$df.residual, lower.tail = F) # LRtest

# Formulas
formula_1a <- "choice ~ politics*fatigue + politics*eng01 + infoseg*fatigue + print*fatigue + length*fatigue"
formula_1b <- "choice ~ politics*fatigue + infoseg*fatigue + print*fatigue + length*fatigue"
formula_1c <- "choice ~ politics*fatigue"
formula_2a <- "choice ~ hard*fatigue + hard*eng01 + easy*fatigue + easy*eng01 + infoseg*fatigue + print*fatigue + length*fatigue"
formula_2b <- "choice ~ hard*fatigue + easy*fatigue + infoseg*fatigue + print*fatigue + length*fatigue"
formula_2c <- "choice ~ hard*fatigue + easy*fatigue"
formula_3a <- "choice ~ politics*fatigue + politics*eng01 + print*fatigue + length*fatigue"
formula_3b <- "choice ~ politics*fatigue + print*fatigue + length*fatigue"
formula_3c <- "choice ~ politics*fatigue"
formula_4a <- "choice ~ hard*fatigue + hard*eng01 + easy*fatigue + easy*eng01 + print*fatigue + length*fatigue"
formula_4b <- "choice ~ hard*fatigue + easy*fatigue + print*fatigue + length*fatigue"
formula_4c <- "choice ~ hard*fatigue + easy*fatigue"

# Sentence writing models
m_1a <- ols(as.formula(formula_1a), data=subset(lucid_long, exp == "sentence"), cluster="ResponseId")
m_1b <- ols(as.formula(formula_1b), data=subset(lucid_long, exp == "sentence"), cluster="ResponseId")
m_1c <- ols(as.formula(formula_1c), data=subset(lucid_long, exp == "sentence"), cluster="ResponseId")
m_2a <- ols(as.formula(formula_2a), data=subset(lucid_long, exp == "sentence"), cluster="ResponseId")
m_2b <- ols(as.formula(formula_2b), data=subset(lucid_long, exp == "sentence"), cluster="ResponseId")
m_2c <- ols(as.formula(formula_2c), data=subset(lucid_long, exp == "sentence"), cluster="ResponseId")
m_1a$table
m_1b$table
m_1c$table
m_2a$table
m_2b$table
m_2c$table

# Symbol writing models
m_3a <- ols(as.formula(formula_3a), data=subset(lucid_long, exp == "symbol"), cluster="ResponseId")
m_3b <- ols(as.formula(formula_3b), data=subset(lucid_long, exp == "symbol"), cluster="ResponseId")
m_3c <- ols(as.formula(formula_3c), data=subset(lucid_long, exp == "symbol"), cluster="ResponseId")
m_4a <- ols(as.formula(formula_4a), data=subset(lucid_long, exp == "symbol"), cluster="ResponseId")
m_4b <- ols(as.formula(formula_4b), data=subset(lucid_long, exp == "symbol"), cluster="ResponseId")
m_4c <- ols(as.formula(formula_4c), data=subset(lucid_long, exp == "symbol"), cluster="ResponseId")
m_3a$table
m_3b$table
m_3c$table
m_4a$table
m_4b$table
m_4c$table

# Regression tables
write.csv(m_1a$table, "Tables/Lucid_1.csv", row.names = T)
write.csv(m_1b$table, "Tables/Lucid_2.csv", row.names = T)
write.csv(m_1c$table, "Tables/Lucid_3.csv", row.names = T)
write.csv(m_2a$table, "Tables/Lucid_4.csv", row.names = T)
write.csv(m_2b$table, "Tables/Lucid_5.csv", row.names = T)
write.csv(m_2c$table, "Tables/Lucid_6.csv", row.names = T)
write.csv(m_3a$table, "Tables/Lucid_7.csv", row.names = T)
write.csv(m_3b$table, "Tables/Lucid_8.csv", row.names = T)
write.csv(m_3c$table, "Tables/Lucid_9.csv", row.names = T)
write.csv(m_4a$table, "Tables/Lucid_10.csv", row.names = T)
write.csv(m_4b$table, "Tables/Lucid_11.csv", row.names = T)
write.csv(m_4c$table, "Tables/Lucid_12.csv", row.names = T)

# Balance robustness checks
formula_balance_1 <- "choice ~ politics*fatigue + politics*eng01 + 
politics*hisp + politics*pid01 + politics*ideo01 + politics*pidstr01 +
infoseg*fatigue + print*fatigue + length*fatigue"
formula_balance_2 <- "choice ~ hard*fatigue + hard*eng01 + easy*fatigue + easy*eng01 + 
hard*hisp + hard*pid01 + hard*ideo01 + hard*pidstr01 +
easy*hisp + easy*pid01 + easy*ideo01 + easy*pidstr01 +
infoseg*fatigue + print*fatigue + length*fatigue"
m_balance_1 <- ols(as.formula(formula_balance_1), data=subset(lucid_long, exp == "sentence"), cluster="ResponseId")
m_balance_2 <- ols(as.formula(formula_balance_2), data=subset(lucid_long, exp == "sentence"), cluster="ResponseId")
m_balance_1$table
m_balance_2$table

### Combined with TESS study ###

# Bring in Tess data and rename vars to be consistent with Lucid data
tess_long <- read.csv("Data/TESS_Long.csv", header = T)
tess_long$eng01 <- tess_long$interest
tess_long$ResponseId <- tess_long$CaseID
tess_long$dataset <- 0
lucid_long$dataset <- 1

# Keep only vars consistent across datasets
tess_long_vars <- tess_long[,c("choice","politics","hard","easy","fatigue",
                               "eng01","print","length","infoseg",
                               "ResponseId","dataset")]
lucid_long_vars <- lucid_long[,c("choice","politics","hard","easy","fatigue",
                               "eng01","print","length","infoseg",
                               "ResponseId","dataset","exp")]
lucid_long_vars <- subset(lucid_long_vars, exp == "sentence")
lucid_long_vars$fatigue <- ifelse(is.na(lucid_long_vars$fatigue), NA, ifelse(lucid_long_vars$fatigue=="control", 0, 1))
table(lucid_long_vars$fatigue)
lucid_long_vars <- lucid_long_vars[,-which(names(lucid_long_vars) %in% c("exp"))]

# Combine datasets
combined <- rbind(tess_long_vars, lucid_long_vars)

# export merged data
write.csv(combined, "Data/Merged Data Stata.csv", row.names = F, na = "")

# Formulas
formula_5a <- "choice ~ politics*fatigue + politics*eng01 + infoseg*fatigue + print*fatigue + length*fatigue + dataset"
formula_5b <- "choice ~ politics*fatigue + infoseg*fatigue + print*fatigue + length*fatigue + dataset"
formula_5c <- "choice ~ politics*fatigue + dataset"
formula_6a <- "choice ~ hard*fatigue + hard*eng01 + easy*fatigue + easy*eng01 + infoseg*fatigue + print*fatigue + length*fatigue + dataset"
formula_6b <- "choice ~ hard*fatigue + easy*fatigue + infoseg*fatigue + print*fatigue + length*fatigue + dataset"
formula_6c <- "choice ~ hard*fatigue + easy*fatigue + dataset"

# Sentence writing models combined data
m_5a <- ols(as.formula(formula_5a), data=combined, cluster="ResponseId")
m_5b <- ols(as.formula(formula_5b), data=combined, cluster="ResponseId")
m_5c <- ols(as.formula(formula_5c), data=combined, cluster="ResponseId")
m_6a <- ols(as.formula(formula_6a), data=combined, cluster="ResponseId")
m_6b <- ols(as.formula(formula_6b), data=combined, cluster="ResponseId")
m_6c <- ols(as.formula(formula_6c), data=combined, cluster="ResponseId")
m_5a$table
m_5b$table
m_5c$table
m_6a$table
m_6b$table
m_6c$table

# Regression tables
write.csv(m_5a$table, "Tables/Lucid_13.csv", row.names = T)
write.csv(m_5b$table, "Tables/Lucid_14.csv", row.names = T)
write.csv(m_5c$table, "Tables/Lucid_15.csv", row.names = T)
write.csv(m_6a$table, "Tables/Lucid_16.csv", row.names = T)
write.csv(m_6b$table, "Tables/Lucid_17.csv", row.names = T)
write.csv(m_6c$table, "Tables/Lucid_18.csv", row.names = T)


summary(lm(as.formula(formula_6c), data=subset(combined, dataset == 1)))

summary(lm(as.formula(formula_6c), data=combined))














