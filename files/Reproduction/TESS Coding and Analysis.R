##### TESS Experiment Coding #####

# Clear environment
rm(list=ls())

# Load dataset
tess <- read.csv("Data/TESS3_171_Johnston_Client.csv", header = T)

### Background variables ###

# Age
tess$age <- (tess$ppage - min(tess$ppage)) / (max(tess$ppage) - min(tess$ppage))

# Education
tess$educ <- ifelse(tess$ppeduc < 9, 0, 
                    ifelse(tess$ppeduc > 8 & tess$ppeduc < 13, 
                           tess$ppeduc - 8, 5)) / 5

# Race/Ethnicity
tess$black <- ifelse(tess$ppethm == 2, 1, 0)
tess$hisp <- ifelse(tess$ppethm == 4, 1, 0)

# Gender
tess$female <- ifelse(tess$ppgender == 2, 1, 0)

# Income
tess$income <- (19 - tess$ppincimp) / 18

# South
tess$south <- ifelse(tess$PPREG4 == 3, 1, 0)

# Unemployed
tess$unemp <- ifelse(tess$ppwork == 3 | tess$ppwork == 4, 1, 0)

# Religiosity
tess$relig <- (6 - tess$XREL2) / 5

# Political interest
tess$interest <- (5 - tess$XPP10035) / 4

# 7-point partisanship (higher = Republican)
tess$pid <- (7 - tess$XPARTY7) / 6

# Binary partisanship (1 = Rep, 0 = Dem, Ind = missing)
tess$b_pid <- ifelse(tess$XPARTY7 > 4, 1, ifelse(tess$XPARTY7 < 4, 0, NA))

# PID strength
tess$pid_str <- abs(tess$pid - .5) / .5

# 7-point ideology (higher = conservative)
tess$ideo <- (7 - tess$xideo) / 6

# Ideo strength
tess$ideo_str <- abs(tess$ideo - .5) / .5

### Experimental variables ###

# Condition
tess$condition <- tess$XTESS171

# Cognitive fatigue treatment condition (1 = treated)
tess$fatigue <- ifelse(tess$condition < 8, 0, 1)

# Fatigue manipulation check
tess$effort <- ifelse(tess$Q3 == -1, NA, tess$Q3)
tess$concentrate <- ifelse(tess$Q4 == -1, NA, tess$Q4)
tess$fatigue_check <- ((tess$effort + tess$concentrate) - 2) / 8
summary(lm(effort ~ fatigue, data=tess))
summary(lm(concentrate ~ fatigue, data=tess))
summary(lm(fatigue_check ~ fatigue, data=tess))

# Experiment assignment indicator (1 = participation experiment, 0 = policy experiment)
tess$experiment <- ifelse(tess$condition == 1 | tess$condition == 8, 1, 0)

# Cues treatment for policy experiment (1 = cues)
tess$cues <- ifelse(tess$condition %in% c(2:4,9:11), 0, 
                    ifelse(tess$condition %in% c(5:7,12:14), 1, NA))

# Arguments treatment for policy experiment (1 = strong pro, 0 = no args, -1 = strong con)
tess$args <- ifelse(tess$condition %in% c(2,5,9,12), 0, 
                    ifelse(tess$condition %in% c(3,6,10,13), 1, 
                           ifelse(tess$condition %in% c(4,7,11,14), -1, NA)))

# DREAM Act support
tess$dream <- NA
for (i in 1:nrow(tess)){
  if (tess$condition[i] %in% c(2,9)){
    tess$dream[i] <- ifelse(tess$Q6A[i] == -1, NA, (6 - tess$Q6A[i]) / 5)
  } else if (tess$condition[i] %in% c(5,12)){
    tess$dream[i] <- ifelse(tess$Q6B[i] == -1, NA, (6 - tess$Q6B[i]) / 5)
  } else if (tess$condition[i] %in% c(6,13)){
    tess$dream[i] <- ifelse(tess$Q6C[i] == -1, NA, (6 - tess$Q6C[i]) / 5)
  } else if (tess$condition[i] %in% c(3,10)){
    tess$dream[i] <- ifelse(tess$Q6D[i] == -1, NA, (6 - tess$Q6D[i]) / 5)
  } else if (tess$condition[i] %in% c(7,14)){
    tess$dream[i] <- ifelse(tess$Q6E[i] == -1, NA, (6 - tess$Q6E[i]) / 5)
  } else if (tess$condition[i] %in% c(4,11)){
    tess$dream[i] <- ifelse(tess$Q6F[i] == -1, NA, (6 - tess$Q6F[i]) / 5)
  }
}

# Post-treatment participation variables
tess$exp_interest <- ifelse(tess$Q7 == -1, NA, (5 - tess$Q7) / 4)
tess$exp_complex <- ifelse(tess$Q8 == -1, NA, (5 - tess$Q8) / 4)
tess$exp_active <- ifelse(tess$Q9 == -1, NA, (6 - tess$Q8) / 5)
tess$exp_partic <- rowMeans(tess[,c("exp_interest","exp_complex","exp_active")])

# Media choice DV (in original vars, 1=chose A, 2=chose B)
tess$choice_A_1 <- ifelse(tess$Q5A == -1 | tess$experiment == 0, NA, 2 - tess$Q5A)
tess$choice_B_1 <- ifelse(tess$Q5A == -1 | tess$experiment == 0, NA, tess$Q5A - 1)
tess$choice_A_2 <- ifelse(tess$Q5B == -1 | tess$experiment == 0, NA, 2 - tess$Q5B)
tess$choice_B_2 <- ifelse(tess$Q5B == -1 | tess$experiment == 0, NA, tess$Q5B - 1)
tess$choice_A_3 <- ifelse(tess$Q5C == -1 | tess$experiment == 0, NA, 2 - tess$Q5C)
tess$choice_B_3 <- ifelse(tess$Q5C == -1 | tess$experiment == 0, NA, tess$Q5C - 1)

# Media content indicator for politics (hard or easy)
tess$politics_A_1 <- ifelse(is.na(tess$Q5A_OptA_Genre), NA, 
                            ifelse(tess$Q5A_OptA_Genre == 1, 1, 0))
tess$politics_B_1 <- ifelse(is.na(tess$Q5A_OptB_Genre), NA, 
                            ifelse(tess$Q5A_OptB_Genre == 1, 1, 0))
tess$politics_A_2 <- ifelse(is.na(tess$Q5B_OptA_Genre), NA,
                            ifelse(tess$Q5B_OptA_Genre == 1, 1, 0))
tess$politics_B_2 <- ifelse(is.na(tess$Q5B_OptB_Genre), NA, 
                            ifelse(tess$Q5B_OptB_Genre == 1, 1, 0))
tess$politics_A_3 <- ifelse(is.na(tess$Q5C_OptA_Genre), NA, 
                            ifelse(tess$Q5C_OptA_Genre == 1, 1, 0))
tess$politics_B_3 <- ifelse(is.na(tess$Q5C_OptB_Genre), NA, 
                            ifelse(tess$Q5C_OptB_Genre == 1, 1, 0))

# Media content indicator for style, informational segment (1) v. debate or human interest
tess$infoseg_A_1 <- ifelse(is.na(tess$Q5A_OptA_Style), NA, 
                           ifelse(tess$Q5A_OptA_Style == 2, 1, 0))
tess$infoseg_B_1 <- ifelse(is.na(tess$Q5A_OptB_Style), NA, 
                           ifelse(tess$Q5A_OptB_Style == 2, 1, 0))
tess$infoseg_A_2 <- ifelse(is.na(tess$Q5B_OptA_Style), NA,
                           ifelse(tess$Q5B_OptA_Style == 2, 1, 0))
tess$infoseg_B_2 <- ifelse(is.na(tess$Q5B_OptB_Style), NA, 
                           ifelse(tess$Q5B_OptB_Style == 2, 1, 0))
tess$infoseg_A_3 <- ifelse(is.na(tess$Q5C_OptA_Style), NA, 
                           ifelse(tess$Q5C_OptA_Style == 2, 1, 0))
tess$infoseg_B_3 <- ifelse(is.na(tess$Q5C_OptB_Style), NA, 
                           ifelse(tess$Q5C_OptB_Style == 2, 1, 0))

# Media content indicator for print (1) versus video (0)
tess$print_A_1 <- ifelse(is.na(tess$Q5A_OptA_Medium), NA, 
                         ifelse(tess$Q5A_OptA_Medium == 2, 1, 0))
tess$print_B_1 <- ifelse(is.na(tess$Q5A_OptB_Medium), NA, 
                         ifelse(tess$Q5A_OptB_Medium == 2, 1, 0))
tess$print_A_2 <- ifelse(is.na(tess$Q5B_OptA_Medium), NA,
                         ifelse(tess$Q5B_OptA_Medium == 2, 1, 0))
tess$print_B_2 <- ifelse(is.na(tess$Q5B_OptB_Medium), NA, 
                         ifelse(tess$Q5B_OptB_Medium == 2, 1, 0))
tess$print_A_3 <- ifelse(is.na(tess$Q5C_OptA_Medium), NA, 
                         ifelse(tess$Q5C_OptA_Medium == 2, 1, 0))
tess$print_B_3 <- ifelse(is.na(tess$Q5C_OptB_Medium), NA, 
                         ifelse(tess$Q5C_OptB_Medium == 2, 1, 0))

# Media content indicator for length, 5 min (1) versus 1 min (0)
tess$length_A_1 <- ifelse(is.na(tess$Q5A_OptA_Length), NA, 
                          ifelse(tess$Q5A_OptA_Length == 2, 1, 0))
tess$length_B_1 <- ifelse(is.na(tess$Q5A_OptB_Length), NA, 
                          ifelse(tess$Q5A_OptB_Length == 2, 1, 0))
tess$length_A_2 <- ifelse(is.na(tess$Q5B_OptA_Length), NA,
                          ifelse(tess$Q5B_OptA_Length == 2, 1, 0))
tess$length_B_2 <- ifelse(is.na(tess$Q5B_OptB_Length), NA, 
                          ifelse(tess$Q5B_OptB_Length == 2, 1, 0))
tess$length_A_3 <- ifelse(is.na(tess$Q5C_OptA_Length), NA, 
                          ifelse(tess$Q5C_OptA_Length == 2, 1, 0))
tess$length_B_3 <- ifelse(is.na(tess$Q5C_OptB_Length), NA, 
                          ifelse(tess$Q5C_OptB_Length == 2, 1, 0))

# Media content indicator for type of politics (hard topics)
tess$hard_A_1 <- ifelse(is.na(tess$Q5A_OptA_Topic), NA, 
                        ifelse(tess$Q5A_OptA_Topic %in% c(1:3), 1, 0))
tess$hard_B_1 <- ifelse(is.na(tess$Q5A_OptB_Topic), NA,
                        ifelse(tess$Q5A_OptB_Topic %in% c(1:3), 1, 0))
tess$hard_A_2 <- ifelse(is.na(tess$Q5B_OptA_Topic), NA, 
                        ifelse(tess$Q5B_OptA_Topic %in% c(1:3), 1, 0))
tess$hard_B_2 <- ifelse(is.na(tess$Q5B_OptB_Topic), NA, 
                        ifelse(tess$Q5B_OptB_Topic %in% c(1:3), 1, 0))
tess$hard_A_3 <- ifelse(is.na(tess$Q5C_OptA_Topic), NA, 
                        ifelse(tess$Q5C_OptA_Topic %in% c(1:3), 1, 0))
tess$hard_B_3 <- ifelse(is.na(tess$Q5C_OptB_Topic), NA, 
                        ifelse(tess$Q5C_OptB_Topic %in% c(1:3), 1, 0))

# Media content indicator for type of politics (easy topics)
tess$easy_A_1 <- ifelse(is.na(tess$Q5A_OptA_Topic), NA, 
                        ifelse(tess$Q5A_OptA_Topic %in% c(4:6), 1, 0))
tess$easy_B_1 <- ifelse(is.na(tess$Q5A_OptB_Topic), NA, 
                        ifelse(tess$Q5A_OptB_Topic %in% c(4:6), 1, 0))
tess$easy_A_2 <- ifelse(is.na(tess$Q5B_OptA_Topic), NA, 
                        ifelse(tess$Q5B_OptA_Topic %in% c(4:6), 1, 0))
tess$easy_B_2 <- ifelse(is.na(tess$Q5B_OptB_Topic), NA, 
                        ifelse(tess$Q5B_OptB_Topic %in% c(4:6), 1, 0))
tess$easy_A_3 <- ifelse(is.na(tess$Q5C_OptA_Topic), NA, 
                        ifelse(tess$Q5C_OptA_Topic %in% c(4:6), 1, 0))
tess$easy_B_3 <- ifelse(is.na(tess$Q5C_OptB_Topic), NA, 
                        ifelse(tess$Q5C_OptB_Topic %in% c(4:6), 1, 0))

write.csv(tess, "Data/TESS_Coded.csv", row.names = F)







##### TESS media choice experiment #####

# Clear environment
rm(list=ls())

# Packages
library(MASS)

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
tess <- read.csv("Data/TESS_Coded.csv")

# Reshape to long
tess_long_temp <- reshape(tess, direction = "long",
                          varying = list(c("choice_A_1","choice_A_2","choice_A_3"),
                                         c("choice_B_1","choice_B_2","choice_B_3"),
                                         c("politics_A_1","politics_A_2","politics_A_3"),
                                         c("politics_B_1","politics_B_2","politics_B_3"),
                                         c("infoseg_A_1","infoseg_A_2","infoseg_A_3"),
                                         c("infoseg_B_1","infoseg_B_2","infoseg_B_3"),
                                         c("print_A_1","print_A_2","print_A_3"),
                                         c("print_B_1","print_B_2","print_B_3"),
                                         c("length_A_1","length_A_2","length_A_3"),
                                         c("length_B_1","length_B_2","length_B_3"),
                                         c("hard_A_1","hard_A_2","hard_A_3"),
                                         c("hard_B_1","hard_B_2","hard_B_3"),
                                         c("easy_A_1","easy_A_2","easy_A_3"),
                                         c("easy_B_1","easy_B_2","easy_B_3")),
                          v.names = c("choice_A","choice_B","politics_A","politics_B",
                                      "infoseg_A","infoseg_B","print_A","print_B",
                                      "length_A","length_B","hard_A","hard_B","easy_A","easy_B"))
colnames(tess_long_temp)[which(colnames(tess_long_temp)=="id")] <- "id_temp"
colnames(tess_long_temp)[which(colnames(tess_long_temp)=="time")] <- "time_temp"
tess_long <- reshape(tess_long_temp, direction = "long",
                     varying = list(c("choice_A","choice_B"),
                                    c("politics_A","politics_B"),
                                    c("infoseg_A","infoseg_B"),
                                    c("print_A","print_B"),
                                    c("length_A","length_B"),
                                    c("hard_A","hard_B"),
                                    c("easy_A","easy_B")),
                     v.names = c("choice","politics","infoseg",
                                 "print","length","hard","easy"))

# Write data for later use in combined analysis
write.csv(tess_long, "Data/Tess_Long.csv", row.names = F)
write.csv(tess_long, "Data/Tess Long Stata.csv", row.names = F, na = "")

# Politics in general #
formula_1 <- "choice ~ politics*fatigue + politics*interest + 
infoseg*fatigue + print*fatigue + length*fatigue"
m_1 <- ols(as.formula(formula_1), data=tess_long, cluster="CaseID")
m_1$table

formula_1_SA_1 <- "choice ~ politics*fatigue +
infoseg*fatigue + print*fatigue + length*fatigue"
m_1_SA_1 <- ols(as.formula(formula_1_SA_1), data=tess_long, cluster="CaseID")
m_1_SA_1$table

formula_1_SA_2 <- "choice ~ politics*fatigue"
m_1_SA_2 <- ols(as.formula(formula_1_SA_2), data=tess_long, cluster="CaseID")
m_1_SA_2$table

# Hard versus easy
formula_2 <- "choice ~ hard*fatigue + hard*interest + 
easy*fatigue + easy*interest + 
infoseg*fatigue + print*fatigue + length*fatigue"
m_2 <- ols(as.formula(formula_2), data=tess_long, cluster="CaseID")
m_2$table

formula_2_SA_1 <- "choice ~ hard*fatigue + easy*fatigue + 
infoseg*fatigue + print*fatigue + length*fatigue"
m_2_SA_1 <- ols(as.formula(formula_2_SA_1), data=tess_long, cluster="CaseID")
m_2_SA_1$table

formula_2_SA_2 <- "choice ~ hard*fatigue + easy*fatigue"
m_2_SA_2 <- ols(as.formula(formula_2_SA_2), data=tess_long, cluster="CaseID")
m_2_SA_2$table

### Table ###

write.csv(m_1$table, "Tables/TESS_1.csv", row.names = T)
write.csv(m_1_SA_1$table, "Tables/TESS_2.csv", row.names = T)
write.csv(m_1_SA_2$table, "Tables/TESS_3.csv", row.names = T)
write.csv(m_2$table, "Tables/TESS_4.csv", row.names = T)
write.csv(m_2_SA_1$table, "Tables/TESS_5.csv", row.names = T)
write.csv(m_2_SA_2$table, "Tables/TESS_6.csv", row.names = T)


##### TESS policy experiment #####

# Clear environment
rm(list=ls())

# Import data
tess <- read.csv("Data/TESS_Coded.csv")

# Model
m1 <- lm(dream ~ pid*cues*fatigue + as.factor(args)*fatigue, data = tess)
summary(m1)

s2_table_2 <- cbind(coef(m1), sqrt(diag(vcov(m1))))
write.csv(s2_table_2, "Tables/Study 2_Table 2.csv", row.names = T)

