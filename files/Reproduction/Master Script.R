
# Clear environment
rm(list=ls())

##### Get Estimates #####

# Make sure working directory is the reproduction folder
getwd()

# Run SCCB file
source("SCCB Coding and Analysis.R")

# Run GSS file
source("GSS Coding and Analysis.R")

# Run TESS file
source("TESS Coding and Analysis.R")

# Run Lucid file
source("Lucid Coding and Analysis.R")

##### Create Figure #####

results <- read.csv("Tables/Observational Results.csv", header = T, stringsAsFactors = F)
results <- results[order(results$b),]

pdf(file="Figures/Figure 1A.pdf", 
    height = 8, width = 5, family="serif", pointsize=11)

par(mar=c(5,6,1,1))

plot(results$b[results$dv!="voted"], 1:length(results$b[results$dv!="voted"]),
     xlim=c(-.3,.2), xlab="OLS regression coefficient", ylab="", axes=F,
     pch = c(15,16,17,16,16,16,15,18,18,16,18,16,16,16,17,17,17,18,17,15,16,18),
     col = c("blue","green","red","green","green","green","blue","orange","orange","green","orange","green","green","green","red","red","red","orange","red","blue","green","orange"))
axis(1, at=seq(-.3,.2,.1), labels = c("-0.30","-0.20","-0.10","0.00","0.10","0.20"))
axis(2, at=1:length(results$b[results$dv!="voted"]),
     labels = results$iv[results$dv != "voted"], las=2)
abline(v=0, lty=3)
segments(results$b[results$dv!="voted"] - 2*results$se[results$dv!="voted"], 
         1:length(results$b[results$dv!="voted"]),
         results$b[results$dv!="voted"] + 2*results$se[results$dv!="voted"],
         1:length(results$b[results$dv!="voted"]),
         col = c("blue","green","red","green","green","green","blue","orange","orange","green","orange","green","green","green","red","red","red","orange","red","blue","green","orange"))
legend("topleft", c("Interest in politics","News consumption","Interest in issues","Political action"),
       pch=c(15:18), col=c("blue","green","red","orange"),bty="n", inset=.02, cex = .9)

dev.off()


pdf(file="Figures/Figure 1B.pdf", 
    height = 6, width = 4, family="serif", pointsize=11)

par(mar=c(5,6,1,1))

plot(results$b[results$dv=="voted"], 1:length(results$b[results$dv=="voted"]),
     xlim=c(-3,2), xlab="Logistic regression coefficient", ylab="", axes=F, pch=19)
axis(1, at=seq(-3,2,1))
axis(2, at=1:length(results$b[results$dv=="voted"]),
     labels = results$iv[results$dv == "voted"], las=2)
abline(v=0, lty=3)
segments(results$b[results$dv=="voted"] - 2*results$se[results$dv=="voted"], 
         1:length(results$b[results$dv=="voted"]),
         results$b[results$dv=="voted"] + 2*results$se[results$dv=="voted"],
         1:length(results$b[results$dv=="voted"]))

dev.off()


pdf(file="Figures/Figure 1.pdf", 
    height = 6, width = 8, family="serif", pointsize=11)

par(mar=c(5,8.5,1,1), mfrow=c(1,2))

plot(results$b[results$dv!="voted"], 1:length(results$b[results$dv!="voted"]),
     xlim=c(-.4,.2), xlab="OLS regression coefficient", ylab="", axes=F,
     pch = c(15,16,17,16,16,16,15,18,18,16,18,16,16,16,17,17,17,18,17,15,16,18),
     col = c("blue","green","red","green","green","green","blue","orange","orange","green","orange","green","green","green","red","red","red","orange","red","blue","green","orange"))
axis(1, at=seq(-.4,.2,.1), labels = c("-0.40","-0.30","-0.20","-0.10","0.00","0.10","0.20"))
axis(2, at=1:length(results$b[results$dv!="voted"]),
     labels = results$iv[results$dv != "voted"], las=2)
abline(v=0, lty=3)
segments(results$b[results$dv!="voted"] - 2*results$se[results$dv!="voted"], 
         1:length(results$b[results$dv!="voted"]),
         results$b[results$dv!="voted"] + 2*results$se[results$dv!="voted"],
         1:length(results$b[results$dv!="voted"]),
         col = c("blue","green","red","green","green","green","blue","orange","orange","green","orange","green","green","green","red","red","red","orange","red","blue","green","orange"))
legend("topleft", c("Interest in politics","News consumption","Interest in issues","Political action"),
       pch=c(15:18), col=c("blue","green","red","orange"),bty="n", inset=.02, cex = .8)

par(mar=c(5,8.5,1,1))

plot(results$b[results$dv=="voted"], 1:length(results$b[results$dv=="voted"]),
     xlim=c(-2,2), xlab="Logistic regression coefficient", ylab="", axes=F, pch=19)
axis(1, at=seq(-2,2,1))
axis(2, at=1:length(results$b[results$dv=="voted"]),
     labels = results$iv[results$dv == "voted"], las=2)
abline(v=0, lty=3)
segments(results$b[results$dv=="voted"] - 2*results$se[results$dv=="voted"], 
         1:length(results$b[results$dv=="voted"]),
         results$b[results$dv=="voted"] + 2*results$se[results$dv=="voted"],
         1:length(results$b[results$dv=="voted"]))
legend("topleft", c("Turnout"), pch=16, bty="n", inset=.02, cex=.8)

dev.off()

##### Meta-analysis of observational results #####

library(meta)

# load data
effects <- read.csv("Data/Combined Observational Results.csv", stringsAsFactors = F)
head(effects)

### Linear models ###

# meta-analysis for linear models using all effects

m_lin_all <- metagen(TE = beta,
                 seTE = se,
                 studlab = iv,
                 data = subset(effects, dv != "turnout"),
                 sm = "MD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Linear Models")
summary(m_lin_all)

# meta-analysis for linear models for sleep problems IV

m_lin_sleep <- metagen(TE = beta,
                     seTE = se,
                     studlab = iv,
                     data = subset(effects, dv != "turnout" & iv == "sleepprobs"),
                     sm = "MD",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     hakn = TRUE,
                     title = "Linear Models - Sleep Problems")
summary(m_lin_sleep)

# meta-analysis for linear models for all other IVs

m_lin_nonsleep <- metagen(TE = beta,
                       seTE = se,
                       studlab = iv,
                       data = subset(effects, dv != "turnout" & iv != "sleepprobs"),
                       sm = "MD",
                       fixed = FALSE,
                       random = TRUE,
                       method.tau = "REML",
                       hakn = TRUE,
                       title = "Linear Models - Sleep Problems")
summary(m_lin_nonsleep)

# meta-analysis for linear models for all other IVs, no usedup

m_lin_nonsleep_noused <- metagen(TE = beta,
                          seTE = se,
                          studlab = iv,
                          data = subset(effects, dv != "turnout" & iv != "sleepprobs" & iv != "usedup"),
                          sm = "MD",
                          fixed = FALSE,
                          random = TRUE,
                          method.tau = "REML",
                          hakn = TRUE,
                          title = "Linear Models - Sleep Problems")
summary(m_lin_nonsleep_noused)

### Logit models ###

# meta-analysis for linear models using all effects

m_log_all <- metagen(TE = beta,
                     seTE = se,
                     studlab = iv,
                     data = subset(effects, dv == "turnout"),
                     sm = "MD",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     hakn = TRUE,
                     title = "Linear Models")
summary(m_log_all)

