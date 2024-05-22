library('xts')
library('reshape2')
library('ggplot2')
library(tidyverse)
library(lubridate)
library(nlme)

source("GLLAfunctions.R")

# ----------------------------------
# Read in the different variables: sleep duration, stress, productivity, physical activity
# slp<-read.csv("dailySleep.csv", header=TRUE, colClasses = c('character','numeric'))
# slp <- subset(slp, select = -c(TST))
# slp %>% group_by(slp$pid) %>% count()

daily<-read.csv("daily_measures_imputed.csv", header=TRUE)
daily %>% group_by(daily$pid) %>% count()
pids = as.data.frame(unique(daily$pid))
names(pids) <- 'pid'
n <- dim(pids)[1]

# get column names after the first two
metrics <- names(daily)[3:ncol(daily)]

# ----------------------------------
## time shuffle
for (m in metrics) {  
  datacorr <- matrix(NA,n,2)
  i<- 1

  datacorrFake <- matrix(NA,n,101)
    
  for (p in unique(daily$pid)) {
    # if (p == 'vxx') next
    ## if pid in line, then change j index in for loop to 2:101
    datacorrFake[i,1] <- p
    datacorr[i,1] <- p
    # run auto 100 instances for each pid using the time shuffled data
    for (j in (2:101)) {
      tData <- daily[,m][daily$pid == p]
      tlen <- length(tData)
      tTimeRand <- order(runif(tlen,0,1))
      tDatab <- tData[tTimeRand]
      datacorrFake[i,j] <- cor(tDatab[1:(tlen-1)],tDatab[2:tlen])
    }
    
    datacorr[i,2] <- cor(tData[1:(tlen-1)],tData[2:tlen])
    i <- i+1
  }

  # extract each row as a vector then order fake in ascending order and get 95th percentile
  # compare real datacorr to the 95th - if its outside, can reject null with 95 confidence
  # if its inside then fail to reject
  for (i in (1:n)) {
    temp <- as.numeric(sort(datacorrFake[i,(2:101)]))
    q95 <- quantile(temp,.95)
    pids[i,2] <- q95
  }

  for (i in (1:n)) {
    temp <- as.numeric(sort(datacorrFake[i,(2:101)]))
    q05 <- quantile(temp,.05)
    pids[i,3] <- q05
  }

  colnames(datacorr) <- c('pid','realCor')
  colnames(pids) <- c('pid','tRandCor_95','tRandCor_05')

  corComp <- merge(datacorr, pids, by='pid')
  corComp$realCor <- as.numeric(corComp$realCor)
  corComp$greater95 <- corComp$realCor > corComp$tRandCor_95
  corComp$less05 <- corComp$realCor < corComp$tRandCor_05

  # assign the copied data frame to a new variable name
  assign(paste0(m, "_corComp"), corComp)
}


####################################
# GLLA
# ----------------------------------
# The first thing you need to do is to extract the within person means so that everyone is centered around zero, 
# an estimate of their equilibrium value.  The easiest way to do this is to fit an random intercept lme() model to the data and save the residuals. 
t1.duration <- c(residuals(lme(duration ~  1, random=list( ~ 1 | pid), data= daily)))
t1.activity <- c(residuals(lme(activity ~  1, random=list( ~ 1 | pid), data= daily)))
t1.stress <- c(residuals(lme(stress ~  1, random=list( ~ 1 | pid), data= daily)))
t1.productivity <- c(residuals(lme(productivity ~  1, random=list( ~ 1 | pid), data= daily)))
# ----------------------------------
theTau <- 1
deltaT <- 1.6387
embedD <- 4

totalSamples <- n
totalInterval <- totalSamples * deltaT

theTimes  <- seq(0, totalInterval, length=totalSamples)  # the measurement occasions
# ----------------------------------
# Embed the data and calculate the derivatives.
tEmbed.dur <- gllaEmbed(t1.duration, embed=embedD, tau=theTau, groupby=daily$pid, label='x', idColumn=TRUE)
tEmbed.act <- gllaEmbed(t1.activity, embed=embedD, tau=theTau, groupby=daily$pid, label='x', idColumn=TRUE)
tEmbed.stress <- gllaEmbed(t1.stress, embed=embedD, tau=theTau, groupby=daily$pid, label='x', idColumn=TRUE)
tEmbed.prod <- gllaEmbed(t1.productivity, embed=embedD, tau=theTau, groupby=daily$pid, label='x', idColumn=TRUE)

tEmbed.pids <- as.data.frame(tEmbed.dur[,1])
names(tEmbed.pids) <- 'pid'

tEmbed.dur <- subset(tEmbed.dur, select = -c(ID))
tEmbed.dur <- matrix(as.numeric(tEmbed.dur), ncol=ncol(tEmbed.dur))

tEmbed.act <- subset(tEmbed.act, select = -c(ID))
tEmbed.act <- matrix(as.numeric(tEmbed.act), ncol=ncol(tEmbed.act))

tEmbed.stress <- subset(tEmbed.stress, select = -c(ID))
tEmbed.stress <- matrix(as.numeric(tEmbed.stress), ncol=ncol(tEmbed.stress))

tEmbed.prod <- subset(tEmbed.prod, select = -c(ID))
tEmbed.prod <- matrix(as.numeric(tEmbed.prod), ncol=ncol(tEmbed.prod))

# ----------------------------------
# Calculate and print the W matrix.
theOrder <- 2
wMatrix <- gllaWMatrix(embed=embedD, tau=theTau, deltaT=deltaT, order=theOrder)
wMatrix
# ----------------------------------
# GLLAEstimates <- tEmbed.dur %*% wMatrix
# # dimnames(GLLAEstimates) <- list(NULL, c("x", "dx/dt"))
# dimnames(GLLAEstimates) <- list(NULL, c("x", "dx/dt", "dx^2/dt^2"))

GLLAEst.dur <- tEmbed.dur %*% wMatrix
dimnames(GLLAEst.dur) <- list(NULL, c("slp.x", "slp.dx", "slp.d2x"))
GLLAdf.dur <- data.frame(pid=tEmbed.pids$pid, slp.x=GLLAEst.dur[,1], slp.dx=GLLAEst.dur[,2], slp.d2x=GLLAEst.dur[,3])

GLLAEst.act <- tEmbed.act %*% wMatrix
dimnames(GLLAEst.act) <- list(NULL, c("act.x", "act.dx", "act.d2x"))
GLLAdf.act <- data.frame(pid=tEmbed.pids$pid, act.x=GLLAEst.act[,1], act.dx=GLLAEst.act[,2], act.d2x=GLLAEst.act[,3])

GLLAEst.stress <- tEmbed.stress %*% wMatrix
dimnames(GLLAEst.stress) <- list(NULL, c("stress.x", "stress.dx", "stress.d2x"))
GLLAdf.stress <- data.frame(pid=tEmbed.pids$pid, stress.x=GLLAEst.stress[,1], stress.dx=GLLAEst.stress[,2], stress.d2x=GLLAEst.stress[,3])

GLLAEst.prod <- tEmbed.prod %*% wMatrix
dimnames(GLLAEst.prod) <- list(NULL, c("prod.x", "prod.dx", "prod.d2x"))
GLLAdf.prod <- data.frame(pid=tEmbed.pids$pid, prod.x=GLLAEst.prod[,1], prod.dx=GLLAEst.prod[,2], prod.d2x=GLLAEst.prod[,3])

multiEst <- cbind(GLLAEst.dur,GLLAEst.act,GLLAEst.stress,GLLAEst.prod)
multiGLLAdf <- cbind(GLLAdf.dur,GLLAdf.act,GLLAdf.stress,GLLAdf.prod)

# midSample <- ((embedD - 1) / 2) * theTau

# ----------------------------------
# Pairs plot of the estimated derivatives.

pairs(multiEst, cex=.5, main="Coupled System")

png("Prod2GLLAPairs.png")
pairs(GLLAEst.prod, cex=.5, main="Productivity")
dev.off()

# ----------------------------------
# # fit a linear model to the GLLA estimates.
# # Next you will need to add the ID column back into the GLLAEstimates to get ready for an lme() style multilevel model of the dynamics
# # GLLAdataframe<- data.frame(pid=tEmbed.prod[,1], x=GLLAEstimates[,1], dx=GLLAEstimates[,2])
# GLLAdataframe <- data.frame(pid=tEmbed.prod[,1], x=multiGLLAdf[,1], dx=multiGLLAdf[,2], d2x=multiGLLAdf[,3])
# 
# # Now you are ready to model the first order dynamics with each person potentially having their own parameters.
# tFit <- lme(dx ~ x - 1, random = list(~x - 1 | pid), data= GLLAdataframe)
# summary(tFit)
# 
# # The model you fit constrains everyone to have the same stability parameter.
# # The same applies to the second order model like this
# # tFit <- lme(d2x ~ x + dx - 1, random = list(~x + dx - 1 | pid), data= GLLAdataframe)
tFit <- lme(prod.d2x ~ prod.x + prod.dx - 1, random = list(~prod.x + prod.dx - 1 | pid), data= GLLAdf.prod, control = lmeControl(opt='optim'))
summary(tFit)

sink("lme_prod.txt")
print("Productivity")
print(summary(tFit))
sink()
closeAllConnections()

# slp.d2x ~ slp.x + prod.x + act.x + stress.x) + (slp.dx + prod.dx + act.dx + stress.dx) - 1, random=list( ~ slp.x + slp.dx)

tFit.slp <- lme(slp.d2x ~ (slp.x + prod.x + act.x + stress.x) + (slp.dx + prod.dx + act.dx + stress.dx) - 1, 
            random = list(~slp.x + slp.dx  - 1 | pid), data= multiGLLAdf, control = lmeControl(opt='optim'))
summary(tFit.slp) 

tFit.act <- lm(act.d2x ~ (act.x + prod.x + act.x + stress.x) + (slp.dx + prod.dx + act.dx + stress.dx) - 1, data= multiGLLAdf)
summary(tFit.act)

tFit.stress <- lme(stress.d2x ~ (act.x + prod.x + act.x + stress.x) + (slp.dx + prod.dx + act.dx + stress.dx) - 1, 
                random = list(~ stress.x + stress.dx - 1 | pid), data= multiGLLAdf, control = lmeControl(opt='optim'))
summary(tFit.stress)

tFit.prod <- lme(prod.d2x ~ (act.x + prod.x + act.x + stress.x) + (slp.dx + prod.dx + act.dx + stress.dx) - 1, 
                   random = list(~ prod.x + prod.dx - 1 | pid), data= multiGLLAdf, control = lmeControl(opt='optim'))
summary(tFit.prod)

sink("CoupledLME.txt")
print(summary(tFit.slp))
print(summary(tFit.act))
print(summary(tFit.stress))
print(summary(tFit.prod))
sink()
####################################
# GLLA by pid
# ----------------------------------
daily_pid <- split(daily, f=daily$pid)
pids = as.data.frame(unique(daily$pid))
names(pids) <- "pid"

pid_deltaT <- read.csv("deltaT.csv", header=TRUE, colClasses = c('character','numeric'))

# ----------------------------------
theTau <- 1
embedD <- 4
theOrder <- 2
# ----------------------------------
setwd("~/Documents/PhD_Research/LLL-Wellbeing/spring22/glla_pid2/activity")
actLM <- list()

for (i in 1:length(daily_pid)) {
  # get deltaT for the specific pid
  if (names(daily_pid)[i] == pid_deltaT$pid[i]) {
    pdeltaT <- pid_deltaT$deltaT[i]
  }
  
  pidN <- dim(daily_pid[[i]])[1]
  totalSamples <- pidN
  totalInterval <- totalSamples * pdeltaT
  theTimes  <- seq(0, totalInterval, length=totalSamples)  # the measurement occasions
  
  tpid.duration <- c(residuals(lm(duration~1, data= daily_pid[[i]])))
  tpid.activity <- c(residuals(lm(activity ~  1, data= daily_pid[[i]])))
  tpid.stress <- c(residuals(lm(stress ~  1, data= daily_pid[[i]])))
  tpid.productivity <- c(residuals(lm(productivity ~  1, data= daily_pid[[i]],)))
  
  # Embed the data and calculate the derivatives.
  tEmbed.dur <- gllaEmbed(tpid.duration, embed=embedD, tau=theTau, label='x', idColumn=FALSE)
  tEmbed.act <- gllaEmbed(tpid.activity, embed=embedD, tau=theTau, label='x', idColumn=FALSE)
  tEmbed.stress <- gllaEmbed(tpid.stress, embed=embedD, tau=theTau, label='x', idColumn=FALSE)
  tEmbed.prod <- gllaEmbed(tpid.productivity, embed=embedD, tau=theTau, label='x', idColumn=FALSE)
  
  # ----------------------------------
  # Calculate and print the W matrix.
  wMatrix <- gllaWMatrix(embed=embedD, tau=theTau, deltaT=pdeltaT, order=theOrder)
  # ----------------------------------
  GLLAEstimates <- tEmbed.act %*% wMatrix
  # dimnames(GLLAEstimates) <- list(NULL, c("x", "dx/dt"))
  dimnames(GLLAEstimates) <- list(NULL, c("x", "dx/dt", "dx^2/dt^2"))
  
  GLLAdataframe <- data.frame(x=GLLAEstimates[,1], dx=GLLAEstimates[,2], d2x=GLLAEstimates[,3])
  # midSample <- ((embedD - 1) / 2) * theTau
  # ----------------------------------
  # # Pairs plot of the estimated derivatives.
  # fname <- paste(names(daily_pid)[i],"Prod2GLLAPairs.png")
  # fname<-gsub(" ","",fname)
  # 
  # ptitle <- paste(names(daily_pid)[i], "- Productivity")
  # png(fname)
  # pairs(GLLAEstimates, cex=.5, main = ptitle)
  # dev.off()
  
  tLM <- lm(d2x ~ x + dx - 1, data= GLLAdataframe)
  actLM[[names(daily_pid)[i]]] <- summary(tLM)
}

# ----------------------------------
for (i in 1:length(pids)) {
  pids$deltaT[i] <- pid_deltaT$deltaT[i]
  pids$slp_rsq[i] <- slpLM[[pids$pid[i]]]$r.squared
  pids$slp_adjr[i] <- slpLM[[pids$pid[i]]]$adj.r.squared
  pids$slp_p[i] <- pids$deltaT[i]/slpLM[[pids$pid[i]]]$coefficients[1]
  
  pids$act_rsq[i] <- actLM[[pids$pid[i]]]$r.squared
  pids$act_adjr[i] <- actLM[[pids$pid[i]]]$adj.r.squared
  pids$act_p[i] <- pids$deltaT[i]/actLM[[pids$pid[i]]]$coefficients[1]
  
  pids$stress_rsq[i] <- stressLM[[pids$pid[i]]]$r.squared
  pids$stress_adjr[i] <- stressLM[[pids$pid[i]]]$adj.r.squared
  pids$stress_p[i] <- pids$deltaT[i]/stressLM[[pids$pid[i]]]$coefficients[1]
  
  pids$prod_rsq[i] <- prodLM[[pids$pid[i]]]$r.squared
  pids$prod_adjr[i] <- prodLM[[pids$pid[i]]]$adj.r.squared
  pids$prod_p[i] <- pids$deltaT[i]/prodLM[[pids$pid[i]]]$coefficients[1]
}

# ----------------------------------
setwd("~/Documents/PhD_Research/LLL-Wellbeing/spring22/glla_pid2")

sink("pidLM_slp.txt")
print("Sleep")
for (i in 1:length(slpLM)) {
  print(names(slpLM[i]))
  print(slpLM[[i]])
}
sink()

sink("pidLM_act.txt")
print("Activity")
for (i in 1:length(actLM)) {
  print(names(actLM[i]))
  print(actLM[[i]])
}
sink()

sink("pidLM_stress.txt")
print("Stress")
for (i in 1:length(stressLM)) {
  print(names(stressLM[i]))
  print(stressLM[[i]])
}
sink()


sink("pidLM_prod.txt")
print("Productivity")
for (i in 1:length(prodLM)) {
  print(names(prodLM[i]))
  print(prodLM[[i]])
}
sink()

