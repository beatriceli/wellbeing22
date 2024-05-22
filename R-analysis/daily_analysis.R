library('xts')
library('reshape2')
library('ggplot2')
library(tidyverse)
library(lubridate)

setwd("~/Documents/PhD_Research/LLL-Wellbeing/spring22")

df<-read.csv("dailySleep.csv", header=TRUE, colClasses = c('character','numeric'))

pids = as.data.frame(unique(df$pid))

df %>% group_by(df$pid) %>% count()

n = nrow(pids)
## time shuffle
datacorr <- matrix(NA,n,2)
i<- 1

datacorrFake <- matrix(NA,n,101)
  
for (p in unique(df$pid)) {
  ## if pid in line, then change j index in for loop to 2:101
  datacorrFake[i,1] <- p
  datacorr[i,1] <- p
  # run auto 100 instances for each pid using the time shuffled data
  for (j in (2:101)) {
    tData <- df$Q23_1[df$pid == p]
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





