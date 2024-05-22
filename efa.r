# ----------------------------------
# Read libraries and set options.

setwd("~/Library/CloudStorage/GoogleDrive-blx2wj@virginia.edu/Other\ computers/My\ Mac/Documents/PhD_Research/GitHub/well-being")
setwd("~/Documents/PhD_Research/GitHub/well-being")
options(width=80)
Sys.setenv(OMP_NUM_THREADS=parallel::detectCores())

library(dplyr)
library(psych)
library(OpenMx)
library("GPArotation")
mxOption(NULL, 'Number of Threads', parallel::detectCores())

# ----------------------------------
# Read the data and run an EFA.

data <- read.csv("daily_measures_imputed.csv") %>% rename(ID = pid)
# drop wakeDate and duration column
data <- subset(data, select = -c(wakeDate, duration, sleep))

describe(data)

# scree(data[,-1])
# 
# tout1 <- fa(data[,-1], nfactors=1)
# print(tout1, sort=TRUE)
# 
# tout2 <- fa(data[,-1], nfactors=2, rotate="promax")
# print(tout2, sort=TRUE)

# ----------------------------------
# Create Autoregressive model with time invariant autoregression parameter.
manifests <- names(data)[2:dim(data)[2]]
numVars <- length(manifests)
predictors <- manifests[1:(numVars-1)]
outcomes <- manifests[2:numVars]

arModel1 <- mxModel(model="AR_Model_1",
                    type="RAM",
                    manifestVars=manifests,
                    mxPath(from=predictors, to=outcomes, arrows=1, free=TRUE, values=.2, labels="b"),
                    mxPath(from=manifests[1], arrows=2, free=TRUE, values=.8, labels="VarX"),
                    mxPath(from=outcomes, arrows=2, free=TRUE, values=.8, labels="VarE"),
                    mxPath(from="one", to=manifests, arrows=1, free=FALSE, values=0, labels="MeanX"),
                    mxData(observed=data, type="raw")
)

arModel1Out <- mxRun(arModel1)

summary(arModel1Out)


