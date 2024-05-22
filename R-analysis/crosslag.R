

setwd("~/Google Drive/Other computers/My Mac/Documents/PhD_Research/GitHub/well-being/R-analysis")

# ----------------------------------
# Read libraries and set options.

options(width=80)
Sys.setenv(OMP_NUM_THREADS=parallel::detectCores())

library(dplyr)
library(psych)
library(OpenMx)
mxOption(NULL, 'Number of Threads', parallel::detectCores())
source("GLLAfunctions.R")

# ----------------------------------
# Read the data.
data <- read.csv("padded_data_transformed.csv")
describe(data)

dataTST <- read.csv("padded_data_transformed_TST.csv")
describe(dataTST)
#cor(data)

# ----------------------------------
# Create Full Crosslag model with time invariant parameters.

manifestsX <- paste("X",1:58,sep="")
manifestsY <- paste("Y",1:58,sep="")
manifestsZ <- paste("Z",1:58,sep="")

predictorsX <- paste("X",1:57,sep="")
outcomesX <- paste("X",2:58,sep="")
predictorsY <- paste("Y",1:57,sep="")
outcomesY <- paste("Y",2:58,sep="")
predictorsZ <- paste("Z",1:57,sep="")
outcomesZ <- paste("Z",2:58,sep="")


crossModelFull <- mxModel(model = "Full_Crosslag",
                          type = "RAM",
                          manifestVars = c(manifestsX, manifestsY, manifestsZ),
                          mxPath(from = predictorsX, to = outcomesX, arrows = 1, free = TRUE, values = .2, labels = "bX"),
                          mxPath(from = predictorsY, to = outcomesY, arrows = 1, free = TRUE, values = .2, labels = "bY"),
                          mxPath(from = predictorsZ, to = outcomesZ, arrows = 1, free = TRUE, values = .2, labels = "bZ"),
                          mxPath(from = predictorsX, to = outcomesY, arrows = 1, free = TRUE, values = .2, labels = "bXY"),
                          mxPath(from = predictorsY, to = outcomesX, arrows = 1, free = TRUE, values = .2, labels = "bYX"),
                          mxPath(from = predictorsX, to = outcomesZ, arrows = 1, free = TRUE, values = .2, labels = "bXZ"),
                          mxPath(from = predictorsY, to = outcomesZ, arrows = 1, free = TRUE, values = .2, labels = "bYZ"),
                          mxPath(from = predictorsZ, to = outcomesX, arrows = 1, free = TRUE, values = .2, labels = "bZX"),
                          mxPath(from = predictorsZ, to = outcomesY, arrows = 1, free = TRUE, values = .2, labels = "bZY"),
                          mxPath(from=c(manifestsX[1],manifestsY[1],manifestsZ[1]), to=c(manifestsX[1],manifestsY[1],manifestsZ[1]), arrows=2, free=TRUE, values=.2, connect="unique.bivariate"),
                          mxPath(from=outcomesX, arrows=2, free=TRUE, values=.8, labels="VarEX"),
                          mxPath(from=outcomesY, arrows=2, free=TRUE, values=.8, labels="VarEY"),
                          mxPath(from=outcomesZ, arrows=2, free=TRUE, values=.8, labels="VarEZ"),
                          mxPath(from = manifestsX[1], arrows = 2, free = TRUE, values = .5, labels = "VarX"),
                          mxPath(from = manifestsY[1], arrows = 2, free = TRUE, values = .5, labels = "VarY"),
                          mxPath(from = manifestsZ[1], arrows = 2, free = TRUE, values = .5, labels = "VarZ"),
                          mxPath(from = "one", to = manifestsX, arrows = 1, free = TRUE, values = 0, labels = "MeanX"),
                          mxPath(from = "one", to = manifestsY, arrows = 1, free = TRUE, values = 0, labels = "MeanY"),
                          mxPath(from = "one", to = manifestsY, arrows = 1, free = TRUE, values = 0, labels = "MeanZ"),
                          mxData(observed = data, type = "raw"),
                          mxCI(c("bX","bY","bZ","bXZ"))
)
crossModelFullOut <- mxTryHard(crossModelFull,intervals = TRUE)
# tRefModel <- mxRefModels(crossModelFullOut, run=TRUE) 
summary(crossModelFullOut)



crossModelFull2 <- mxModel(model = "Full_Crosslag2",
                          type = "RAM",
                          manifestVars = c(manifestsX, manifestsY, manifestsZ),
                          mxPath(from = predictorsX, to = outcomesX, arrows = 1, free = TRUE, values = .2, labels = "bX"),
                          mxPath(from = predictorsY, to = outcomesY, arrows = 1, free = TRUE, values = .2, labels = "bY"),
                          mxPath(from = predictorsZ, to = outcomesZ, arrows = 1, free = TRUE, values = .2, labels = "bZ"),
                          mxPath(from = predictorsX, to = outcomesY, arrows = 1, free = TRUE, values = .2, labels = "bXY"),
                          mxPath(from = predictorsY, to = outcomesX, arrows = 1, free = TRUE, values = .2, labels = "bYX"),
                          mxPath(from = predictorsX, to = outcomesZ, arrows = 1, free = TRUE, values = .2, labels = "bXZ"),
                          mxPath(from = predictorsY, to = outcomesZ, arrows = 1, free = TRUE, values = .2, labels = "bYZ"),
                          mxPath(from = predictorsZ, to = outcomesX, arrows = 1, free = TRUE, values = .2, labels = "bZX"),
                          mxPath(from = predictorsZ, to = outcomesY, arrows = 1, free = TRUE, values = .2, labels = "bZY"),
                          mxPath(from=c(manifestsX[1],manifestsY[1],manifestsZ[1]), to=c(manifestsX[1],manifestsY[1],manifestsZ[1]), arrows=2, free=TRUE, values=.2, connect="unique.bivariate"),
                          mxPath(from=outcomesX, arrows=2, free=TRUE, values=.8, labels="VarEX"),
                          mxPath(from=outcomesY, arrows=2, free=TRUE, values=.8, labels="VarEY"),
                          mxPath(from=outcomesZ, arrows=2, free=TRUE, values=.8, labels="VarEZ"),
                          mxPath(from = manifestsX[1], arrows = 2, free = TRUE, values = .5, labels = "VarX"),
                          mxPath(from = manifestsY[1], arrows = 2, free = TRUE, values = .5, labels = "VarY"),
                          mxPath(from = manifestsZ[1], arrows = 2, free = TRUE, values = .5, labels = "VarZ"),
                          mxPath(from = "one", to = manifestsX, arrows = 1, free = TRUE, values = 0, labels = "MeanX"),
                          mxPath(from = "one", to = manifestsY, arrows = 1, free = TRUE, values = 0, labels = "MeanY"),
                          mxPath(from = "one", to = manifestsY, arrows = 1, free = TRUE, values = 0, labels = "MeanZ"),
                          mxData(observed = dataTST, type = "raw"),
                          mxCI(c("bX","bY","bZ","bXZ"))
)
crossModelFullOut2 <- mxTryHard(crossModelFull2,intervals = TRUE)
# tRefModel <- mxRefModels(crossModelFullOut, run=TRUE) 
summary(crossModelFullOut2)

# X = "REM:nREM", Y = "steps_std", Z = "stress"
data3 <- read.csv("stress_data_crosslag.csv")
describe(data3)
crossModelFull3 <- mxModel(model = "Full_Crosslag3",
                          type = "RAM",
                          manifestVars = c(manifestsX, manifestsY, manifestsZ),
                          mxPath(from = predictorsX, to = outcomesX, arrows = 1, free = TRUE, values = .2, labels = "bX"),
                          mxPath(from = predictorsY, to = outcomesY, arrows = 1, free = TRUE, values = .2, labels = "bY"),
                          mxPath(from = predictorsZ, to = outcomesZ, arrows = 1, free = TRUE, values = .2, labels = "bZ"),
                          mxPath(from = predictorsX, to = outcomesY, arrows = 1, free = TRUE, values = .2, labels = "bXY"),
                          mxPath(from = predictorsY, to = outcomesX, arrows = 1, free = TRUE, values = .2, labels = "bYX"),
                          mxPath(from = predictorsX, to = outcomesZ, arrows = 1, free = TRUE, values = .2, labels = "bXZ"),
                          mxPath(from = predictorsY, to = outcomesZ, arrows = 1, free = TRUE, values = .2, labels = "bYZ"),
                          mxPath(from = predictorsZ, to = outcomesX, arrows = 1, free = TRUE, values = .2, labels = "bZX"),
                          mxPath(from = predictorsZ, to = outcomesY, arrows = 1, free = TRUE, values = .2, labels = "bZY"),
                          mxPath(from=c(manifestsX[1],manifestsY[1],manifestsZ[1]), to=c(manifestsX[1],manifestsY[1],manifestsZ[1]), arrows=2, free=TRUE, values=.2, connect="unique.bivariate"),
                          mxPath(from=outcomesX, arrows=2, free=TRUE, values=.8, labels="VarEX"),
                          mxPath(from=outcomesY, arrows=2, free=TRUE, values=.8, labels="VarEY"),
                          mxPath(from=outcomesZ, arrows=2, free=TRUE, values=.8, labels="VarEZ"),
                          mxPath(from = manifestsX[1], arrows = 2, free = TRUE, values = .5, labels = "VarX"),
                          mxPath(from = manifestsY[1], arrows = 2, free = TRUE, values = .5, labels = "VarY"),
                          mxPath(from = manifestsZ[1], arrows = 2, free = TRUE, values = .5, labels = "VarZ"),
                          mxPath(from = "one", to = manifestsX, arrows = 1, free = TRUE, values = 0, labels = "MeanX"),
                          mxPath(from = "one", to = manifestsY, arrows = 1, free = TRUE, values = 0, labels = "MeanY"),
                          mxPath(from = "one", to = manifestsY, arrows = 1, free = TRUE, values = 0, labels = "MeanZ"),
                          mxData(observed = data3, type = "raw"),
                          mxCI(c("bX","bY","bZ","bXZ"))
)
crossModelFullOut3 <- mxTryHard(crossModelFull3,intervals = TRUE)
# tRefModel <- mxRefModels(crossModelFullOut, run=TRUE) 
summary(crossModelFullOut3)


mxCompare(crossModelFullOut, crossModelFullOut3)
