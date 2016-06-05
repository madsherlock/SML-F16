source("loadRData.R")
library("e1071")
library("klaR")
library("lattice")
library("Hmisc")
library("binr")
library("stats")
library("caret")
library("doMC")
library("base")
registerDoMC(cores=4)
setwd("~/Dropbox/Eksamen/SML/SML-F16/Rcode")

all =  loadRData("../data/labeled_datasets/data-labeled-1-1-1-2-2-1-2-2-3-1-3-2-4-1-4-2-4-3-5-1-5-2-5-3-6-1-7-1-8-1-10-1-10-2-11-1-11-2-11-3-13-1-14-1-14-2-dpi100-sigma1.RData")
G2M2 = loadRData("../data/labeled_datasets/data-labeled-2-2-dpi100-sigma1.5.RData")

ctrl <- trainControl(method = "repeatedcv", number = 10 , savePred=T, classProb=T, verboseIter = T, preProcOptions = list(thresh = 0.95))
start = proc.time()
start
mod <- train(x = all$data, y = all$labels, method = "svmPoly", trControl = ctrl, preProcess = "pca")
mod$preProcess
slut = proc.time() 
print(mod)
plot(mod)
