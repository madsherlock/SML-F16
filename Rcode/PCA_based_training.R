gc()
library(caret)
library(doParallel); cl <- makeCluster(4); registerDoParallel(cl) 

setwd("~/Dropbox/Eksamen/SML/SML-F16/Rcode")
load("../data/data.RData")
load("../data/testClass_new.RData")


label <- factor(c(label, recursive = TRUE))
trainingDigit <- data.frame(do.call(rbind, trainingDigit))

k_list=expand.grid(k=c(1,2))


control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 10,
                        preProcOptions = list(thresh = 0.8),
                        p = 0.9)

str(label)
str(trainingDigit)
knnFit <- train(x = trainingDigit,
                y = label,
                method = "knn",
                tuneGrid = k_list, 
                trControl = control,
                preProcess = "pca")

knnFit$results
