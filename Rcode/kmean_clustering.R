gc()
library(caret)
library(doParallel); cl <- makeCluster(4); registerDoParallel(cl) 

# Data loading 
setwd("~/Dropbox/Eksamen/SML/SML-F16/Rcode")
load("../data/data.RData")
load("../data/testClass_new.RData")
load("../data/data-1-1-100-1.5.RData")
List_1_1 = trainingDigit
load("../data/data-1-2-100-1.5.RData")
List_1_2 = trainingDigit
load("../data/data-2-1-100-1.5.RData")
List_2_1 = trainingDigit
load("../data/data-2-2-100-1.5.RData")
List_2_2 = trainingDigit
load("../data/data-3-1-100-1.5.RData")
List_3_1 = trainingDigit
load("../data/data-4-1-100-1.5.RData")
List_4_1 = trainingDigit
load("../data/data-4-2-100-1.5.RData")
List_4_2 = trainingDigit
load("../data/data-4-3-100-1.5.RData")
List_4_3 = trainingDigit
load("../data/data-5-1-100-1.5.RData")
List_5_1 = trainingDigit
load("../data/data-5-2-100-1.5.RData")
List_5_2 = trainingDigit
load("../data/data-6-1-100-1.5.RData")
List_6_1 = trainingDigit
load("../data/data-7-1-100-1.5.RData")
List_7_1 = trainingDigit
load("../data/data-8-1-100-1.5.RData")
List_8_1 = trainingDigit
load("../data/data-12-1-100-1.5.RData")
List_12_1 = trainingDigit
load("../data/data-13-1-100-1.5.RData")
List_13_1 = trainingDigit