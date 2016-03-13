gc()
library(caret)
library(base)
library(doParallel); cl <- makeCluster(4); registerDoParallel(cl) 

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

#PCA_based_training <- funktion(data = data, thres = thres)
#{

  #label <- factor(c(label, recursive = TRUE))  -  Single person Labels size
  #trainingDigit <- data.frame(do.call(rbind, trainingDigit)) - Convert Troels version to Dataframe
  #label <- c(label,label,label,label,label,label,label,label,label,label,label,label,label,label,label)

  label_0 = unlist(list(label[[1]],label[[1]],label[[1]],label[[1]],label[[1]],label[[1]],label[[1]],label[[1]],label[[1]],label[[1]],label[[1]],label[[1]],label[[1]],label[[1]],label[[1]]))
  label_1 = unlist(list(label[[2]],label[[2]],label[[2]],label[[2]],label[[2]],label[[2]],label[[2]],label[[2]],label[[2]],label[[2]],label[[2]],label[[2]],label[[2]],label[[2]],label[[2]]))
  label_2 = unlist(list(label[[3]],label[[3]],label[[3]],label[[3]],label[[3]],label[[3]],label[[3]],label[[3]],label[[3]],label[[3]],label[[3]],label[[3]],label[[3]],label[[3]],label[[3]]))
  label_3 = unlist(list(label[[4]],label[[4]],label[[4]],label[[4]],label[[4]],label[[4]],label[[4]],label[[4]],label[[4]],label[[4]],label[[4]],label[[4]],label[[4]],label[[4]],label[[4]]))
  label_4 = unlist(list(label[[5]],label[[5]],label[[5]],label[[5]],label[[5]],label[[5]],label[[5]],label[[5]],label[[5]],label[[5]],label[[5]],label[[5]],label[[5]],label[[5]],label[[5]]))
  label_5 = unlist(list(label[[6]],label[[6]],label[[6]],label[[6]],label[[6]],label[[6]],label[[6]],label[[6]],label[[6]],label[[6]],label[[6]],label[[6]],label[[6]],label[[6]],label[[6]]))
  label_6 = unlist(list(label[[7]],label[[7]],label[[7]],label[[7]],label[[7]],label[[7]],label[[7]],label[[7]],label[[7]],label[[7]],label[[7]],label[[7]],label[[7]],label[[7]],label[[7]]))
  label_7 = unlist(list(label[[8]],label[[8]],label[[8]],label[[8]],label[[8]],label[[8]],label[[8]],label[[8]],label[[8]],label[[8]],label[[8]],label[[8]],label[[8]],label[[8]],label[[8]]))
  label_8 = unlist(list(label[[9]],label[[9]],label[[9]],label[[9]],label[[9]],label[[9]],label[[9]],label[[9]],label[[9]],label[[9]],label[[9]],label[[9]],label[[9]],label[[9]],label[[9]]))
  label_9 = unlist(list(label[[10]],label[[10]],label[[10]],label[[10]],label[[10]],label[[10]],label[[10]],label[[10]],label[[10]],label[[10]],label[[10]],label[[10]],label[[10]],label[[10]],label[[10]]))
  bigLabel =  list(label_0,label_1,label_2,label_3,label_4,label_5,label_6,label_7,label_8,label_9)
  str(bigLabel)
  
  bigLabel <- factor(c(bigLabel, recursive = TRUE)) #Labels for multiple persons. 
  trainingDigit <- data.frame(do.call(rbind, c(List_1_1,List_1_2,List_2_1,List_2_2,List_3_1,List_4_1,List_4_2,List_4_3,List_5_1,List_5_2,List_6_1,List_7_1,List_8_1,List_12_1,List_13_1)))  
  
  k_list=expand.grid(k=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
  
  
  control <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 10,
                          preProcOptions = list(thresh = 0.8),
                          p = 0.9)

  str(label)
  str(trainingDigit)

  knnFit <- train(x = trainingDigit, # trainingDigit = single person , bigTrainingDigit = All people
                y = bigLabel, # bigLabel  =  all people , Label =  SinglePerson
                method = "knn",
                tuneGrid = k_list, 
                trControl = control,
                preProcess = "pca")

  knnFit$results
  
  
#}