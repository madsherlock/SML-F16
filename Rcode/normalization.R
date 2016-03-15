gc()
library(caret)
library(doParallel); cl <- makeCluster(4); registerDoParallel(cl) 
#--Normalization
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
#-------------------------------------------
#min-max normalization
# z_i =  (x_i - min(x))/max(x)-min(x)

#Normalizing One dataset:
normalized_1_1_0 = (List_1_1[[1]]-min(List_1_1[[1]]))/(max(List_1_1[[1]])-min(List_1_1[[1]]))
normalized_1_1_1 = (List_1_1[[2]]-min(List_1_1[[2]]))/(max(List_1_1[[2]])-min(List_1_1[[2]]))
normalized_1_1_2 = (List_1_1[[3]]-min(List_1_1[[3]]))/(max(List_1_1[[3]])-min(List_1_1[[3]]))
normalized_1_1_3 = (List_1_1[[4]]-min(List_1_1[[4]]))/(max(List_1_1[[4]])-min(List_1_1[[4]]))
normalized_1_1_4 = (List_1_1[[5]]-min(List_1_1[[5]]))/(max(List_1_1[[5]])-min(List_1_1[[5]]))
normalized_1_1_5 = (List_1_1[[6]]-min(List_1_1[[6]]))/(max(List_1_1[[6]])-min(List_1_1[[6]]))
normalized_1_1_6 = (List_1_1[[7]]-min(List_1_1[[7]]))/(max(List_1_1[[7]])-min(List_1_1[[7]]))
normalized_1_1_7 = (List_1_1[[8]]-min(List_1_1[[8]]))/(max(List_1_1[[8]])-min(List_1_1[[8]]))
normalized_1_1_8 = (List_1_1[[9]]-min(List_1_1[[9]]))/(max(List_1_1[[9]])-min(List_1_1[[9]]))
normalized_1_1_9 = (List_1_1[[10]]-min(List_1_1[[10]]))/(max(List_1_1[[10]])-min(List_1_1[[10]]))

#For multiple do the same as above for multiple
#combine each set using 
# Normalization_0 = unlist(list(....))

#combine dataset to Troels format. 
# To train with it use PCA_BASED_TRAINING.r 
normalized =  list(normalized_1_1_0,normalized_1_1_1,normalized_1_1_2,normalized_1_1_3,normalized_1_1_4,normalized_1_1_5,normalized_1_1_6,normalized_1_1_7,normalized_1_1_8,normalized_1_1_9)
#outputs data normalized using Minmax and return into Troels's dataformat. 
