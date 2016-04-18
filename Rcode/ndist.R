##Naive Based Lolll
source("loadRData.R")
library("e1071")
library("klaR")
library("lattice")
library("Hmisc")
library("binr")
library("stats")
library("caret")
library("rms")
#library("doMC"); registerDoMC(cores = 4);

#model <- NaiveBayes(x = G2M1$data, grouping = G2M1$labels, usekernel = FALSE)
#pred <- predict(model,newdata = G2M2$data[1:400,])




G2M1 = loadRData("../data/labeled_datasets/data-labeled-2-1-dpi100-sigma1-6.RData")
G2M2 = loadRData("../data/labeled_datasets/data-labeled-2-2-dpi100-sigma1-6.RData")

G2M1$data[1:400,2]

length(dnorm(x,mean(x),sd(x)))

plot(x,dnorm(x,mean(x),sd(x)), type="l", col = rainbow(324))
