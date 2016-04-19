##Naive Based Lolll
source("loadRData.R")
source("OVL.R")
library("e1071")
library("klaR")
library("lattice")
library("Hmisc")
library("binr")
library("stats")
library("caret")
library("rms")
library("fitdistrplus")
library("overlap")
#library("doMC"); registerDoMC(cores = 4);

#model <- NaiveBayes(x = G2M1$data, grouping = G2M1$labels, usekernel = FALSE)
#pred <- predict(model,newdata = G2M2$data[1:400,])




G2M1 = loadRData("../data/labeled_datasets/data-labeled-2-1-dpi100-sigma1-6.RData")
G2M2 = loadRData("../data/labeled_datasets/data-labeled-2-2-dpi100-sigma1-6.RData")
g_fewer  = loadRData("../data/labeled_datasets/data-labeled-1-1-2-1-3-1-4-1-5-1-6-1-7-1-8-1-11-1-13-1-dpi100-sigma1.6.RData")



x<-G2M1$data[1:400,2]
y<-G2M2$data[1:400,2] 

descdist(y, discrete = TRUE, boot = 100)

fit.norm_x <- fitdist(x,"norm")
fit.norm_y <- fitdist(y,"norm")
fit.norm_x$estimate[[1]]
fit.norm_y$estimate[[2]]
plot(fit.norm_x)

generate_plot(fit.norm_x$estimate[[1]],fit.norm_y$estimate[[1]],fit.norm_x$estimate[[2]], fit.norm_y$estimate[[2]])
  


