##Naive Based Lolll
source("loadRData.R")
source("count_shingle.R")
library("e1071")
library("klaR")
library("lattice")
library("Hmisc")
library("binr")
library("stats")
library("caret")
#library("doMC"); registerDoMC(cores = 4);




G2M1 = loadRData("../data/labeled_datasets/data-labeled-2-1-dpi100-sigma1-6.RData")
G2M2 = loadRData("../data/labeled_datasets/data-labeled-2-2-dpi100-sigma1-6.RData")


g_all = loadRData("../data/labeled_datasets/data-labeled-1-1-1-2-2-1-2-2-3-1-3-2-4-1-4-2-4-3-5-1-5-2-5-3-6-1-7-1-8-1-10-1-10-2-11-1-11-2-11-3-13-1-14-1-14-2-dpi100-sigma1-6.RData")
g_all_p = loadRData("../data/labeled_datasets/data-labeled-1-1-1-2-2-1-2-2-3-1-3-2-4-1-4-2-4-3-5-1-5-2-5-3-6-1-7-1-8-1-10-2-11-1-11-2-11-3-13-1-dpi100-sigma1-6.RData")
g_fewer  = loadRData("../data/labeled_datasets/data-labeled-1-1-2-1-3-1-4-1-5-1-6-1-7-1-8-1-11-1-13-1-dpi100-sigma1.6.RData")

length(g_fewer$data)/1296000

##Binning procedure:
G2M2_data = ave(G2M2$data,cut(x= G2M2$data, breaks = seq(from = 0, to = 1, by= 0.0041)), FUN=median)
G2M1_data = ave(G2M1$data,cut(x= G2M2$data,breaks =seq(from = 0, to = 1, by = 0.0505)),FUN = median)
g_fewer_data = ave(g_fewer$data,cut(x= g_fewer$data, breaks = seq(from = 0, to = 1, by= 0.0041)), FUN=median)
G2M2_data_pca = princomp(x=G2M2$data)
g_fewer_data_pca = princomp(x=g_fewer$data)

plot(round(cov(G2M2$data),4))

max(diag(round(cov(G2M2$data),5)))
#g_all_data = ave(g_all$data,cut(x= g_all$data, breaks = seq(from = 0, to = 1, by= 0.1)), FUN=median)
#g_all_p_data = ave(g_all_p$data,cut(x= g_all_p$data, breaks = seq(from = 0, to = 1, by= 0.1)), FUN=median)
#G2M1_data = ave(G2M1$data,cut(x= G2M1$data, breaks = seq(from = 0, to = 1, by= 0.01)), FUN=median)

##Training
train_control <- trainControl(method="cv", number=10,  preProcOptions = list(thresh = 0.95),p = 0.9)
fit <- train(x = g_fewer_data, y = g_fewer$labels , method = "nb", trControl=train_control, preProcess = "pca")



plot(x = 0, y = 0, xlim = c(1,1000), ylim = c(1,4000), ylab = "False predicted", xlab = "bin selection", main= "Number of false prediction vs. number of bins", type = "n")
for( i in 1:1000)
{
  print("iteration")
  print(i)
  G2M2_data = ave(G2M2$data,cut(x= G2M2$data, breaks = seq(from = 0, to = 1, by= i/1000)), FUN=median)
  g_fewer_data = ave(g_fewer$data,cut(x= g_fewer$data, breaks = seq(from = 0, to = 1, by= i/1000)), FUN=median)
  #G2M1_data = ave(G2M1$data,cut(x= G2M2$data,breaks =seq(from = 0, to = 1, by = i/1000)),FUN = median)
  
  print("divided")
  model = naiveBayes(x = g_fewer_data,  y = g_fewer$labels)
  print("model made")
  
  pred = predict(object = model, newdata = G2M2_data )
  print("pred made")
  
  confus <- confusionMatrix(pred, G2M2$labels)$table
  path <- paste("confus_", i, ".RData", sep="")
  save(confus, file = path)
  
  vectorOfTables[[i]] =  table(pred==G2M2$labels)[[1]]
  print(table(pred==G2M2$labels))
  lines(x = i , y = table(pred==G2M2$labels)[[1]], type = "b", col = "red")
}


(unlist(vectorOfTables))[[2]]
vectorOfTables = table(pred==G2M2$labels)
model = naiveBayes(x = g_fewer_data,  y = g_fewer$labels)
pred = predict(object = model, newdata = G2M2_data )
print(table(pred==G2M2$labels))



which(unlist(vectorOfTables) == min(unlist(vectorOfTables)))

seq(from=0.0001, to=1, by=0.001)[5]

plot(x = seq(from=0.0001, to=0.999, by=0.001) , y = unlist(vectorOfTables), type = 'l', col=ifelse(unlist(vectorOfTables)==1499, "red", "black"), ylab = "False predicted", xlab = "bin selection", main= "Number of false prediction vs. number of bins")

save(vectorOfTables_bac, file = "../data/vectorOFTable.RData")

which(unlist(vectorOfTables)==min(unlist(vectorOfTables)))

#################
#Numerical split -  Preprocess#
#################
#digit 0

data_0 = matrix(g_fewer_data[g_fewer$labels=="X0"],ncol = ncol(g_fewer_data))
data_1 = matrix(G2M1$data[G2M1$labels=="X1"],ncol = ncol(G2M1$data))
data_2 = G2M1$data[G2M1$labels=="X2"]
data_3 = G2M1$data[G2M1$labels=="X3"]
data_4 = G2M1$data[G2M1$labels=="X4"]
data_5 = G2M1$data[G2M1$labels=="X5"]
data_6 = G2M1$data[G2M1$labels=="X6"]
data_7 = G2M1$data[G2M1$labels=="X7"]
data_8 = G2M1$data[G2M1$labels=="X8"]
data_9 = G2M1$data[G2M1$labels=="X9"]


