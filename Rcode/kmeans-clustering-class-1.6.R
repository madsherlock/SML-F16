library(cluster)
library(HSAUR)
library(fpc)
library(base)
library(doParallel); cl <- makeCluster(4); registerDoParallel(cl) 
source("multiKmeans.r")






class_1.6 = loadRData("../data/labeled_datasets/data-labeled-1-1-1-2-2-1-2-2-3-1-3-2-4-1-4-2-4-3-5-1-5-2-5-3-6-1-7-1-8-1-10-1-10-2-11-1-11-2-11-3-13-1-14-1-14-2-dpi100-sigma1.6.RData")

class_1.6_num_0 = class_1.6[4]$data[1+0:9200*1,]
class_1.6_num_1 = class_1.6[4]$data[9201:18400,]
class_1.6_num_2 = class_1.6[4]$data[18401:27600,]
class_1.6_num_3 = class_1.6[4]$data[27601:36800,]
class_1.6_num_4 = class_1.6[4]$data[36801:46000,]
class_1.6_num_5 = class_1.6[4]$data[46001:55200,]
class_1.6_num_6 = class_1.6[4]$data[55201:64400,]
class_1.6_num_7 = class_1.6[4]$data[64401:73600,]
class_1.6_num_8 = class_1.6[4]$data[73601:82800,]
class_1.6_num_9 = class_1.6[4]$data[82801:92000,]


sample_size_class_0 = floor(0.90 * nrow(class_1.6_num_0))
sample_size_class_1 = floor(0.90 * nrow(class_1.6_num_1))
sample_size_class_2 = floor(0.90 * nrow(class_1.6_num_2))
sample_size_class_3 = floor(0.90 * nrow(class_1.6_num_3))
sample_size_class_4 = floor(0.90 * nrow(class_1.6_num_4))
sample_size_class_5 = floor(0.90 * nrow(class_1.6_num_5))
sample_size_class_6 = floor(0.90 * nrow(class_1.6_num_6))
sample_size_class_7 = floor(0.90 * nrow(class_1.6_num_7))
sample_size_class_8 = floor(0.90 * nrow(class_1.6_num_8))
sample_size_class_9 = floor(0.90 * nrow(class_1.6_num_9))


train_ind_class_0 <- sample(seq_len(nrow(class_1.6_num_0)), size = sample_size_class_0)
train_ind_class_1 <- sample(seq_len(nrow(class_1.6_num_1)), size = sample_size_class_1)
train_ind_class_2 <- sample(seq_len(nrow(class_1.6_num_2)), size = sample_size_class_2)
train_ind_class_3 <- sample(seq_len(nrow(class_1.6_num_3)), size = sample_size_class_3)
train_ind_class_4 <- sample(seq_len(nrow(class_1.6_num_4)), size = sample_size_class_4)
train_ind_class_5 <- sample(seq_len(nrow(class_1.6_num_5)), size = sample_size_class_5)
train_ind_class_6 <- sample(seq_len(nrow(class_1.6_num_6)), size = sample_size_class_6)
train_ind_class_7 <- sample(seq_len(nrow(class_1.6_num_7)), size = sample_size_class_7)
train_ind_class_8 <- sample(seq_len(nrow(class_1.6_num_8)), size = sample_size_class_8)
train_ind_class_9 <- sample(seq_len(nrow(class_1.6_num_9)), size = sample_size_class_9)



train_class_1.6_num_0 =  class_1.6_num_0[train_ind_class_0,] #90% train data
test_class_1.6_num_0 = class_1.6_num_0[-train_ind_class_0,]  #10% test data

train_class_1.6_num_1 =  class_1.6_num_1[train_ind_class_1,] #90% train data
test_class_1.6_num_1 = class_1.6_num_1[-train_ind_class_1,]  #10% test data

train_class_1.6_num_2 =  class_1.6_num_2[train_ind_class_2,] #90% train data
test_class_1.6_num_2 = class_1.6_num_2[-train_ind_class_2,]  #10% test data

train_class_1.6_num_3 =  class_1.6_num_3[train_ind_class_3,] #90% train data
test_class_1.6_num_3 = class_1.6_num_3[-train_ind_class_3,]  #10% test data

train_class_1.6_num_4 =  class_1.6_num_4[train_ind_class_4,] #90% train data
test_class_1.6_num_4 = class_1.6_num_4[-train_ind_class_4,]  #10% test data

train_class_1.6_num_5 =  class_1.6_num_5[train_ind_class_5,] #90% train data
test_class_1.6_num_5 = class_1.6_num_5[-train_ind_class_5,]  #10% test data

train_class_1.6_num_6 =  class_1.6_num_6[train_ind_class_6,] #90% train data
test_class_1.6_num_6 = class_1.6_num_6[-train_ind_class_6,]  #10% test data

train_class_1.6_num_7 =  class_1.6_num_7[train_ind_class_7,] #90% train data
test_class_1.6_num_7 = class_1.6_num_7[-train_ind_class_7,]  #10% test data

train_class_1.6_num_8 =  class_1.6_num_8[train_ind_class_8,] #90% train data
test_class_1.6_num_8 = class_1.6_num_8[-train_ind_class_8,]  #10% test data

train_class_1.6_num_9 =  class_1.6_num_9[train_ind_class_9,] #90% train data
test_class_1.6_num_9 = class_1.6_num_9[-train_ind_class_9,]  #10% test data

dim(class_1.6_num_0) == (dim(train_class_1.6_num_0) + dim(test_class_1.6_num_0))


#Performs Kmeans with given omount of centroids
# Run time 
start.time <- Sys.time()
###############################
#Clara for different distance methods 
###############################
centroids = 400
class_output_0= kmeans(train_class_1.6_num_0,center= centroids, iter.max = 30)
class_output_1= kmeans(train_class_1.6_num_1,center= centroids, iter.max = 30)
class_output_2= kmeans(train_class_1.6_num_2,center= centroids, iter.max = 30)
class_output_3= kmeans(train_class_1.6_num_3,center= centroids, iter.max = 30)
class_output_4= kmeans(train_class_1.6_num_4,center= centroids, iter.max = 30)
class_output_5= kmeans(train_class_1.6_num_5,center= centroids, iter.max = 30)
class_output_6= kmeans(train_class_1.6_num_6,center= centroids, iter.max = 30)
class_output_7= kmeans(train_class_1.6_num_7,center= centroids, iter.max = 30)
class_output_8= kmeans(train_class_1.6_num_8,center= centroids, iter.max = 30)
class_output_9= kmeans(train_class_1.6_num_9,center= centroids, iter.max = 30)
###############################
#Others

end.time <- Sys.time()
##############################
time.taken <- end.time - start.time
time.taken # End Timer

clusplot(train_class_1.6_num_0, class_output_0$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
#Explains which centroid the data element is saved to. 
class_output_0$cluster



#Knn performance with Clusters 
####################################
start.time <- Sys.time()

output_class_0_k1 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 1)
end_0_0.time <- Sys.time()

output_class_0_k2 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 2)
end_1_0.time <- Sys.time()

output_class_0_k3 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 3)
end_3_0.time <- Sys.time()

output_class_0_k4 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 4)
end_4_0.time <- Sys.time()

output_class_0_k5 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 5)
end_5_0.time <- Sys.time()

output_class_0_k6 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 6)
end_6_0.time <- Sys.time()

output_class_0_k7 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 7)
end_7_0.time <- Sys.time()

output_class_0_k8 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 8)
end_8_0.time <- Sys.time()

output_class_0_k9 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 9)
end_9_0.time <- Sys.time()

output_class_0_k10 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 10)
end_10_0.time <- Sys.time()

output_class_0_k11 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 11)
end_11_0.time <- Sys.time()

output_class_0_k12 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 12)
end_12_0.time <- Sys.time()

output_class_0_k13 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 13)
end_13_0.time <- Sys.time()

output_class_0_k14 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 14)
end_14_0.time <- Sys.time()

output_class_0_k15 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 15)
end_15_0.time <- Sys.time()

output_class_0_k16 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 16)
end_16_0.time <- Sys.time()

output_class_0_k17 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 17)
end_17_0.time <- Sys.time()

output_class_0_k18 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 18)
end_18_0.time <- Sys.time()

output_class_0_k19 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 19)
end_19_0.time <- Sys.time()

output_class_0_k20 = prediction.strength(train_class_1.6_num_0, Gmax = 400, M = 5, classification = "knn", count = TRUE, nnk = 20)
end_20_0.time <- Sys.time()
time.taken <- end_0.time - start.time
time.taken # End Timer


output_class_0 = matrix(nrow = 20, ncol = 5 )
output_class_0[1,] = output_class_0_k1$mean.pred
output_class_0[2,] = output_class_0_k2$mean.pred
output_class_0[3,] = output_class_0_k3$mean.pred
output_class_0[4,] = output_class_0_k4$mean.pred
output_class_0[5,] = output_class_0_k5$mean.pred
output_class_0[6,] = output_class_0_k6$mean.pred
output_class_0[7,] = output_class_0_k7$mean.pred
output_class_0[8,] = output_class_0_k8$mean.pred
output_class_0[9,] = output_class_0_k9$mean.pred
output_class_0[10,] = output_class_0_k10$mean.pred
output_class_0[11,] = output_class_0_k11$mean.pred
output_class_0[12,] = output_class_0_k12$mean.pred
output_class_0[13,] = output_class_0_k13$mean.pred
output_class_0[14,] = output_class_0_k14$mean.pred
output_class_0[15,] = output_class_0_k15$mean.pred
output_class_0[16,] = output_class_0_k16$mean.pred
output_class_0[17,] = output_class_0_k17$mean.pred
output_class_0[18,] = output_class_0_k18$mean.pred
output_class_0[19,] = output_class_0_k19$mean.pred
output_class_0[20,] = output_class_0_k20$mean.pred


# creates a own color palette from red to green
my_palette <- colorRampPalette(rainbow(10),space = "Lab")(n = 999)

#HEatMap
heatmap.2(
  output_class_0,
  Colv = "NA",
  Rowv = "NA", 
  #main = "k vs clusters vs mean prediction", # heat map title,
  xlab = "Clusters",
  ylab = "K",
  trace = "none",
  margins = c(3,4),
  cellnote = round(output_person_dependent_0, 3),
  notecol = "black",
  density.info="density", 
  denscol="black",
  col=my_palette)       # use on color palette defined earlier

legend("top", legend = "K vs. Clusters vs. Mean prediction for digit 0",bty = "n", cex=1)  

 