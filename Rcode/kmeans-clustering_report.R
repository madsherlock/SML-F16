##kmeans-clustering-class-1.6 Report
library(cluster)
library(HSAUR)
library(fpc)
library(base)
library("doMC"); registerDoMC(cores = 4);
source("multiKmeans.R")
source("elbowGraph.R")
source("loadRData.R")

##Load Data
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

#Seperate into training and test
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

#######################################################################################################
#######################################################################################################
train_class_1.6_num_0 =  class_1.6_num_0[train_ind_class_0,] #90% train data
#######################################################################################################
sample_size_class_0_2 = floor(0.50 * nrow(train_class_1.6_num_0))
train_ind_class_0_2 = sample(seq_len(nrow(train_class_1.6_num_0)), size = sample_size_class_0_2 )
train_class_1.6_num_0_2 =  class_1.6_num_0[train_ind_class_0_2,] #90%/2 train data

sample_size_class_0_4 = floor(0.50 * nrow(train_class_1.6_num_0_2))
train_ind_class_0_4 = sample(seq_len(nrow(train_class_1.6_num_0_2)), size = sample_size_class_0_4 )
train_class_1.6_num_0_4 =  class_1.6_num_0[train_ind_class_0_4,] #90%/4 train data

sample_size_class_0_8 = floor(0.50 * nrow(train_class_1.6_num_0_4))
train_ind_class_0_8 = sample(seq_len(nrow(train_class_1.6_num_0_4)), size = sample_size_class_0_8 )
train_class_1.6_num_0_8 =  class_1.6_num_0[train_ind_class_0_8,] #90%/8 train data

sample_size_class_0_16 = floor(0.50 * nrow(train_class_1.6_num_0_8))
train_ind_class_0_16 = sample(seq_len(nrow(train_class_1.6_num_0_8)), size = sample_size_class_0_16 )
train_class_1.6_num_0_16 =  class_1.6_num_0[train_ind_class_0_16,] #90%/16 train data
#######################################################################################################
test_class_1.6_num_0 = class_1.6_num_0[-train_ind_class_0,]  #10% test data
#######################################################################################################
#######################################################################################################
train_class_1.6_num_1 =  class_1.6_num_1[train_ind_class_1,] #90% train data
#######################################################################################################
sample_size_class_1_2 = floor(0.50 * nrow(train_class_1.6_num_1))
train_ind_class_1_2 = sample(seq_len(nrow(train_class_1.6_num_1)), size = sample_size_class_1_2 )
train_class_1.6_num_1_2 =  class_1.6_num_1[train_ind_class_1_2,] #90%/2 train data

sample_size_class_1_4 = floor(0.50 * nrow(train_class_1.6_num_1_2))
train_ind_class_1_4 = sample(seq_len(nrow(train_class_1.6_num_1_2)), size = sample_size_class_1_4 )
train_class_1.6_num_1_4 =  class_1.6_num_1[train_ind_class_1_4,] #90%/4 train data

sample_size_class_1_8 = floor(0.50 * nrow(train_class_1.6_num_1_4))
train_ind_class_1_8 = sample(seq_len(nrow(train_class_1.6_num_1_4)), size = sample_size_class_1_8 )
train_class_1.6_num_1_8 =  class_1.6_num_1[train_ind_class_1_8,] #90%/8 train data

sample_size_class_1_16 = floor(0.50 * nrow(train_class_1.6_num_1_8))
train_ind_class_1_16 = sample(seq_len(nrow(train_class_1.6_num_1_8)), size = sample_size_class_1_16 )
train_class_1.6_num_1_16 =  class_1.6_num_1[train_ind_class_1_16,] #90%/16 train data
#######################################################################################################
test_class_1.6_num_1 = class_1.6_num_1[-train_ind_class_1,]  #10% test data
#######################################################################################################
#######################################################################################################
train_class_1.6_num_2 =  class_1.6_num_2[train_ind_class_2,] #90% train data
#######################################################################################################
sample_size_class_2_2 = floor(0.50 * nrow(train_class_1.6_num_2))
train_ind_class_2_2 = sample(seq_len(nrow(train_class_1.6_num_2)), size = sample_size_class_2_2 )
train_class_1.6_num_2_2 =  class_1.6_num_2[train_ind_class_2_2,] #90%/2 train data

sample_size_class_2_4 = floor(0.50 * nrow(train_class_1.6_num_2_2))
train_ind_class_2_4 = sample(seq_len(nrow(train_class_1.6_num_2_2)), size = sample_size_class_2_4 )
train_class_1.6_num_2_4 =  class_1.6_num_2[train_ind_class_2_4,] #90%/4 train data

sample_size_class_2_8 = floor(0.50 * nrow(train_class_1.6_num_2_4))
train_ind_class_2_8 = sample(seq_len(nrow(train_class_1.6_num_2_4)), size = sample_size_class_2_8 )
train_class_1.6_num_2_8 =  class_1.6_num_2[train_ind_class_2_8,] #90%/8 train data

sample_size_class_2_16 = floor(0.50 * nrow(train_class_1.6_num_2_8))
train_ind_class_2_16 = sample(seq_len(nrow(train_class_1.6_num_2_8)), size = sample_size_class_2_16 )
train_class_1.6_num_2_16 =  class_1.6_num_2[train_ind_class_2_16,] #90%/16 train data
#######################################################################################################
test_class_1.6_num_2 = class_1.6_num_2[-train_ind_class_2,]  #10% test data
#######################################################################################################
#######################################################################################################
train_class_1.6_num_3 =  class_1.6_num_3[train_ind_class_3,] #90% train data
#######################################################################################################
sample_size_class_3_2 = floor(0.50 * nrow(train_class_1.6_num_3))
train_ind_class_3_2 = sample(seq_len(nrow(train_class_1.6_num_3)), size = sample_size_class_3_2 )
train_class_1.6_num_3_2 =  class_1.6_num_3[train_ind_class_3_2,] #90%/2 train data

sample_size_class_3_4 = floor(0.50 * nrow(train_class_1.6_num_3_2))
train_ind_class_3_4 = sample(seq_len(nrow(train_class_1.6_num_3_2)), size = sample_size_class_3_4 )
train_class_1.6_num_3_4 =  class_1.6_num_3[train_ind_class_3_4,] #90%/4 train data

sample_size_class_3_8 = floor(0.50 * nrow(train_class_1.6_num_3_4))
train_ind_class_3_8 = sample(seq_len(nrow(train_class_1.6_num_3_4)), size = sample_size_class_3_8 )
train_class_1.6_num_3_8 =  class_1.6_num_3[train_ind_class_3_8,] #90%/8 train data

sample_size_class_3_16 = floor(0.50 * nrow(train_class_1.6_num_3_8))
train_ind_class_3_16 = sample(seq_len(nrow(train_class_1.6_num_3_8)), size = sample_size_class_3_16 )
train_class_1.6_num_3_16 =  class_1.6_num_3[train_ind_class_3_16,] #90%/16 train data
#######################################################################################################
test_class_1.6_num_3 = class_1.6_num_3[-train_ind_class_3,]  #10% test data
#######################################################################################################
#######################################################################################################
train_class_1.6_num_4 =  class_1.6_num_4[train_ind_class_4,] #90% train data
#######################################################################################################
sample_size_class_4_2 = floor(0.50 * nrow(train_class_1.6_num_4))
train_ind_class_4_2 = sample(seq_len(nrow(train_class_1.6_num_4)), size = sample_size_class_4_2 )
train_class_1.6_num_4_2 =  class_1.6_num_4[train_ind_class_4_2,] #90%/2 train data

sample_size_class_4_4 = floor(0.50 * nrow(train_class_1.6_num_4_2))
train_ind_class_4_4 = sample(seq_len(nrow(train_class_1.6_num_4_2)), size = sample_size_class_4_4 )
train_class_1.6_num_4_4 =  class_1.6_num_4[train_ind_class_4_4,] #90%/4 train data

sample_size_class_4_8 = floor(0.50 * nrow(train_class_1.6_num_4_4))
train_ind_class_4_8 = sample(seq_len(nrow(train_class_1.6_num_4_4)), size = sample_size_class_4_8 )
train_class_1.6_num_4_8 =  class_1.6_num_4[train_ind_class_4_8,] #90%/8 train data

sample_size_class_4_16 = floor(0.50 * nrow(train_class_1.6_num_4_8))
train_ind_class_4_16 = sample(seq_len(nrow(train_class_1.6_num_4_8)), size = sample_size_class_4_16 )
train_class_1.6_num_4_16 =  class_1.6_num_4[train_ind_class_4_16,] #90%/16 train data
#######################################################################################################
test_class_1.6_num_4 = class_1.6_num_4[-train_ind_class_4,]  #10% test data
#######################################################################################################
#######################################################################################################
train_class_1.6_num_5 =  class_1.6_num_5[train_ind_class_5,] #90% train data
#######################################################################################################
sample_size_class_5_2 = floor(0.50 * nrow(train_class_1.6_num_5))
train_ind_class_5_2 = sample(seq_len(nrow(train_class_1.6_num_5)), size = sample_size_class_5_2 )
train_class_1.6_num_5_2 =  class_1.6_num_5[train_ind_class_5_2,] #90%/2 train data

sample_size_class_5_4 = floor(0.50 * nrow(train_class_1.6_num_5_2))
train_ind_class_5_4 = sample(seq_len(nrow(train_class_1.6_num_5_2)), size = sample_size_class_5_4 )
train_class_1.6_num_5_4 =  class_1.6_num_5[train_ind_class_5_4,] #90%/4 train data

sample_size_class_5_8 = floor(0.50 * nrow(train_class_1.6_num_5_4))
train_ind_class_5_8 = sample(seq_len(nrow(train_class_1.6_num_5_4)), size = sample_size_class_5_8 )
train_class_1.6_num_5_8 =  class_1.6_num_5[train_ind_class_5_8,] #90%/8 train data

sample_size_class_5_16 = floor(0.50 * nrow(train_class_1.6_num_5_8))
train_ind_class_5_16 = sample(seq_len(nrow(train_class_1.6_num_5_8)), size = sample_size_class_5_16 )
train_class_1.6_num_5_16 =  class_1.6_num_5[train_ind_class_5_16,] #90%/16 train data
#######################################################################################################
test_class_1.6_num_5 = class_1.6_num_5[-train_ind_class_5,]  #10% test data
#######################################################################################################
#######################################################################################################
train_class_1.6_num_6 =  class_1.6_num_6[train_ind_class_6,] #90% train data
#######################################################################################################
sample_size_class_6_2 = floor(0.50 * nrow(train_class_1.6_num_6))
train_ind_class_6_2 = sample(seq_len(nrow(train_class_1.6_num_6)), size = sample_size_class_6_2 )
train_class_1.6_num_6_2 =  class_1.6_num_6[train_ind_class_6_2,] #90%/2 train data

sample_size_class_6_4 = floor(0.50 * nrow(train_class_1.6_num_6_2))
train_ind_class_6_4 = sample(seq_len(nrow(train_class_1.6_num_6_2)), size = sample_size_class_6_4 )
train_class_1.6_num_6_4 =  class_1.6_num_6[train_ind_class_6_4,] #90%/4 train data

sample_size_class_6_8 = floor(0.50 * nrow(train_class_1.6_num_6_4))
train_ind_class_6_8 = sample(seq_len(nrow(train_class_1.6_num_6_4)), size = sample_size_class_6_8 )
train_class_1.6_num_6_8 =  class_1.6_num_6[train_ind_class_6_8,] #90%/8 train data

sample_size_class_6_16 = floor(0.50 * nrow(train_class_1.6_num_6_8))
train_ind_class_6_16 = sample(seq_len(nrow(train_class_1.6_num_6_8)), size = sample_size_class_6_16 )
train_class_1.6_num_6_16 =  class_1.6_num_6[train_ind_class_6_16,] #90%/16 train data
#######################################################################################################
test_class_1.6_num_6 = class_1.6_num_6[-train_ind_class_6,]  #10% test data
#######################################################################################################
#######################################################################################################
train_class_1.6_num_7 =  class_1.6_num_7[train_ind_class_7,] #90% train data
#######################################################################################################
sample_size_class_7_2 = floor(0.50 * nrow(train_class_1.6_num_7))
train_ind_class_7_2 = sample(seq_len(nrow(train_class_1.6_num_7)), size = sample_size_class_7_2 )
train_class_1.6_num_7_2 =  class_1.6_num_7[train_ind_class_7_2,] #90%/2 train data

sample_size_class_7_4 = floor(0.50 * nrow(train_class_1.6_num_7_2))
train_ind_class_7_4 = sample(seq_len(nrow(train_class_1.6_num_7_2)), size = sample_size_class_7_4 )
train_class_1.6_num_7_4 =  class_1.6_num_7[train_ind_class_7_4,] #90%/4 train data

sample_size_class_7_8 = floor(0.50 * nrow(train_class_1.6_num_7_4))
train_ind_class_7_8 = sample(seq_len(nrow(train_class_1.6_num_7_4)), size = sample_size_class_7_8 )
train_class_1.6_num_7_8 =  class_1.6_num_7[train_ind_class_7_8,] #90%/8 train data

sample_size_class_7_16 = floor(0.50 * nrow(train_class_1.6_num_7_8))
train_ind_class_7_16 = sample(seq_len(nrow(train_class_1.6_num_7_8)), size = sample_size_class_7_16 )
train_class_1.6_num_7_16 =  class_1.6_num_7[train_ind_class_7_16,] #90%/16 train data
#######################################################################################################
test_class_1.6_num_7 = class_1.6_num_7[-train_ind_class_7,]  #10% test data
#######################################################################################################
#######################################################################################################
train_class_1.6_num_8 =  class_1.6_num_8[train_ind_class_8,] #90% train data
#######################################################################################################
sample_size_class_8_2 = floor(0.50 * nrow(train_class_1.6_num_8))
train_ind_class_8_2 = sample(seq_len(nrow(train_class_1.6_num_8)), size = sample_size_class_8_2 )
train_class_1.6_num_8_2 =  class_1.6_num_8[train_ind_class_8_2,] #90%/2 train data

sample_size_class_8_4 = floor(0.50 * nrow(train_class_1.6_num_8_2))
train_ind_class_8_4 = sample(seq_len(nrow(train_class_1.6_num_8_2)), size = sample_size_class_8_4 )
train_class_1.6_num_8_4 =  class_1.6_num_8[train_ind_class_8_4,] #90%/4 train data

sample_size_class_8_8 = floor(0.50 * nrow(train_class_1.6_num_8_4))
train_ind_class_8_8 = sample(seq_len(nrow(train_class_1.6_num_8_4)), size = sample_size_class_8_8 )
train_class_1.6_num_8_8 =  class_1.6_num_8[train_ind_class_8_8,] #90%/8 train data

sample_size_class_8_16 = floor(0.50 * nrow(train_class_1.6_num_8_8))
train_ind_class_8_16 = sample(seq_len(nrow(train_class_1.6_num_8_8)), size = sample_size_class_8_16 )
train_class_1.6_num_8_16 =  class_1.6_num_8[train_ind_class_8_16,] #90%/16 train data
#######################################################################################################
test_class_1.6_num_8 = class_1.6_num_8[-train_ind_class_8,]  #10% test data
#######################################################################################################
#######################################################################################################
train_class_1.6_num_9 =  class_1.6_num_9[train_ind_class_9,] #90% train data
#######################################################################################################
sample_size_class_9_2 = floor(0.50 * nrow(train_class_1.6_num_9))
train_ind_class_9_2 = sample(seq_len(nrow(train_class_1.6_num_9)), size = sample_size_class_9_2 )
train_class_1.6_num_9_2 =  class_1.6_num_9[train_ind_class_9_2,] #90%/2 train data

sample_size_class_9_4 = floor(0.50 * nrow(train_class_1.6_num_9_2))
train_ind_class_9_4 = sample(seq_len(nrow(train_class_1.6_num_9_2)), size = sample_size_class_9_4 )
train_class_1.6_num_9_4 =  class_1.6_num_9[train_ind_class_9_4,] #90%/4 train data

sample_size_class_9_8 = floor(0.50 * nrow(train_class_1.6_num_9_4))
train_ind_class_9_8 = sample(seq_len(nrow(train_class_1.6_num_9_4)), size = sample_size_class_9_8 )
train_class_1.6_num_9_8 =  class_1.6_num_9[train_ind_class_9_8,] #90%/8 train data

sample_size_class_9_16 = floor(0.50 * nrow(train_class_1.6_num_9_8))
train_ind_class_9_16 = sample(seq_len(nrow(train_class_1.6_num_9_8)), size = sample_size_class_9_16 )
train_class_1.6_num_9_16 =  class_1.6_num_9[train_ind_class_9_16,] #90%/16 train data
#######################################################################################################
test_class_1.6_num_9 = class_1.6_num_9[-train_ind_class_9,]  #10% test data
#######################################################################################################
#MultiKmeans 
centroids = 400
class_output_0= multiKmeans(train_class_1.6_num_0,max.clusters = centroids, iter = 30)
class_output_0_2= multiKmeans(train_class_1.6_num_0_2,max.clusters = centroids, iter = 30)
class_output_0_4= multiKmeans(train_class_1.6_num_0_4,max.clusters = centroids, iter = 30)
class_output_0_8= multiKmeans(train_class_1.6_num_0_8,max.clusters = centroids, iter = 30)
class_output_0_16= multiKmeans(train_class_1.6_num_0_16,max.clusters = centroids, iter = 30)
##
class_output_1= multiKmeans(train_class_1.6_num_1,max.clusters = centroids, iter = 30)
class_output_2= multiKmeans(train_class_1.6_num_2,max.clusters = centroids, iter = 30)
class_output_3= multiKmeans(train_class_1.6_num_3,max.clusters = centroids, iter = 30)
class_output_4= multiKmeans(train_class_1.6_num_4,max.clusters = centroids, iter = 30)
class_output_5= multiKmeans(train_class_1.6_num_5,max.clusters = centroids, iter = 30)
class_output_6= multiKmeans(train_class_1.6_num_6,max.clusters = centroids, iter = 30)
class_output_7= multiKmeans(train_class_1.6_num_7,max.clusters = centroids, iter = 30)
class_output_8= multiKmeans(train_class_1.6_num_8,max.clusters = centroids, iter = 30)
class_output_9= multiKmeans(train_class_1.6_num_9,max.clusters = centroids, iter = 30)
#######################################################################################################
#Plotting and storring Rgraphs
class_output_9


dev.off()
elbowGraph(class_output_0_8$css) #0-100 - 0- 0.6cm - 2,6cm (3cm = 100) 30 - 74.2857
dev.print(png, '../Report3/elbowPlot_0_8.png', width=8,height=6,units="in",res=300)
dev.off()
elbowGraph(class_output_0_16$css) #0-100 - 0- 0.6cm - 2,6cm (3cm = 100) 30 - 74.2857
dev.print(png, '../Report3/elbowPlot_0_16.png', width=8,height=6,units="in",res=300)
dev.off()
#################################################################################
dev.off()
class_output_0
class_output_0_2
class_output_0_4
class_output_0_8
class_output_0_16
elbowGraphMult(class_output_0$css,class_output_0_2$css,class_output_0_4$css,class_output_0_8$css,class_output_0_16$css)
dev.print(png, '../Report3/elbowPlot_0_f.png', width=8,height=6,units="in",res=300)
dev.off()
#################################################################################
#Prediction and validation
start.time <- Sys.time()
for(digit in 1:10)
{
  data = list(train_class_1.6_num_0_4,train_class_1.6_num_1_4,train_class_1.6_num_2_4,train_class_1.6_num_3_4,train_class_1.6_num_4_4,train_class_1.6_num_5_4,train_class_1.6_num_6_4,train_class_1.6_num_7_4,train_class_1.6_num_8_4,train_class_1.6_num_9_4)
  for(i in 1:20)
  { 
    
    print("k: ")
    print(i)
    print(digit)
    nam <- paste("output_class_",digit-1,"_k_",i,sep = "")
    start.time <- paste("start_",digit-1,"_",i,sep= "")
    time <- paste("end_",digit-1,"_",i,sep= "")
    assign(start.time,Sys.time())
    assign(nam,prediction.strength(data[[digit]], Gmax = 20, M = 2 ,classification = "knn", count = TRUE, nnk = i))
    assign(time, Sys.time())
  }

}

###################################################################################
#Heatmap
#dataPrep
end_0_1
start_0_2

runtime_0= c((end_0_1 - start_0_1),
(end_0_2 - start_0_1) ,
(end_0_3 - start_0_1),
(end_0_4 - start_0_1),
end_0_5 - start_0_1,
end_0_6 - start_0_1,
end_0_7 - start_0_1,
end_0_8 - start_0_1,
end_0_9 - start_0_1,
end_0_10 - start_0_1,
end_0_11 - start_0_1,
end_0_12 - start_0_1,
end_0_13 - start_0_1,
end_0_14 - start_0_1,
end_0_15 - start_0_1,
end_0_16 - start_0_1,
end_0_17 - start_0_1,
end_0_18 - start_0_1,
end_0_19 - start_0_1,
end_0_20 - start_0_1)

runtime_1= c((end_1_1 - start_1_1),
             (end_1_2 - start_1_1) ,
             (end_1_3 - start_1_1),
             (end_1_4 - start_1_1),
             end_1_5 - start_1_1,
             end_1_6 - start_1_1,
             end_1_7 - start_1_1,
             end_1_8 - start_1_1,
             end_1_9 - start_1_1,
             end_1_10 - start_1_1,
             end_1_11 - start_1_1,
             end_1_12 - start_1_1,
             end_1_13 - start_1_1,
             end_1_14 - start_1_1,
             end_1_15 - start_1_1,
             end_1_16 - start_1_1,
             end_1_17 - start_1_1,
             end_1_18 - start_1_1,
             end_1_19 - start_1_1,
             end_1_20 - start_1_1)


runtime_2= c((end_2_1 - start_2_1),
             (end_2_2 - start_2_1) ,
             (end_2_3 - start_2_1),
             (end_2_4 - start_2_1),
             end_2_5 - start_2_1,
             end_2_6 - start_2_1,
             end_2_7 - start_2_1,
             end_2_8 - start_2_1,
             end_2_9 - start_2_1,
             end_2_10 - start_2_1,
             end_2_11 - start_2_1,
             end_2_12 - start_2_1,
             end_2_13 - start_2_1,
             end_2_14 - start_2_1,
             end_2_15 - start_2_1,
             end_2_16 - start_2_1,
             end_2_17 - start_2_1,
             end_2_18 - start_2_1,
             end_2_19 - start_2_1,
             end_2_20 - start_2_1)

runtime_3= c((end_3_1 - start_3_1),
             (end_3_2 - start_3_1),
             (end_3_3 - start_3_1),
             (end_3_4 - start_3_1),
             end_3_5 - start_3_1,
             end_3_6 - start_3_1,
             end_3_7 - start_3_1,
             end_3_8 - start_3_1,
             end_3_9 - start_3_1,
             end_3_10 - start_3_1,
             end_3_11 - start_3_1,
             end_3_12 - start_3_1,
             end_3_13 - start_3_1,
             end_3_14 - start_3_1,
             end_3_15 - start_3_1,
             end_3_16 - start_3_1,
             end_3_17 - start_3_1,
             end_3_18 - start_3_1,
             end_3_19 - start_3_1,
             end_3_20 - start_3_1)

runtime_4= c((end_4_1 - start_4_1),
             (end_4_2 - start_4_1),
             (end_4_3 - start_4_1),
             (end_4_4 - start_4_1),
             end_4_5 - start_4_1,
             end_4_6 - start_4_1,
             end_4_7 - start_4_1,
             end_4_8 - start_4_1,
             end_4_9 - start_4_1,
             end_4_10 - start_4_1,
             end_4_11 - start_4_1,
             end_4_12 - start_4_1,
             end_4_13 - start_4_1,
             end_4_14 - start_4_1,
             end_4_15 - start_4_1,
             end_4_16 - start_4_1,
             end_4_17 - start_4_1,
             end_4_18 - start_4_1,
             end_4_19 - start_4_1,
             end_4_20 - start_4_1)


runtime_5= c((end_5_1 - start_5_1),
             (end_5_2 - start_5_1) ,
             (end_5_3 - start_5_1),
             (end_5_4 - start_5_1),
             end_5_5 - start_5_1,
             end_5_6 - start_5_1,
             end_5_7 - start_5_1,
             end_5_8 - start_5_1,
             end_5_9 - start_5_1,
             end_5_10 - start_5_1,
             end_5_11 - start_5_1,
             end_5_12 - start_5_1,
             end_5_13 - start_5_1,
             end_5_14 - start_5_1,
             end_5_15 - start_5_1,
             end_5_16 - start_5_1,
             end_5_17 - start_5_1,
             end_5_18 - start_5_1,
             end_5_19 - start_5_1,
             end_5_20 - start_5_1)


runtime_6= c((end_6_1 - start_6_1),
             (end_6_2 - start_6_1),
             (end_6_3 - start_6_1),
             (end_6_4 - start_6_1),
             end_6_5 - start_6_1,
             end_6_6 - start_6_1,
             end_6_7 - start_6_1,
             end_6_8 - start_6_1,
             end_6_9 - start_6_1,
             end_6_10 - start_6_1,
             end_6_11 - start_6_1,
             end_6_12 - start_6_1,
             end_6_13 - start_6_1,
             end_6_14 - start_6_1,
             end_6_15 - start_6_1,
             end_6_16 - start_6_1,
             end_6_17 - start_6_1,
             end_6_18 - start_6_1,
             end_6_19 - start_6_1,
             end_6_20 - start_6_1)


runtime_7= c((end_7_1 - start_7_1),
             (end_7_2 - start_7_1),
             (end_7_3 - start_7_1),
             (end_7_4 - start_7_1),
             end_7_5 - start_7_1,
             end_7_6 - start_7_1,
             end_7_7 - start_7_1,
             end_7_8 - start_7_1,
             end_7_9 - start_7_1,
             end_7_10 - start_7_1,
             end_7_11 - start_7_1,
             end_7_12 - start_7_1,
             end_7_13 - start_7_1,
             end_7_14 - start_7_1,
             end_7_15 - start_7_1,
             end_7_16 - start_7_1,
             end_7_17 - start_7_1,
             end_7_18 - start_7_1,
             end_7_19 - start_7_1,
             end_7_20 - start_7_1)


runtime_8= c((end_8_1 - start_8_1),
             (end_8_2 - start_8_1) ,
             (end_8_3 - start_8_1),
             (end_8_4 - start_8_1),
             end_8_5 - start_8_1,
             end_8_6 - start_8_1,
             end_8_7 - start_8_1,
             end_8_8 - start_8_1,
             end_8_9 - start_8_1,
             end_8_10 - start_8_1,
             end_8_11 - start_8_1,
             end_8_12 - start_8_1,
             end_8_13 - start_8_1,
             end_8_14 - start_8_1,
             end_8_15 - start_8_1,
             end_8_16 - start_8_1,
             end_8_17 - start_8_1,
             end_8_18 - start_8_1,
             end_8_19 - start_8_1,
             end_8_20 - start_8_1)


runtime_9= c((end_9_1 - start_9_1),
             (end_9_2 - start_9_1) ,
             (end_9_3 - start_9_1),
             (end_9_4 - start_9_1),
             end_9_5 - start_9_1,
             end_9_6 - start_9_1,
             end_9_7 - start_9_1,
             end_9_8 - start_9_1,
             end_9_9 - start_9_1,
             end_9_10 - start_9_1,
             end_9_11 - start_9_1,
             end_9_12 - start_9_1,
             end_9_13 - start_9_1,
             end_9_14 - start_9_1,
             end_9_15 - start_9_1,
             end_9_16 - start_9_1,
             end_9_17 - start_9_1,
             end_9_18 - start_9_1,
             end_9_19 - start_9_1,
             end_9_20 - start_9_1)




output_class_0 = matrix(ncol = 20, nrow = 20)
output_class_1 = matrix(ncol = 20, nrow = 20)
output_class_2 = matrix(ncol = 20, nrow = 20)
output_class_3 = matrix(ncol = 20, nrow = 20)
output_class_4 = matrix(ncol = 20, nrow = 20)
output_class_5 = matrix(ncol = 20, nrow = 20)
output_class_6 = matrix(ncol = 20, nrow = 20)
output_class_7 = matrix(ncol = 20, nrow = 20)
output_class_8 = matrix(ncol = 20, nrow = 20)
output_class_9 = matrix(ncol = 20, nrow = 20)

output_class_0_k_1$

output_class_0[1,] = output_class_0_k_1$mean.pred
output_class_1[1,] = output_class_1_k_1$mean.pred
output_class_2[1,] = output_class_2_k_1$mean.pred
output_class_3[1,] = output_class_3_k_1$mean.pred
output_class_4[1,] = output_class_4_k_1$mean.pred
output_class_5[1,] = output_class_5_k_1$mean.pred
output_class_6[1,] = output_class_6_k_1$mean.pred
output_class_7[1,] = output_class_7_k_1$mean.pred
output_class_8[1,] = output_class_8_k_1$mean.pred
output_class_9[1,] = output_class_9_k_1$mean.pred


output_class_0[2,] = output_class_0_k_2$mean.pred
output_class_1[2,] = output_class_1_k_2$mean.pred
output_class_2[2,] = output_class_2_k_2$mean.pred
output_class_3[2,] = output_class_3_k_2$mean.pred
output_class_4[2,] = output_class_4_k_2$mean.pred
output_class_5[2,] = output_class_5_k_2$mean.pred
output_class_6[2,] = output_class_6_k_2$mean.pred
output_class_7[2,] = output_class_7_k_2$mean.pred
output_class_8[2,] = output_class_8_k_2$mean.pred
output_class_9[2,] = output_class_9_k_2$mean.pred


output_class_0[3,] = output_class_0_k_3$mean.pred
output_class_1[3,] = output_class_1_k_3$mean.pred
output_class_2[3,] = output_class_2_k_3$mean.pred
output_class_3[3,] = output_class_3_k_3$mean.pred
output_class_4[3,] = output_class_4_k_3$mean.pred
output_class_5[3,] = output_class_5_k_3$mean.pred
output_class_6[3,] = output_class_6_k_3$mean.pred
output_class_7[3,] = output_class_7_k_3$mean.pred
output_class_8[3,] = output_class_8_k_3$mean.pred
output_class_9[3,] = output_class_9_k_3$mean.pred


output_class_0[4,] = output_class_0_k_4$mean.pred
output_class_1[4,] = output_class_1_k_4$mean.pred
output_class_2[4,] = output_class_2_k_4$mean.pred
output_class_3[4,] = output_class_3_k_4$mean.pred
output_class_4[4,] = output_class_4_k_4$mean.pred
output_class_5[4,] = output_class_5_k_4$mean.pred
output_class_6[4,] = output_class_6_k_4$mean.pred
output_class_7[4,] = output_class_7_k_4$mean.pred
output_class_8[4,] = output_class_8_k_4$mean.pred
output_class_9[4,] = output_class_9_k_4$mean.pred


output_class_0[5,] = output_class_0_k_5$mean.pred
output_class_1[5,] = output_class_1_k_5$mean.pred
output_class_2[5,] = output_class_2_k_5$mean.pred
output_class_3[5,] = output_class_3_k_5$mean.pred
output_class_4[5,] = output_class_4_k_5$mean.pred
output_class_5[5,] = output_class_5_k_5$mean.pred
output_class_6[5,] = output_class_6_k_5$mean.pred
output_class_7[5,] = output_class_7_k_5$mean.pred
output_class_8[5,] = output_class_8_k_5$mean.pred
output_class_9[5,] = output_class_9_k_5$mean.pred

output_class_0[6,] = output_class_0_k_6$mean.pred
output_class_1[6,] = output_class_1_k_6$mean.pred
output_class_2[6,] = output_class_2_k_6$mean.pred
output_class_3[6,] = output_class_3_k_6$mean.pred
output_class_4[6,] = output_class_4_k_6$mean.pred
output_class_5[6,] = output_class_5_k_6$mean.pred
output_class_6[6,] = output_class_6_k_6$mean.pred
output_class_7[6,] = output_class_7_k_6$mean.pred
output_class_8[6,] = output_class_8_k_6$mean.pred
output_class_9[6,] = output_class_9_k_6$mean.pred


output_class_0[7,] = output_class_0_k_7$mean.pred
output_class_1[7,] = output_class_1_k_7$mean.pred
output_class_2[7,] = output_class_2_k_7$mean.pred
output_class_3[7,] = output_class_3_k_7$mean.pred
output_class_4[7,] = output_class_4_k_7$mean.pred
output_class_5[7,] = output_class_5_k_7$mean.pred
output_class_6[7,] = output_class_6_k_7$mean.pred
output_class_7[7,] = output_class_7_k_7$mean.pred
output_class_8[7,] = output_class_8_k_7$mean.pred
output_class_9[7,] = output_class_9_k_7$mean.pred

output_class_0[8,] = output_class_0_k_8$mean.pred
output_class_1[8,] = output_class_1_k_8$mean.pred
output_class_2[8,] = output_class_2_k_8$mean.pred
output_class_3[8,] = output_class_3_k_8$mean.pred
output_class_4[8,] = output_class_4_k_8$mean.pred
output_class_5[8,] = output_class_5_k_8$mean.pred
output_class_6[8,] = output_class_6_k_8$mean.pred
output_class_7[8,] = output_class_7_k_8$mean.pred
output_class_8[8,] = output_class_8_k_8$mean.pred
output_class_9[8,] = output_class_9_k_8$mean.pred

output_class_0[9,] = output_class_0_k_9$mean.pred
output_class_1[9,] = output_class_1_k_9$mean.pred
output_class_2[9,] = output_class_2_k_9$mean.pred
output_class_3[9,] = output_class_3_k_9$mean.pred
output_class_4[9,] = output_class_4_k_9$mean.pred
output_class_5[9,] = output_class_5_k_9$mean.pred
output_class_6[9,] = output_class_6_k_9$mean.pred
output_class_7[9,] = output_class_7_k_9$mean.pred
output_class_8[9,] = output_class_8_k_9$mean.pred
output_class_9[9,] = output_class_9_k_9$mean.pred


output_class_0[10,] = output_class_0_k_10$mean.pred
output_class_1[10,] = output_class_1_k_10$mean.pred
output_class_2[10,] = output_class_2_k_10$mean.pred
output_class_3[10,] = output_class_3_k_10$mean.pred
output_class_4[10,] = output_class_4_k_10$mean.pred
output_class_5[10,] = output_class_5_k_10$mean.pred
output_class_6[10,] = output_class_6_k_10$mean.pred
output_class_7[10,] = output_class_7_k_10$mean.pred
output_class_8[10,] = output_class_8_k_10$mean.pred
output_class_9[10,] = output_class_9_k_10$mean.pred


output_class_0[11,] = output_class_0_k_11$mean.pred
output_class_1[11,] = output_class_1_k_11$mean.pred
output_class_2[11,] = output_class_2_k_11$mean.pred
output_class_3[11,] = output_class_3_k_11$mean.pred
output_class_4[11,] = output_class_4_k_11$mean.pred
output_class_5[11,] = output_class_5_k_11$mean.pred
output_class_6[11,] = output_class_6_k_11$mean.pred
output_class_7[11,] = output_class_7_k_11$mean.pred
output_class_8[11,] = output_class_8_k_11$mean.pred
output_class_9[11,] = output_class_9_k_11$mean.pred

output_class_0[12,] = output_class_0_k_12$mean.pred
output_class_1[12,] = output_class_1_k_12$mean.pred
output_class_2[12,] = output_class_2_k_12$mean.pred
output_class_3[12,] = output_class_3_k_12$mean.pred
output_class_4[12,] = output_class_4_k_12$mean.pred
output_class_5[12,] = output_class_5_k_12$mean.pred
output_class_6[12,] = output_class_6_k_12$mean.pred
output_class_7[12,] = output_class_7_k_12$mean.pred
output_class_8[12,] = output_class_8_k_12$mean.pred
output_class_9[12,] = output_class_9_k_12$mean.pred

output_class_0[13,] = output_class_0_k_13$mean.pred
output_class_1[13,] = output_class_1_k_13$mean.pred
output_class_2[13,] = output_class_2_k_13$mean.pred
output_class_3[13,] = output_class_3_k_13$mean.pred
output_class_4[13,] = output_class_4_k_13$mean.pred
output_class_5[13,] = output_class_5_k_13$mean.pred
output_class_6[13,] = output_class_6_k_13$mean.pred
output_class_7[13,] = output_class_7_k_13$mean.pred
output_class_8[13,] = output_class_8_k_13$mean.pred
output_class_9[13,] = output_class_9_k_13$mean.pred

output_class_0[14,] = output_class_0_k_14$mean.pred
output_class_1[14,] = output_class_1_k_14$mean.pred
output_class_2[14,] = output_class_2_k_14$mean.pred
output_class_3[14,] = output_class_3_k_14$mean.pred
output_class_4[14,] = output_class_4_k_14$mean.pred
output_class_5[14,] = output_class_5_k_14$mean.pred
output_class_6[14,] = output_class_6_k_14$mean.pred
output_class_7[14,] = output_class_7_k_14$mean.pred
output_class_8[14,] = output_class_8_k_14$mean.pred
output_class_9[14,] = output_class_9_k_14$mean.pred


output_class_0[15,] = output_class_0_k_15$mean.pred
output_class_1[15,] = output_class_1_k_15$mean.pred
output_class_2[15,] = output_class_2_k_15$mean.pred
output_class_3[15,] = output_class_3_k_15$mean.pred
output_class_4[15,] = output_class_4_k_15$mean.pred
output_class_5[15,] = output_class_5_k_15$mean.pred
output_class_6[15,] = output_class_6_k_15$mean.pred
output_class_7[15,] = output_class_7_k_15$mean.pred
output_class_8[15,] = output_class_8_k_15$mean.pred
output_class_9[15,] = output_class_9_k_15$mean.pred

output_class_0[16,] = output_class_0_k_16$mean.pred
output_class_1[16,] = output_class_1_k_16$mean.pred
output_class_2[16,] = output_class_2_k_16$mean.pred
output_class_3[16,] = output_class_3_k_16$mean.pred
output_class_4[16,] = output_class_4_k_16$mean.pred
output_class_5[16,] = output_class_5_k_16$mean.pred
output_class_6[16,] = output_class_6_k_16$mean.pred
output_class_7[16,] = output_class_7_k_16$mean.pred
output_class_8[16,] = output_class_8_k_16$mean.pred
output_class_9[16,] = output_class_9_k_16$mean.pred

output_class_0[17,] = output_class_0_k_17$mean.pred
output_class_1[17,] = output_class_1_k_17$mean.pred
output_class_2[17,] = output_class_2_k_17$mean.pred
output_class_3[17,] = output_class_3_k_17$mean.pred
output_class_4[17,] = output_class_4_k_17$mean.pred
output_class_5[17,] = output_class_5_k_17$mean.pred
output_class_6[17,] = output_class_6_k_17$mean.pred
output_class_7[17,] = output_class_7_k_17$mean.pred
output_class_8[17,] = output_class_8_k_17$mean.pred
output_class_9[17,] = output_class_9_k_17$mean.pred

output_class_0[18,] = output_class_0_k_18$mean.pred
output_class_1[18,] = output_class_1_k_18$mean.pred
output_class_2[18,] = output_class_2_k_18$mean.pred
output_class_3[18,] = output_class_3_k_18$mean.pred
output_class_4[18,] = output_class_4_k_18$mean.pred
output_class_5[18,] = output_class_5_k_18$mean.pred
output_class_6[18,] = output_class_6_k_18$mean.pred
output_class_7[18,] = output_class_7_k_18$mean.pred
output_class_8[18,] = output_class_8_k_18$mean.pred
output_class_9[18,] = output_class_9_k_18$mean.pred

output_class_0[19,] = output_class_0_k_19$mean.pred
output_class_1[19,] = output_class_1_k_19$mean.pred
output_class_2[19,] = output_class_2_k_19$mean.pred
output_class_3[19,] = output_class_3_k_19$mean.pred
output_class_4[19,] = output_class_4_k_19$mean.pred
output_class_5[19,] = output_class_5_k_19$mean.pred
output_class_6[19,] = output_class_6_k_19$mean.pred
output_class_7[19,] = output_class_7_k_19$mean.pred
output_class_8[19,] = output_class_8_k_19$mean.pred
output_class_9[19,] = output_class_9_k_19$mean.pred

output_class_0[20,] = output_class_0_k_20$mean.pred
output_class_1[20,] = output_class_1_k_20$mean.pred
output_class_2[20,] = output_class_2_k_20$mean.pred
output_class_3[20,] = output_class_3_k_20$mean.pred
output_class_4[20,] = output_class_4_k_20$mean.pred
output_class_5[20,] = output_class_5_k_20$mean.pred
output_class_6[20,] = output_class_6_k_20$mean.pred
output_class_7[20,] = output_class_7_k_20$mean.pred
output_class_8[20,] = output_class_8_k_20$mean.pred
output_class_9[20,] = output_class_9_k_20$mean.pred


dev.off()
# creates a own color palette from red to green
my_palette <- colorRampPalette(rainbow(10),space = "Lab")(n = 999)

#HEatMap
heatmap.2(
  output_class_9,
  Colv = "NA",
  Rowv = "NA", 
  #main = "k vs clusters vs mean prediction", # heat map title,
  xlab = "Clusters",
  ylab = "K",
  trace = "none",
  margins = c(4,4),
  cellnote = round(output_class_9, 1), notecex=0.6, 
  notecol = "black",
  density.info="density", 
  denscol="black",
  col=my_palette)       # use on color palette defined earlier

legend("top", legend = "K vs. Clusters vs. Mean prediction for digit 9",bty = "n", cex=1)  
dev.print(png, '../Report3/knn_cluster_9_heatmap.png', width=8,height=6,units="in",res=300)

dev.off()

runtime_0[[1]]

plot(x = 1:20, y= runtime_9, xlab = "# of k", ylab= "Runtime in minutes", main=  "Runtime for clustering and knn for digit 9")
dev.print(png, '../Report3/runtime_9.png', width=8,height=6,units="in",res=300)

dev.off()

runtime_0

runtime_2


##############
#dendogram

hc_0 <- hclust(dist(train_ind_class_0))
hc_1 <- hclust(dist(train_ind_class_1))
hc_2 <- hclust(dist(train_ind_class_2))
hc_3 <- hclust(dist(train_ind_class_3))
hc_4 <- hclust(dist(train_ind_class_4))
hc_5 <- hclust(dist(train_ind_class_5))
hc_6 <- hclust(dist(train_ind_class_6))
hc_7 <- hclust(dist(train_ind_class_7))
hc_8 <- hclust(dist(train_ind_class_8))
hc_9 <- hclust(dist(train_ind_class_9))

hcd_0 <- as.dendrogram(hc_0)
hcd_1 <- as.dendrogram(hc_1)
hcd_2 <- as.dendrogram(hc_2)
hcd_3 <- as.dendrogram(hc_3)
hcd_4 <- as.dendrogram(hc_4)
hcd_5 <- as.dendrogram(hc_5)
hcd_6 <- as.dendrogram(hc_6)
hcd_7 <- as.dendrogram(hc_7)
hcd_8 <- as.dendrogram(hc_8)
hcd_9 <- as.dendrogram(hc_9)

plot(cut(hcd_0,h=2500)$upper, main = "Digit 0" )
plot(cut(hcd_1,h=2500)$upper, main = "Digit 1" )
plot(cut(hcd_2,h=2500)$upper, main = "Digit 2" )
plot(cut(hcd_3,h=2500)$upper, main = "Digit 3" )
plot(cut(hcd_4,h=2500)$upper, main = "Digit 4" )
plot(cut(hcd_5,h=2500)$upper, main = "Digit 5" )
plot(cut(hcd_6,h=2500)$upper, main = "Digit 6" )
plot(cut(hcd_7,h=2500)$upper, main = "Digit 7" )
plot(cut(hcd_8,h=2500)$upper, main = "Digit 8" )
plot(cut(hcd_9,h=2500)$upper, main = "Digit 9" )



output_kmeans_20 = kmeans(class_1.6$data,center= 20, iter.max = 30,nstart = 10)

class_1.6$labels[output_kmeans_20$cluster==1]

plot(as.dendrogram(hclust(dist(output_kmeans_20$centers))))

amount_1 = length(class_1.6$labels[output_kmeans_20$cluster==1])
max_1 = max(sort(table(class_1.6$labels[output_kmeans_20$cluster==1])))
name_1 = names(sort(table(class_1.6$labels[output_kmeans_20$cluster==1]),decreasing = TRUE))[[1]]


length(class_1.6$labels[output_kmeans_20$cluster==2])
table(class_1.6$labels[output_kmeans_20$cluster==2])

length(class_1.6$labels[output_kmeans_20$cluster==3])
table(class_1.6$labels[output_kmeans_20$cluster==3])

length(class_1.6$labels[output_kmeans_20$cluster==4])
table(class_1.6$labels[output_kmeans_20$cluster==4])

length(class_1.6$labels[output_kmeans_20$cluster==5])
table(class_1.6$labels[output_kmeans_20$cluster==5])

length(class_1.6$labels[output_kmeans_20$cluster==6])
table(class_1.6$labels[output_kmeans_20$cluster==6])

length(class_1.6$labels[output_kmeans_20$cluster==7])
table(class_1.6$labels[output_kmeans_20$cluster==7])

length(class_1.6$labels[output_kmeans_20$cluster==8])
table(class_1.6$labels[output_kmeans_20$cluster==8])

length(class_1.6$labels[output_kmeans_20$cluster==9])
table(class_1.6$labels[output_kmeans_20$cluster==9])

length(class_1.6$labels[output_kmeans_20$cluster==10])
table(class_1.6$labels[output_kmeans_20$cluster==10])

length(class_1.6$labels[output_kmeans_20$cluster==11])
table(class_1.6$labels[output_kmeans_20$cluster==11])

length(class_1.6$labels[output_kmeans_20$cluster==12])
table(class_1.6$labels[output_kmeans_20$cluster==12])

length(class_1.6$labels[output_kmeans_20$cluster==13])
table(class_1.6$labels[output_kmeans_20$cluster==13])

length(class_1.6$labels[output_kmeans_20$cluster==14])
table(class_1.6$labels[output_kmeans_20$cluster==14])

length(class_1.6$labels[output_kmeans_20$cluster==15])
table(class_1.6$labels[output_kmeans_20$cluster==15])

length(class_1.6$labels[output_kmeans_20$cluster==16])
table(class_1.6$labels[output_kmeans_20$cluster==16])

length(class_1.6$labels[output_kmeans_20$cluster==17])
table(class_1.6$labels[output_kmeans_20$cluster==17])

length(class_1.6$labels[output_kmeans_20$cluster==18])
table(class_1.6$labels[output_kmeans_20$cluster==18])


length(class_1.6$labels[output_kmeans_20$cluster==19])
table(class_1.6$labels[output_kmeans_20$cluster==19])

length(class_1.6$labels[output_kmeans_20$cluster==20])
table(class_1.6$labels[output_kmeans_20$cluster==20])

