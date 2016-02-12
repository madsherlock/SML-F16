#-------------------------------------------------------------
#load libraries
#-------------------------------------------------------------
library("png")
library("EBImage")
library("class")
library("gmodels")
library("ggplot2")
library("caret")
library("descr")
library("chemometrics")
library("foreach")
library("ggvis")
source("setworkingdir.R")
#library("lda")
#library("neuralnet")
#library("RSNNS")

#install.packages("neuralnet")

#-------------------------------------------------------------
#Functions to be used in evaluation
#-------------------------------------------------------------

#inspiration for smoothing
smoothImage <- function(grayImg){
  #two ways of specifying kernel:
  # kernel <- matrix( 
  #           c(1, 1, 1, 
  #             1, 1, 1, 
  #             1, 1, 1), # the data elements 
  #           3,              # number of rows 
  #           3)
  # kernel <- kernel/9
  # kernel
  
  kernel <- matrix( 
    1, # the data elements 
    3,# number of rows 
    3)
  kernel <- kernel/9
  print(kernel)
  
  #using r library for smoothing
  smoothed <- filter2(grayImg, kernel)
  
  #simple implementation of average filter:
  # imgWidth <- length(gray[1,])
  # imgHeight <- length(gray[,1])
  # kernelSize <- 1
  # for(px in 1:imgWidth)
  # {
  #   for(py in 1:imgHeight)
  #   {
  #     baseX <- px - kernelSize
  #     endX <- px + kernelSize
  #     if(baseX < 1){baseX<-1}
  #     if(endX > imgWidth){endX<-imgWidth}
  #     
  #     baseY <- py - kernelSize
  #     endY <- py + kernelSize
  #     if(baseY < 1){baseY<-1}
  #     if(endY > imgHeight){endY<-imgHeight}
  #     
  #     
  #     value <- 0
  #     for(pkx in baseX:endX)
  #     {
  #       for(pky in baseY:endY)
  #       {
  #         value <- value+gray[pky,pkx]
  #       }
  #     }
  #     kernelValues <- (endY-baseY+1)*(endX-baseX+1)    
  #     value <- value/kernelValues
  #     
  #     smoothed[py,px] <- value
  #   }
  # }
  return(smoothed)
}


#-------------------------------------------------------------
#This currently loads data according to the paths in the begining.
#Should be modified to load group members data.
#-------------------------------------------------------------
loadSinglePersonsData <- function(DPI,groupNr,groupMemberNr){
  #load the scaned images
  ciffers <- list(readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.png"), collapse = "")))
  #load the corner values
  corners <- read.csv(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Corners.txt"), collapse = ""))
  corners <- trunc(corners*DPI/300)
  print(corners)
  
  #define lists to be used
  #  gray <- list(1:5)
  #   smoothed <- list(1:5)
  prepared <- list(1:5)
  
  
  #convert the images to gray scale.
  for(i in 1:5)
  {
    r <-ciffers[[i]][,,1]
    g <-ciffers[[i]][,,2]
    b <-ciffers[[i]][,,3]
    prepared[[i]] <- (r+g+b)/3
  }
  
  #smooth images
  for(i in 1:5)
  {
    prepared[[i]] <- smoothImage(prepared[[i]])
  }
  
  #generate image that is prepared for learning and visualization
  #   for(i in 1:5)
  #   {
  #     prepared[[i]] <- smoothed[[i]]
  #   }
  
  
  #extract individual ciffers
  xStep  <- (corners[1,7]-corners[1,1])/20;
  yStep  <- (corners[1,8]-corners[1,2])/20;
  xStepT <- 60*DPI/300#trunc(xStep)
  yStepT <- 60*DPI/300#trunc(yStep)
  
  tempM <- matrix(,20*20,(yStepT-2)*(xStepT-2))
  trainingDigit <- list(1:10);
  
  for(pages in 1:5)
  {
    for(box in 1:2)
    {
      #     trainingDigit[[(pages-1)*2 + box]] <- matrix(,20*20,(yStepT-2)*(xStepT-2))) 
      for(cifX in 1:20)
      {
        aXbase <- corners[(pages-1)*2 + box,1] + xStep*(cifX-1)
        for(cifY in 1:20)
        {
          aYbase <- corners[(pages-1)*2 + box,2] + yStep*(cifY-1)
          
          for(px in 1:xStepT-2)
          {
            for(py in 1:yStepT-2)
            {
              tempM[(cifY-1)*20 + cifX, (px-1)*(yStepT-2) + py] <- prepared[[pages]][aYbase+py+1,aXbase+px+1]
            }
          }
        }
      }
      trainingDigit[[(pages-1)*2 + box]] <- tempM
    }
  }
  
  #color grid to show whats used for training
  for(pages in 1:5)
  {
    for(box in 1:2)
    {
      for(cifX in 1:21)
      {
        aXbase <- corners[(pages-1)*2 + box,1] + xStep*(cifX-1)
        xStart <- aXbase-1
        xEnd <- aXbase+1
        for(px in xStart:xEnd)
        {
          for(py in corners[(pages-1)*2 + box,2]:corners[(pages-1)*2 + box,8])
          {
            prepared[[pages]][py,px] <- 0.0
          }
        }
        
        aYbase <- corners[(pages-1)*2 + box,2] + yStep*(cifX-1)
        yStart <- aYbase-1
        yEnd <- aYbase+1
        for(py in yStart:yEnd)
        {
          for(px in corners[(pages-1)*2 + box,1]:corners[(pages-1)*2 + box,7])
          {
            prepared[[pages]][py,px] <- 0.0
          }
        }
      }
    }
  }
  
  #show image
  #   img <- ciffers[[1]]
  #   img[,,1] <- prepared[[1]]
  #   img[,,2] <- prepared[[1]]
  #   img[,,3] <- prepared[[1]]
  display(prepared[[1]])
  display(prepared[[2]])
  display(prepared[[3]])
  display(prepared[[4]])
  display(prepared[[5]])
  
  
  
  
  #use the generated training data to apply learning
  print( trainingDigit[[1]] )
  
  return(trainingDigit)
}



getRandomSplit <- function(trainingData, trainingPc){
  #randomly split data in a balanced maner:
  trainL = list(1:length(trainingData))
  testL = list(1:length(trainingData))
  for(Nr in 0:(length(trainingData)-1))
  {
    amountEachNumber = nrow(trainingData[[Nr+1]]);
    
    set.seed(1) ## make randomness reproducible here
    rand <- sample(amountEachNumber) #generate  a randomly ordered indexing list the size of the datasample
    
    vector = c(1:amountEachNumber);
    for(i in 1:trunc(amountEachNumber*trainingPc))
    {
      vector[i] = 1;
    }
    for(i in trunc(amountEachNumber*trainingPc)+1:amountEachNumber)
    {
      vector[i] = 2;
    }
    splittingIndexer = vector[rand]
    splitData <- split.data.frame(trainingData[[Nr+1]], factor(splittingIndexer))
    
    trainL[[Nr+1]] <- splitData[[1]]
    testL[[Nr+1]]<- splitData[[2]]
  }  
  
  training <- trainL[[1]]
  testing <- testL[[1]]
  trainClass <- rep(0,nrow(trainL[[1]]) )
  testClass <- rep(0,nrow(testL[[1]]) )
  for(i in 2:10)
  {
    training <- rbind(training, trainL[[i]])
    testing <- rbind(testing, testL[[i]])
    
    trainClass <- append(trainClass, rep(i-1,nrow(trainL[[i]]) ) )
    testClass <- append(testClass, rep(i-1,nrow(testL[[i]]) ) )
  }
  trainClassF <- factor(trainClass)
  testClassF <- factor(testClass)
  
  print(nrow(training) );
  print(length(trainClassF) );
  print(nrow(testing) );
  print(length(testClassF) );
  
  return(list(training, testing, trainClassF, testClassF))
  
  
  #non balanced random split
  # retList[[5]][]
  # dataSamples = nrow(retList[[1]]);
  # 
  # set.seed(1) ## make reproducible here, but not if generating many random samples
  # rand <- sample(dataSamples) #generate  a randomly ordered indexing list the size of the datasample
  # print(rand)
  # vector = c(1:dataSamples);
  # for(i in 1:(dataSamples/2))
  # {
  #   vector[i] = 1;
  # }
  # for(i in (dataSamples/2+1):dataSamples)
  # {
  #   vector[i] = 2;
  # }
  # print(vector)
  # splittingIndexer = vector[rand]
  # print(splittingIndexer)
  #   
  #   
  # splitData <- split.data.frame(retList[[1]], factor(splittingIndexer))
  # training = splitData[[1]]
  # testing = splitData[[2]]
  # 
  # splitClass <- split(retList[[3]], factor(splittingIndexer))
  # trainClassF = splitClass[[1]]
  # testClassF = splitClass[[2]]
}



#-------------------------------------------------------------
#This is the "main function" or the code that is actualy run
#-------------------------------------------------------------

DPI = 100;
NrOfTrainingSets = 2;
NrOfTestingSets = 1;


NrOfDataSets = NrOfTrainingSets + NrOfTestingSets;
fullDataList = list(1:NrOfDataSets)
fullModList = list(1:NrOfDataSets)


#add as many people as you have specified training and test data

#Data used for Training
fullDataList[[1]] = loadSinglePersonsData(DPI,2,1); gc(); #Kiddi
fullDataList[[2]] = loadSinglePersonsData(DPI,1,1); gc(); #Should be Mikael - G1M1 

#Data used for testing
fullDataList[[3]] = loadSinglePersonsData(DPI,1,2); gc(); #Group1 Member2  - G1M2

#fullDataList[[4]] = loadSinglePersonsData(DPI,2,2); gc();
#fullDataList[[5]] = loadSinglePersonsData(DPI,3,1); gc();
#fullDataList[[6]] = loadSinglePersonsData(DPI,3,2); gc();
#fullDataList[[7]] = loadSinglePersonsData(DPI,4,1); gc();
#fullDataList[[8]] = loadSinglePersonsData(DPI,4,2); gc();
#fullDataList[[9]] = loadSinglePersonsData(DPI,4,3); gc();
#fullDataList[[9]] = loadSinglePersonsData(DPI,5,1); gc();
#fullDataList[[10]] = loadSinglePersonsData(DPI,5,2); gc();
#fullDataList[[11]] = loadSinglePersonsData(DPI,6,1); gc();
#fullDataList[[12]] = loadSinglePersonsData(DPI,6,2); gc();
#fullDataList[[12]] = loadSinglePersonsData(DPI,7,1); gc();
#fullDataList[[15]] = loadSinglePersonsData(DPI,7,3); gc();

for(i in 1:NrOfTrainingSets)  
{  
  for(j in 1:10)    
  {    
    if(i == 1 && j == 1)      
    {      
      training = fullDataList[[i]][[j]];      
      trainClass = rep(j-1,nrow(fullDataList[[i]][[j]]));      
    }    
    else      
    {      
      training = rbind(training, fullDataList[[i]][[j]]);      
      trainClass = append(trainClass, rep(j-1,nrow(fullDataList[[i]][[j]]) ) );      
    }    
  }  
}

for(i in 1:NrOfTestingSets)  
{  
  for(j in 1:10)    
  {    
    if(i == 1 && j == 1)      
    {      
      testing = fullDataList[[i + NrOfTrainingSets]][[j]];      
      testClass = rep(j-1,nrow(fullDataList[[i + NrOfTrainingSets]][[j]]));      
    }    
    else      
    {      
      testing = rbind(testing, fullDataList[[i + NrOfTrainingSets]][[j]]);
      testClass = append(testClass, rep(j-1,nrow(fullDataList[[i + NrOfTrainingSets]][[j]]) ) ); 
    } 
  }
}
nnTrainingClass <- matrix(nrow = nrow(training), ncol = 10)
for(i in 1:nrow(training))  
{  
  for(j in 1:10)    
  {
    nnTrainingClass[i,j] <- 0;
    if(testClass[i] == j-1)
    {
      nnTrainingClass[i,j] <- 1;
    }
  }
}
#nnTrainingClass

#contain data from the first two persons: 
trainClassF = factor(trainClass)

#contain data from the third person:
testClassF = factor(testClass)

str(training)
str(trainClass)
str(testing)
str(testClass)


##KNN

#Data Normalization
normalize <- function(x){
  num <- x-min(x)
  denum <- max(x)-min(x)
  return(num/denum)
}
#testing_normalized = as.data.frame(lapply(testing, normalize))
#training_normalized = as.data.frame(lapply(training, normalize))

#knn
#test_pred = knn(training,testing,trainClass,k=15)
#CrossTable - Confusion Matrix
#CrossTable(x=testClass,y= test_pred, prop.chisq = FALSE)
#
#mean(test_pred == testClass)



table_generator <- function(training, testing, testing_class, train_class, maxK){
  output =  data.frame(x=numeric(maxK), y =numeric(maxK), stringsAsFactors = FALSE)
  for(k in 1:maxK){
    message("iteration: " , k)
    output$x[k] = k
    predicted = knn(training,testing,trainClass,k)
    output$y[k] = mean(predicted == testing_class)
  }
  return(output)
}

erro= table_generator(training,testing,testClass,trainClass,100)

