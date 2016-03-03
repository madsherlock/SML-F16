#k-NN
library("caret")

#Calculate k-NN performance of one run for each k (no cross validation).
#Returns confusion matrix including accuracy, for each k,
#as well as prediction running time (seconds) for each k.
#The time statistic does not include CPU time spent outside R.
knnPerformanceNoCV <- function(data,dataClassF,trainPercent=0.9,k_list=c(1,2,3))
{
  # Now, we must split the data into training and testing sets.
  # see caret's createDataPartition or createFolds
  #splitData = createFolds(y=data,k=10,list=TRUE,returnTrain=FALSE);
  #splitData = createDataPartition(y=data,times=2,p=0.9,list=TRUE,groups=10);
  #nah, you know what, let's use the Rcode2.R example:
  
  #make an index list to scramble the data
  IdScrambler <- sample(nrow(data)); #Random permutation (reordering of indices)
  scrambledData <- data[IdScrambler,] #the comma is important to specify you want the entire row.
  scrambledDataClassF <- dataClassF[IdScrambler]
  # place the first % of the data in the training set
  percentile <- nrow(data)*trainPercent;
  dataTraining <- scrambledData[1:percentile, ]
  dataTrainingClassF <- scrambledDataClassF[1:percentile]
  # place the last % of the data in the testing set
  dataTesting <- scrambledData[(percentile+1):nrow(data), ]
  dataTestingClassF <- scrambledDataClassF[(percentile+1):nrow(data)]
  #some classifiers need the class label as factors, so
  #here the classes are mapped to factors:
  remove(scrambledData,scrambledDataClassF)
  gc();
  
  
  #k-NN Time!
  #knnTime <- list(1:length(k_list))
  knnTime <- vector(length=length(k_list))
  knnConfusionMatrix <- list(1:length(k_list))
  for(i in 1:length(k_list))
  {
    #Measure time
    #Predict
    timer = proc.time()
    knnPrediction = knn3Train(train=dataTraining,test=dataTesting,cl=dataTrainingClassF,k=k_list[i],prob=TRUE);
    knnTime[i] = (proc.time() - timer)[[1]]
    #Store accuracy and confusion
    knnConfusionMatrix[[i]] = confusionMatrix(data=knnPrediction,reference=dataTestingClassF)
  }
  
  return(list(dataset_size = length(dataClassF),k_list=k_list,knnTime=knnTime,knnConfusionMatrix=knnConfusionMatrix))
}