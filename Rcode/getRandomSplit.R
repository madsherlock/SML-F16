
#Stratified random sampling (digit distribution is 10% per digit in both training and testing data)
getRandomSplit <- function(trainingAndTestingData, trainingPercentage){
  #randomly split data in a balanced maner:
  trainL = list(1:length(trainingAndTestingData))
  testL = list(1:length(trainingAndTestingData))
  for(Nr in 0:(length(trainingAndTestingData)-1))
  {
    amountEachNumber = nrow(trainingAndTestingData[[Nr+1]]);
    
    #set.seed(1) ## make randomness reproducible here
    rand <- sample(amountEachNumber) #generate  a randomly ordered indexing list the size of the datasample
    
    vector = c(1:amountEachNumber);
    for(i in 1:trunc(amountEachNumber*trainingPercentage))
    {
      vector[i] = 1;
    }
    for(i in trunc(amountEachNumber*trainingPercentage)+1:amountEachNumber)
    {
      vector[i] = 2;
    }
    splittingIndexer = vector[rand]
    splitData <- split.data.frame(trainingAndTestingData[[Nr+1]], factor(splittingIndexer))
    
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
  
  #print(nrow(training) );
  #print(length(trainClassF) );
  #print(nrow(testing) );
  #print(length(testClassF) );
  
  return(list(training=training, testing=testing, trainClassF=trainClassF, testClassF=testClassF))
  
}