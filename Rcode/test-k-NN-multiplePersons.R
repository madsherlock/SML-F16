source("crossValidationKNN.R")
source("parallelize.R")
source("loadMultiplePersonsData.R")
source("listOfDigits2classificationData.R")
source("getRandomSplit.R")
source("knnPerformanceNoCV.R")

persons = list(
  c(1,1),
  # c(1,2),
  c(2,1),
  # c(2,2),
  c(3,1),
  # c(3,2),
  c(4,1),
  # c(4,2),
  # c(4,3),
  c(5,1),
  # c(5,2),
  # c(5,3),
  c(6,1),
  # #c(6,2), #Bad file naming
  c(7,1),
  c(8,1),
  c(10,1),
  # c(10,2),
  c(11,1)
  # c(11,2),
  # c(11,3), #Updated his/her Ciphers recently. Should be OK.
  # c(13,1)
  # #c(14,1), #Bug X
  # #c(14,2) #Bug X
)
#DPI_list=c(100,200,300)
#sigma_list=c(0.2, 0.3, 0.6, 1.0, 1.5, 2.0, 2.5, 3, 3.5, 5.0);
DPI=100
sigma=1.5
k_list=c(1,2,3,5,7,11,30,40,80)#,seq(15,50,5),seq(60,100,10))
#Cross validation setup
n_folds = 10 # 90/10 % split
n_repeats = 3 # 10 times 90/10 % split

#All persons in both training and data. Random splits.
# cl = parallelizeMe() #8 cores (about 10 GB RAM)
# runCrossValidationKNNMultiplePersons(persons,c(100),c(1.5),k_list,n_folds=n_folds,n_repeats=1)
# unParallelizeMe(cl)
# gc()
# 
# cl = parallelizeMe() #8 cores (about 10 GB RAM)
# runCrossValidationKNNMultiplePersons(persons,c(100),c(1.5),k_list,n_folds,n_repeats)
# unParallelizeMe(cl)
# gc()
# cl = parallelizeMe(5) #Limit cores!
# runCrossValidationKNNMultiplePersons(persons,c(200),c(2.5),k_list,n_folds,n_repeats)
# runCrossValidationKNNMultiplePersons(persons,c(300),c(3.5),k_list,n_folds,n_repeats)
# unParallelizeMe(cl)



testKNNMultiAll <- function() {
  myData = loadMultiplePersonsDataByDigitMerged(DPI=DPI,persons=persons,sigma=sigma)
  myDataL = listOfDigits2classificationDataAlt(myData)
  
  #myDataT = getRandomSplit(myData,0.9)
  #knnResult = knn3Train(train=myDataT$training,test=myDataT$testing,cl=myDataT$trainClassF,k=1)
  
  remove(myData)
  trCtrl = trainControl(
    method = "repeatedcv",  #Repeated k-fold cross validation
    number = n_folds,       #Number of folds
    repeats = n_repeats,    #Number of repetitions (complete sets of folds to cross validate)
    verboseIter = TRUE,     #Print training log
    returnData = FALSE,     #A logical for saving the data???
    seeds = NA,           #A value of NA will stop the seed from being set within the worker processes
    #while a value of NULL will set the seeds using a random set of integers.
    savePredictions = "final" #"final" saves the predictions for the optimal tuning parameters.
  )
  cl=parallelizeMe()
  for(myK in k_list) {
    cat(format(Sys.time(), "%X"))
    cat("\nk =",myK,"\n\r")
    
    filename = "../data/knnMulti-"
    for(p in persons) {
      filename = paste(c(filename,p[1],"-",p[2],"-"),collapse="")
    }
    filename = paste(c(filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
    
    #filename = paste(c("../data/knnMulti-1-1-2-1-3-1-4-1-5-1-6-1-dpi100-sig1.5-k-",myK,"-rep",n_repeats,".RData"),collapse = "")
    if(file.exists(filename)) {
      #load(file=filename)
      #knnCV
    }
    else {
      knnCV = train(
        x = myDataL$data,
        y = myDataL$dataClassF,
        preProcess = NULL,
        method = "knn",
        tuneGrid = expand.grid(k=myK),
        trControl = trCtrl #See trCtrl above
      )
      save(knnCV,file = filename)
    }
  }
  unParallelizeMe(cl)
  
  
  knnCVResultsAll=data.frame()
  
  for(myK in k_list) {
    filename = "../data/knnMulti-"
    for(p in persons) {
      filename = paste(c(filename,p[1],"-",p[2],"-"),collapse="")
    }
    filename = paste(c(filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
    if(file.exists(filename)) {
      load(filename)
      knnCVResultsAll=rbind(knnCVResultsAll,knnCV$results)
      #knnCV
    }
    else {
      cat("File",filename,"does not exist.\n\r")
    }
  }
  
  filename = "../data/knnMulti-"
  for(p in persons) {
    filename = paste(c(filename,p[1],"-",p[2],"-"),collapse="")
  }
  filename = paste(c(filename,"dpi",DPI,"-sig",sigma,"-rep",n_repeats,"-results.csv"),collapse="")
  write.csv(knnCVResultsAll,file=filename,row.names=FALSE)
  
}








testKNNMultiLeaveOneOut <- function() {
  
  myData = loadMultiplePersonsDataByDigitMerged(DPI=DPI,persons=persons,sigma=sigma)
  myDataL = listOfDigits2classificationDataAlt(myData)
  remove(myData)
  
  
  index = list()	#a list with elements for each resampling iteration.
  np=length(persons)
  allIdx = 1:(np*4000)
  #Each list element is a vector of integers corresponding to the rows used for training at that iteration.
  for(idx in 0:(np-1)) {
    rem=(idx*400+1):(400+idx*400)
    for(idx2 in 1:9) {
      rem=c(rem,    (np*idx2*400+1):(400+np*idx2*400)     )
    }
    index[[idx+1]]=allIdx[-(rem)]
  }
  cat("Index:\n\r")
  str(index)
  
  trCtrl = trainControl(
    method = "repeatedcv",  #Repeated k-fold cross validation
    number = length(persons),       #Number of folds. Leave one person out
    repeats = min(n_repeats,length(persons)),    #Number of repetitions (complete sets of folds to cross validate)
    verboseIter = TRUE,     #Print training log
    returnData = FALSE,     #A logical for saving the data???
    returnResamp = "all", #how much of the resampled summary metrics should be saved. Values can be "final", "all" or "none"
    index = index,          #This is how we ensure that one person's data is left out!
    seeds = NA,           #A value of NA will stop the seed from being set within the worker processes
    #while a value of NULL will set the seeds using a random set of integers.
    savePredictions = "final" #"final" saves the predictions for the optimal tuning parameters.
  )
  cl=parallelizeMe()
  for(myK in k_list) {
    cat(format(Sys.time(), "%X"))
    cat("\nk =",myK,"\n\r")
    
    filename = "../data/knnMultiLOO-"
    for(p in persons) {
      filename = paste(c(filename,p[1],"-",p[2],"-"),collapse="")
    }
    filename = paste(c(filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
    
    if(file.exists(filename)) {
      #load(file=filename)
      #knnCV
    }
    else {
      knnCV = train(
        x = myDataL$data,
        y = myDataL$dataClassF,
        preProcess = NULL,
        method = "knn",
        tuneGrid = expand.grid(k=myK),
        trControl = trCtrl #See trCtrl above
      )
      save(knnCV,file = filename)
    }
  }
  unParallelizeMe(cl)
  
  
  knnCVResultsAll=data.frame()
  
  for(myK in k_list) {
    filename = "../data/knnMultiLOO-"
    for(p in persons) {
      filename = paste(c(filename,p[1],"-",p[2],"-"),collapse="")
    }
    filename = paste(c(filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
    if(file.exists(filename)) {
      load(filename)
      knnCVResultsAll=rbind(knnCVResultsAll,knnCV$results)
      #knnCV
    }
    else {
      cat("File",filename,"does not exist.\n\r")
    }
  }
  
  filename = "../data/knnMultiLOO-"
  for(p in persons) {
    filename = paste(c(filename,p[1],"-",p[2],"-"),collapse="")
  }
  filename = paste(c(filename,"dpi",DPI,"-sig",sigma,"-rep",n_repeats,"-results.csv"),collapse="")
  write.csv(knnCVResultsAll,file=filename,row.names=FALSE)
}


confmatKNNMultiLeaveOneOut <- function(testPersonIndex) {
  personsTrain = persons[-testPersonIndex]
  personsTest = persons[testPersonIndex]
  
  myDataTrain = listOfDigits2classificationDataAlt(
    loadMultiplePersonsDataByDigitMerged(DPI=DPI,persons=personsTrain,sigma=sigma))
  myDataTest = listOfDigits2classificationDataAlt(
    loadMultiplePersonsDataByDigitMerged(DPI=DPI,persons=personsTest,sigma=sigma))
  
  knnPerformance = knnPerformanceNoCVSpecific(
    dataTraining = myDataTrain$data,
    dataTrainingClassF = myDataTrain$dataClassF,
    dataTesting = myDataTest$data,
    dataTestingClassF = myDataTest$dataClassF,
    k_list = 1
  )
  filenameConMat=paste(c("../data/confmatkNNMultiLOO",
                         "-dpi",DPI,
                         "-sig",sigma,
                         "-k",knnPerformance$k_list[1],
                         "-n",knnPerformance$dataset_size,
                         ".csv"), collapse = "")
  conMat=as.table(knnPerformance$knnConfusionMatrix[[1]])
  #Rows: Prediction. Columns: Reference. So conMat[10,4] is number of 3's incorrectly predicted as 9's.
  write.csv(x=conMat,file=filenameConMat)
}

confmatKNNMultiAll <- function() {
  myData = loadMultiplePersonsDataByDigitMerged(DPI=DPI,persons=persons,sigma=sigma)
  myDataL = listOfDigits2classificationDataAlt(myData)
  remove(myData)
  
  knnPerformance=knnPerformanceNoCV(data=myDataL$data,
                                    dataClassF=myDataL$dataClassF,
                                    trainPercent = 0.9,
                                    k_list=1)
  filenameConMat=paste(c("../data/confmatkNNMultiAll-",
                         "-dpi",DPI,
                         "-sig",sigma,
                         "-k",knnPerformance$k_list[1],
                         "-n",knnPerformance$dataset_size,
                         ".csv"), collapse = "")
  conMat=as.table(knnPerformance$knnConfusionMatrix[[1]])
  #Rows: Prediction. Columns: Reference. So conMat[10,4] is number of 3's incorrectly predicted as 9's.
  write.csv(x=conMat,file=filenameConMat)
}





# TESTING SECTION @TODO
#testKNNMultiLeaveOneOut()
#testKNNMultiAll()

confmatKNNMultiLeaveOneOut(testPersonIndex = 1)
confmatKNNMultiAll()

#@TODO
#Interpret resampling results of LeaveOneOut data
#to see which one of the tested persons has
#the weirdest handwriting compared to the rest.
