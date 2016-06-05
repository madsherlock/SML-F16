source("preloading.R")
source("loadLabeledDataset.R")
source("parallelize.R")
source("knnPerformanceNoCV.R")
source("normalize.R")
source("smlPCA.R")
source("displayDigit.R")
library(stats)



getLabeledDatasetFilename <- function(list_of_persons, DPI, sigma, dataset_name = paste(unlist(list_of_persons),collapse="-")) {
  filename_prefix = paste(c("../data/labeled_datasets/data-labeled-",dataset_name),collapse="")
  filename = paste(c(filename_prefix,"-dpi",DPI,"-sigma",sigma,".RData"),collapse="")
  return(filename)
}

# ---------------------------------------------------------------
# ------------------Testing method-------------------------------
# ---------------------------------------------------------------
# Define standard datasets and how to measure statistics on them.
# Eg. person dependent:
#   For each person, do 10-fold CV.
#   Take average and std. deviation of each person's accuracy.
# Eg. All mixed:
#   Mix data from all people, do 10-fold CV with every person in both train and test.
#   Measure accuracy and standard deviation.
# Eg. All LOO:
#   For each person P:
#     Test set: P's data.
#     Train set: data of remaining people.
#     Use train set to train model, measure accuracy on test set.
#   Take average and std. deviation of accuracy for all combinations.
#   Eg. if there are 15 people, there will be 15 overall accuracies to average.
# TODO: More variations?
#
#
#
# ---------------------------------------------------------------
# ------------------Preprocessing--------------------------------
# ---------------------------------------------------------------
# Load data:
#   Crop each digit box to center 80 %   (remove 10% in every border).
#
# Smoothen data to find optimal DPI and sigma:
#   Find optimal smoothing parameter sigma wrt. accuracy.
#      May vary with #neighbours, so vary that as well.
#   Find optimal DPI wrt. running time.
#   Do this on one person's data and assume the results are generalizable.
#   Conclude ONE DPI-sigma combination to rule them all based on accuracy and running time.
#
# Find acceptable PCA thresholds (% of cumulative variance explained).
#   Try 5 different thresholds: 90, 95, 99 and 100 %.
#      The 100 % data is known for one person in advance due to the smoothing test.
#   Measure accuracy vs. k.
#   Measure running time.
#   Do this on various datasets, all using the one DPI-sigma combination to rule them all.
#   
# Visualize principal components.
#   Verify that we have created digit recognizers, not grid detectors.
#   If not verified, go back to step 1 and intensify cropping.
#
#
#
# ---------------------------------------------------------------
# ------------------Method 1: k-NN-------------------------------
# ---------------------------------------------------------------
# TODO
#
#
#
#
#
# ---------------------------------------------------------------
# ------------------Method 2: SVM--------------------------------
# ---------------------------------------------------------------
# TODO





preloadEverything(FALSE)





#Smoothing test:
# Find the optimal smoothing parameter for each DPI,
# based on the Overall accuracy with no other preprocessing.

# Input parameters
k_list = seq(1,11,1)
big_k_list = seq(1,50,1)
n_folds = 10
#n_repeats = 10 #Only 1 repeat of 10-fold CV is necessary, according to NK
n_repeats = 1
testPerson = persons_persondependent[[1]]

#Create fold indices for CV.
#Each element of pdFolds is the indices for the training data
#of one fold. This means for 10-fold cross validation with 10 runs,
#pdFolds will have 100 elements, each being a set of indices to use for
#training. Set seed to 4578 to always create the same folds,
#so that the obtained results are reproducible.
#
# NOTE: Only 1 run, so only 10 list elements are created in pdFolds.
#
dataset_filename = getLabeledDatasetFilename(
  persons_persondependent,
  DPI=100,
  sigma=loading_setups$persondependent[[1]]$sigma_list[1])
data_persondependent_labels = loadLabeledDataset(dataset_filename)$labels
set.seed(4578)
pdFolds = createMultiFolds(data_persondependent_labels,k=n_folds,times=n_repeats)
# pdFolds = createMultiFolds(1:(4000*length(persons_persondependent)),
#                            k=n_folds,times=n_repeats)


pdTrCtrl = trainControl(
  method = "repeatedcv",  #Repeated k-fold cross validation
  number = n_folds,       #Number of folds
  repeats = n_repeats,    #Number of repetitions (complete sets of folds to cross validate)
  verboseIter = TRUE,     #Print training log
  returnData = FALSE,     #A logical for saving the data???
  seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
                          #while a value of NULL will set the seeds using a random set of integers.
  #savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
  index = pdFolds
)
# Output parameters

# Data generation loop (Takes a long time, like 8 hours or something)
cat("Time: ",format(Sys.time(), "%X"),"\n\rGenerate sigma-k-accuracy data for persondependent smoothing:\n\r")
cl = parallelizeMe()
for(loading_setup in loading_setups$persondependent) {
  DPI = loading_setup$DPI
  sigma_list = loading_setup$sigma_list
  cat("Time: ",format(Sys.time(), "%X"),"\n\rDPI: ",DPI,", sigmas: ", sigma_list, "\n\r")
  #Calculate k-NN accuracy for all k-sigma combinations
  for(sigma in sigma_list) {
    cat("Time: ",format(Sys.time(), "%X"),"\n\r sigma: ", sigma, "\n\r")
    dataset_filename = getLabeledDatasetFilename(persons_persondependent,DPI=DPI,sigma=sigma)
    data_persondependent = loadLabeledDataset(dataset_filename)
    for(myK in k_list) {
      knnCV_filename = "../data/knnPersonDependentCV-"
      knnCV_filename = paste(c(knnCV_filename,"G",testPerson[1],"M",testPerson[2],"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
      if(!file.exists(knnCV_filename)) {
        knnCV = train(
          x = data_persondependent$data,
          y = data_persondependent$labels,
          preProcess = NULL,
          method = "knn",
          tuneGrid = expand.grid(k = myK),
          trControl=pdTrCtrl
        )
        cat("DPI =",DPI, "sigma =",sigma,"k =",knnCV$results[[1]],"Accuracy = ",100*knnCV$results[[2]],"%\n\r")
        gc()
        save(knnCV,file = knnCV_filename)
      }
    }
  }
}
unParallelizeMe(cl)
# Data processing loop 1:
# Make table of sigma, k, Accuracy, Kappa, AccuracySD, KappaSD
knnCVResultsBest=data.frame()
for(loading_setup in loading_setups$persondependent) {
  DPI = loading_setup$DPI
  sigma_list = loading_setup$sigma_list
  cat("Writing",DPI,"DPI person dependent results to a csv file.")
  knnCVResults=data.frame()
  best_sigma = sigma_list[1]
  best_k = k_list[1]
  best_acc=0.0
  for(sigma in sigma_list) {
    for(myK in k_list) {
      knnCV_filename = "../data/knnPersonDependentCV-"
      knnCV_filename = paste(c(knnCV_filename,"G",testPerson[1],"M",testPerson[2],"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
      load(file = knnCV_filename)
      #cat(str(c("sigma"=sigma,knnCV$results)))
      knnCVResults=rbind(knnCVResults,c("sigma"=sigma,knnCV$results))
      if(knnCV$results$Accuracy[1] > best_acc) {
        best_acc = knnCV$results$Accuracy[1]
        best_sigma = sigma
        best_k = myK
      }
    }
  }
  knnCVResultsBest=rbind(knnCVResultsBest,c("DPI" = DPI, "sigma" = best_sigma, "k" = best_k))
  knnCV_filename = "../data/knnPersonDependentCV-"
  knnCV_filename = paste(c(knnCV_filename,"G",testPerson[1],"M",testPerson[2],"dpi",DPI,"-rep",n_repeats,"-results.csv"),collapse="")
  write.table(knnCVResults,
              file=knnCV_filename,
              row.names=FALSE,
              col.names=c("sigma","k","Accuracy","Kappa","AccuracySD","KappaSD"),
              sep=',')
  cat("Done\n\r")
}
save(x=knnCVResultsBest,file=paste(c(
  "../data/knnPersonDependentCV-G",testPerson[1],"M",testPerson[2],
  "-rep",n_repeats,"-best_sigma_k.RData"),collapse=""))
# Data processing loop 2:
# Make confusion matrices for optimal sigma-k combinations
# from already carried out cross validation results.
load(file=paste(c("../data/knnPersonDependentCV-G",testPerson[1],"M",testPerson[2],
     "-rep",n_repeats,"-best_sigma_k.RData"),collapse=""))
for(i in 1:nrow(knnCVResultsBest)) {
  DPI=knnCVResultsBest[[1]][i]
  sigma=knnCVResultsBest[[2]][i]
  myK=knnCVResultsBest[[3]][i]
  knnCV_filename = "../data/knnPersonDependentCV-"
  knnCV_filename = paste(c(knnCV_filename,"G",testPerson[1],"M",testPerson[2],"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
  load(file = knnCV_filename)
  conMat = confusionMatrix(knnCV,norm="none")
  filenameConMat=paste(c("../data/knnPersonDependentCV-G",
                         testPerson[1],"M",testPerson[2],
                         "dpi",DPI,
                         "-sig",sigma,
                         "-k",myK,
                         "-confusionMatrix.csv"), collapse = "")
  #Rows: Prediction. Columns: Reference. So conMat[10,4] is number of 3's incorrectly predicted as 9's.
  write.csv(x=(n_folds*n_repeats)*conMat$table,file=filenameConMat)
}







DPI_idx=1
DPI=100*DPI_idx
sigma=loading_setups$all_loadable[[DPI_idx]]$sigma_list[[1]]






# ---------------------------------------------------------------
# ------------------PCA------------------------------------------
# ---------------------------------------------------------------
dataset_filename = getLabeledDatasetFilename(persons_all_loadable,DPI=DPI,sigma=sigma)
data_pcaAll = loadLabeledDataset(dataset_filename)
dataset_filename = getLabeledDatasetFilename(persons_persondependent,DPI=DPI,sigma=sigma)
data_pcaPD = loadLabeledDataset(dataset_filename)
dataset_filename = getLabeledDatasetFilename(persons_fewer,DPI=DPI,sigma=sigma)
data_pcaFewer = loadLabeledDataset(dataset_filename)

# Generate data for plotting cumulated variance and for plotting the principal components themselves
for (loading_setup in loading_setups$all_loadable) {
  DPI = loading_setup$DPI
  sigma_list = loading_setup$sigma_list
  for(sigma in sigma_list) {
    dataset_filename = getLabeledDatasetFilename(persons_all_loadable,DPI=DPI,sigma=sigma)
    data_pcaAll = loadLabeledDataset(dataset_filename)
    normalized_data = normalizeZscore(data_pcaAll$data)
    testPCAAll=smlPCA(normalized_data)
    write.csv(testPCAAll$sdev,file=paste(c("../data/pca-All-sdev-dpi",DPI,"-sigma",sigma,".csv"),collapse=""), row.names = FALSE)
    for(i in 1:10) {
      #displayDigit(data_pcaPD$data[77*i,],paste(c("dig-",77*i,"-",DPI,".png"),collapse=""))
      pc=(abs(testPCAAll$rotation[,i])-min(abs(testPCAAll$rotation[,i])))/(max(abs(testPCAAll$rotation[,i]))-min(abs(testPCAAll$rotation[,i])))
      displayDigit(pc,paste(c("../data/pca-All-dpi",DPI,"-sigma",sigma,"-pc",i,".png"),collapse=""))
    }
  }
}




# The elbow-method chosen PCA threshold
pcaThresh=0.95













# ---------------------------------------------------------------
# ------------------Parameter optimization------------------------
# ------------------on a smaller dataset-------------------------
# ---------------------------------------------------------------
# We optimize on Fewer, LOO dataset


#Create fold indices for CV.
# We only have one set of 10 folds, so just 1 run.
fewerLOOFolds = list()#createMultiFolds(data_fewer_labels,k=n_folds,times=n_repeats)
np=length(persons_fewer) #10 persons
allIdx = 1:(np*4000)
#Each list element is a vector of integers corresponding to the rows used for training at that iteration.
#For each person, make a fold.
for(idx in 0:(np-1)) {
  #Remove current person's zeroes from this fold.
  rem=(idx*400+1):(400+idx*400)
  #Remove current person's ones, twos, ... , nines from this fold.
  for(idx2 in 1:9) {
    rem=c(rem,    (np*idx2*400+1):(400+np*idx2*400)     )
  }
  #Append to fewerLOOFolds, the new fold which does not contain current person.
  fewerLOOFolds[[idx+1]]=allIdx[-(rem)]
}
cat("Index:\n\r")
str(fewerLOOFolds)


set.seed(4578)
fewerSmallLOOFolds = list()
for(idx in 0:(np-1)) {
  smallFold = createFolds(
    y=data_pcaFewer$labels[fewerLOOFolds[[idx+1]]],
    k = 10,
    returnTrain = FALSE
  )[[1]]
  fewerSmallLOOFolds[[idx+1]]=smallFold
}

fewerLOOPCATrCtrl = trainControl(
  method = "repeatedcv",  #Repeated k-fold cross validation
  number = n_folds,       #Number of folds
  repeats = 1,    #Number of repetitions (complete sets of folds to cross validate)
  verboseIter = TRUE,     #Print training log
  returnData = FALSE,     #A logical for saving the data???
  seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
  #while a value of NULL will set the seeds using a random set of integers.
  savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
  preProcOptions = c(thresh=pcaThresh),
  index = fewerLOOFolds
)


#Data generation, PCA, fewer LOO
cat("Time: ",format(Sys.time(), "%X"),"\n\rk-NN: Generate accuracy vs. k data for fewer LOO, PCA:\n\r")
cl = parallelizeMe()
cat("Time: ",format(Sys.time(), "%X"),"\n\r")
for(myK in big_k_list) {
  knnCV_filename = "../data/pcaknnFewerLOOCV-"
  knnCV_filename = paste(c(knnCV_filename,"thresh",pcaThresh,
                           "-dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
  if(!file.exists(knnCV_filename)) {
    knnCV = train(
      x = data_pcaFewer$data,
      y = data_pcaFewer$labels,
      preProcess = "pca",
      method = "knn",
      tuneGrid = expand.grid(k = myK),
      trControl=fewerLOOPCATrCtrl
    )
    cat("DPI =",DPI, "sigma =",sigma,"PCA thresh = ", pcaThresh,
        "k =",knnCV$results[[1]],"Accuracy = ",100*knnCV$results[[2]],"%\n\r")
    gc()
    save(knnCV,file = knnCV_filename)
  }
}
unParallelizeMe(cl)
# Data processing, PCA, fewer LOO
# k, Accuracy, Kappa, AccuracySD, KappaSD
pcaCVResults = data.frame()
for(myK in big_k_list) {
  knnCV_filename = "../data/pcaknnFewerLOOCV-"
  knnCV_filename = paste(c(knnCV_filename,"thresh",pcaThresh,
                           "-dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
  load(file=knnCV_filename) #knnCV
  pcaCVResults = rbind(pcaCVResults,knnCV$results)
}
knnCV_filename = "../data/pcaknnFewerLOOCV-"
knnCV_filename = paste(c(knnCV_filename,
                         "-dpi",DPI,"-sig",sigma,"-rep",n_repeats,"-results.csv"),collapse="")
write.table(pcaCVResults,
            file=knnCV_filename,
            row.names=FALSE,
            col.names=c("k","Accuracy","Kappa","AccuracySD","KappaSD"),
            sep=',')
# ---------------------------------------------------------------
# ---------------------------------------------------------------
# ---------------------------------------------------------------
best_k_fewer=5




# ---------------------------------------------------------------
# ---------------------------------------------------------------
# ---------------------------------------------------------------
# ---------------------------------------------------------------
#Data generation, PCA, fewer LOO
#SVM: 2nd order polynomial. scale = 0.02. Based on previous experiments.
#svmPolyTuneGrid = expand.grid(degree = c(2), scale = c(0.02,0.1,0.2), C = c(0.25,0.75,1))
#svmPolyTuneGrid = expand.grid(degree = c(2), scale = c(0.1), C = c(0.25,0.75,1))

best_degree=2
best_scale=0.1
degree_list=c(best_degree)
scale_list=c(best_scale)
C_list=c(0.25,0.5,0.75,1)
#C_list=c(0.5)

gc()
cat("Time: ",format(Sys.time(), "%X"),"\n\rk-NN: Generate accuracy vs. k data for fewer LOO, PCA:\n\r")
cl = parallelizeMe()
for(currC in C_list) {
  cat("Time: ",format(Sys.time(), "%X"),"\n\r")
  cat("C:",currC,"\n\r")
  svmPolyCV_filename = "../data/pcasvmPolyFewerLOOCV"
  svmPolyCV_filename = paste(c(svmPolyCV_filename,"-thresh",pcaThresh,
                               "-dpi",DPI,"sig",sigma,"-C",currC,"-rep",n_repeats,".RData"),collapse="")
  if(!file.exists(svmPolyCV_filename)) {
    svmPolyCV = train(
      x = data_pcaFewer$data,
      y = data_pcaFewer$labels,
      preProcess = "pca",
      method = "svmPoly",
      tuneGrid = expand.grid(degree=degree_list,scale=scale_list,C=currC),#svmPolyTuneGrid, #degree, scale, C
      #trControl=fewerLOOPCATrCtrlONE
      trControl=fewerLOOPCATrCtrl
    )
    cat("DPI =",DPI, "sigma =",sigma,"PCA thresh = ", pcaThresh,
        "Accuracy = ",100*svmPolyCV$results[["Accuracy"]],"%\n\r")
    gc()
    svmPolyCVResults=svmPolyCV$results
    save(svmPolyCVResults,file = svmPolyCV_filename)
  }
}
unParallelizeMe(cl)

# Data processing, PCA, fewer LOO
collective_svmPolyCVResults=data.frame()
for(currC in C_list) {
  svmPolyCV_filename = "../data/pcasvmPolyFewerLOOCV"
  svmPolyCV_filename = paste(c(svmPolyCV_filename,"-thresh",pcaThresh,
                               "-dpi",DPI,"sig",sigma,"-C",currC,"-rep",n_repeats,".RData"),collapse="")
  load(file=svmPolyCV_filename) #svmPolyCVResults
  collective_svmPolyCVResults=rbind(collective_svmPolyCVResults,svmPolyCVResults)
}
svmPolyCV_filename = "../data/pcasvmPolyFewerLOOCV"
svmPolyCV_filename = paste(c(svmPolyCV_filename,"-thresh",pcaThresh,
                             "-dpi",DPI,"sig",sigma,"-rep",n_repeats,"-results.csv"),collapse="")
write.table(collective_svmPolyCVResults,
            file=svmPolyCV_filename,
            row.names=FALSE,
            #col.names=c("scale","Accuracy","Kappa","AccuracySD","KappaSD"),
            sep=',')


best_C_fewer=0.5


























# ---------------------------------------------------------------
# -----------------------All mixed-------------------------------
# ---------------------------------------------------------------


set.seed(4578)
allMixedFolds = createMultiFolds(data_pcaAll$labels,k=n_folds,times=n_repeats)

allMixedPCATrCtrl = trainControl(
  method = "repeatedcv",  #Repeated k-fold cross validation
  number = n_folds,       #Number of folds
  repeats = 1,    #Number of repetitions (complete sets of folds to cross validate)
  verboseIter = TRUE,     #Print training log
  returnData = FALSE,     #A logical for saving the data???
  seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
  #while a value of NULL will set the seeds using a random set of integers.
  savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
  preProcOptions = c(thresh=pcaThresh),
  index = allMixedFolds
)

# Data generation: All mixed PCA
cat("Time: ",format(Sys.time(), "%X"),"\n\rGenerate accuracy vs. k data for dataset \"all, mixed\" PCA:\n\r")
cl = parallelizeMe()
knnCV_filename="../data/pcaknnAllMixedCV-dpi"
knnCV_filename=paste(c(knnCV_filename,DPI,"-sig",sigma,"-k",best_k_fewer,"-rep",n_repeats,".RData"),collapse="")
if(!file.exists(knnCV_filename)) {
  knnCV = train(
    x = data_pcaAll$data,
    y = data_pcaAll$labels,
    method = "knn",
    preProcess = "pca",
    tuneGrid = expand.grid(k = best_k_fewer),
    trControl = allMixedPCATrCtrl
  )
  cat("k-NN, DPI =",DPI, "sigma =",sigma,"k =",knnCV$results[[1]],"Accuracy = ",100*knnCV$results[[2]],"%\n\r")
  gc()
  save(knnCV,file = knnCV_filename)
}
unParallelizeMe(cl)

cl = parallelizeMe()
svmCV_filename="../data/pcasvmPolyAllMixedCV-dpi"
svmCV_filename=paste(c(svmCV_filename,DPI,"-sig",sigma,"-rep",n_repeats,".RData"),collapse="")
if(!file.exists(svmCV_filename)) {
  svmCV = train(
    x = data_pcaAll$data,
    y = data_pcaAll$labels,
    method = "svmPoly",
    tuneGrid = expand.grid(degree=best_degree,scale=best_scale,C=best_C_fewer),#svmPolyTuneGrid, #degree, scale, C
    preProcess = "pca",
    #tuneGrid = expand.grid(k = best_k_fewer),
    trControl = allMixedPCATrCtrl
  )
  cat("SVM, DPI =",DPI, "sigma =",sigma,"Accuracy = ",100*svmCV$results[[2]],"%\n\r")
  save(svmCV,file = svmCV_filename)
}
unParallelizeMe(cl)
gc()


AllMixedCV_filename = paste(c(
  "../data/AllMixedCV",
  "-dpi",DPI,
  "-sigma",sigma,
  "-k",best_k_fewer,
  "-degree",best_degree,
  "-scale",best_scale,
  "-C",best_C_fewer,
  ".csv"),collapse="")
load(knnCV_filename)
load(svmCV_filename)
AllMixedCV=data.frame(
  knnAccuracy=knnCV$results[["Accuracy"]],
  knnAccuracySD=knnCV$results[["AccuracySD"]],
  svmAccuracy=svmCV$results[["Accuracy"]],
  svmAccuracySD=svmCV$results[["AccuracySD"]]
)
write.table(AllMixedCV,
            file=AllMixedCV_filename,
            row.names=FALSE,
            col.names=TRUE,
            sep=',')
  











# ---------------------------------------------------------------
# -----------------------All LOO---------------------------------
# ---------------------------------------------------------------
allLOOFolds = list()
np=length(persons_all_loadable)
allIdx = 1:(np*4000)
#Each list element is a vector of integers corresponding to the rows used for training at that iteration.
#For each person, make a fold.
for(idx in 0:(np-1)) {
  #Remove current person's zeroes from this fold.
  rem=(idx*400+1):(400+idx*400)
  #Remove current person's ones, twos, ... , nines from this fold.
  for(idx2 in 1:9) {
    rem=c(rem,    (np*idx2*400+1):(400+np*idx2*400)     )
  }
  #Append to fewerLOOFolds, the new fold which does not contain current person.
  allLOOFolds[[idx+1]]=allIdx[-(rem)]
}
cat("Index:\n\r")
str(allLOOFolds)

allLOOPCATrCtrl = trainControl(
  method = "repeatedcv",  #
  number = np,            #
  repeats = 1,    #Number of repetitions (complete sets of folds to cross validate)
  verboseIter = TRUE,     #Print training log
  returnData = FALSE,     #A logical for saving the data???
  seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
  #while a value of NULL will set the seeds using a random set of integers.
  savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
  preProcOptions = c(thresh=pcaThresh),
  index = allLOOFolds
)


# Data generation: All LOO PCA
cat("Time: ",format(Sys.time(), "%X"),"\n\rGenerate accuracy vs. k data for dataset \"all, LOO\" PCA:\n\r")
cl = parallelizeMe()
knnCV_filename="../data/pcaknnAllLOOCV-dpi"
knnCV_filename=paste(c(knnCV_filename,DPI,"-sig",sigma,"-k",best_k_fewer,"-rep",n_repeats,".RData"),collapse="")
if(!file.exists(knnCV_filename)) {
  knnCV = train(
    x = data_pcaAll$data,
    y = data_pcaAll$labels,
    method = "knn",
    preProcess = "pca",
    tuneGrid = expand.grid(k = best_k_fewer),
    trControl = allLOOPCATrCtrl
  )
  cat("k-NN, DPI =",DPI, "sigma =",sigma,"k =",knnCV$results[[1]],"Accuracy = ",100*knnCV$results[[2]],"%\n\r")
  gc()
  save(knnCV,file = knnCV_filename)
}
unParallelizeMe(cl)

cl = parallelizeMe()
svmCV_filename="../data/pcasvmAllLOOCV-dpi"
svmCV_filename=paste(c(svmCV_filename,DPI,"-sig",sigma,"-rep",n_repeats,".RData"),collapse="")
if(!file.exists(svmCV_filename)) {
  svmCV = train(
    x = data_pcaAll$data,
    y = data_pcaAll$labels,
    method = "svmPoly",
    tuneGrid = expand.grid(degree=best_degree,scale=best_scale,C=best_C_fewer),#svmPolyTuneGrid, #degree, scale, C
    preProcess = "pca",
    #tuneGrid = expand.grid(k = best_k_fewer),
    trControl = allLOOPCATrCtrl
  )
  cat("SVM, DPI =",DPI, "sigma =",sigma,"Accuracy = ",100*svmCV$results[[2]],"%\n\r")
  gc()
  save(svmCV,file = svmCV_filename)
}
unParallelizeMe(cl)


AllLOOCV_filename = paste(c(
  "../data/AllLOOCV",
  "-dpi",DPI,
  "-sigma",sigma,
  "-k",best_k_fewer,
  "-degree",best_degree,
  "-scale",best_scale,
  "-C",best_C_fewer,
  ".csv"),collapse="")
load(knnCV_filename)
load(svmCV_filename)
AllLOOCV=data.frame(
  knnAccuracy=knnCV$results[["Accuracy"]],
  knnAccuracySD=knnCV$results[["AccuracySD"]],
  svmAccuracy=svmCV$results[["Accuracy"]],
  svmAccuracySD=svmCV$results[["AccuracySD"]]
)
write.table(AllLOOCV,
            file=AllLOOCV_filename,
            row.names=FALSE,
            col.names=TRUE,
            sep=',')













# ---------------------------------------------------------------
# -----------------------Single person---------------------------
# ---------------------------------------------------------------
set.seed(4578)
pdPCAFolds = createMultiFolds(data_pcaPD$labels,k=n_folds,times=n_repeats)
pdPCATrCtrl = trainControl(
  method = "repeatedcv",  #Repeated k-fold cross validation
  number = n_folds,       #Number of folds
  repeats = n_repeats,    #Number of repetitions (complete sets of folds to cross validate)
  verboseIter = TRUE,     #Print training log
  returnData = FALSE,     #A logical for saving the data???
  seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
  savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
  index = pdPCAFolds
)
cl = parallelizeMe()
for(testPerson in persons_all_loadable) {
  cat("Person: G",testPerson[1],"M",testPerson[2],"\n\r")
  dataset_filename = paste(c(
    "../data/data-",testPerson[1],"-",testPerson[2],"-",DPI,"-",sigma,".RData"
  ),collapse="")
  knnCV_filename="../data/pcaknnPDCV-dpi"
  knnCV_filename=paste(c(knnCV_filename,DPI,"-sig",sigma,
                         "-G",testPerson[1],"M",testPerson[2],
                         "-folds",pdPCATrCtrl$number,
                         "-rep",pdPCATrCtrl$repeats,".RData"),collapse="")
  svmPolyCV_filename="../data/pcasvmPDCV-dpi"
  svmPolyCV_filename=paste(c(svmPolyCV_filename,DPI,"-sig",sigma,
                             "-G",testPerson[1],"M",testPerson[2],
                             "-folds",pdPCATrCtrl$number,
                             "-rep",pdPCATrCtrl$repeats,".RData"),collapse="")
  if((!file.exists(knnCV_filename))||(!file.exists(svmPolyCV_filename))) {
    #data_pd=loadLabeledDataset(dataset_filename)
    data_and_labels =
      listOfDigits2classificationDataAlt(
        dataList =
          loadMultiplePersonsDataByDigitMerged(
            DPI = DPI,
            persons = list(testPerson),
            sigma = sigma,
            reload = FALSE
          )
      )
    
    if(!file.exists(knnCV_filename)) {
      knnCV=train(
        x = data_and_labels$data,
        y = data_and_labels$dataClassF,
        method = "knn",
        preProcess = "pca",
        tuneGrid = expand.grid(k = best_k_fewer),
        trControl = pdPCATrCtrl
      )
      save(knnCV,file=knnCV_filename)
    }
    if(!file.exists(svmPolyCV_filename)) {
      svmPolyCV=train(
        x = data_and_labels$data,
        y = data_and_labels$dataClassF,
        method = "svmPoly",
        preProcess = "pca",
        tuneGrid = expand.grid(degree=best_degree,scale=best_scale,C=best_C_fewer),
        trControl = pdPCATrCtrl
      )
      save(svmPolyCV,file=svmPolyCV_filename)
    }
  }
}
unParallelizeMe(cl)

singlePersonCV=matrix(nrow=0,ncol=4)#data.frame("knnAccuracy","knnAccuracySD","svmAccuracy","svmAccuracySD")#col.names=c("knnAccuracy","knnAccuracySD","svmAccuracy","svmAccuracySD"))
personIterator=0
for(testPerson in persons_all_loadable) {
  personIterator=personIterator+1
  dataset_filename = paste(c(
    "../data/data-",testPerson[1],"-",testPerson[2],"-",DPI,"-",sigma,".RData"
  ),collapse="")
  knnCV_filename="../data/pcaknnPDCV-dpi"
  knnCV_filename=paste(c(knnCV_filename,DPI,"-sig",sigma,
                         "-G",testPerson[1],"M",testPerson[2],
                         "-folds",pdPCATrCtrl$number,
                         "-rep",pdPCATrCtrl$repeats,".RData"),collapse="")
  svmPolyCV_filename="../data/pcasvmPDCV-dpi"
  svmPolyCV_filename=paste(c(svmPolyCV_filename,DPI,"-sig",sigma,
                             "-G",testPerson[1],"M",testPerson[2],
                             "-folds",pdPCATrCtrl$number,
                             "-rep",pdPCATrCtrl$repeats,".RData"),collapse="")
  load(knnCV_filename) #knnCV
  load(svmPolyCV_filename) #svmPolyCV
  singlePersonCV=rbind(singlePersonCV,c(
    knnAccuracy=knnCV$results[["Accuracy"]],
    knnAccuracySD=knnCV$results[["AccuracySD"]],
    svmAccuracy=svmPolyCV$results[["Accuracy"]],
    svmAccuracySD=svmPolyCV$results[["AccuracySD"]]
  ))
}

singlePersonCV_filename = paste(c(
  "../data/singlePersonCV",
  "-dpi",DPI,
  "-sigma",sigma,
  "-k",best_k_fewer,
  "-degree",best_degree,
  "-scale",best_scale,
  "-C",best_C_fewer,
  ".csv"),collapse="")
write.table(singlePersonCV,
            file=singlePersonCV_filename,
            row.names=FALSE,
            col.names=TRUE,
            sep=',')


singlePersonCVMean=colMeans(singlePersonCV)

singlePersonAllCV=list()
singlePersonAllCV[["knnAccuracy"]]=mean(singlePersonCV[,1])
singlePersonAllCV[["svmAccuracy"]]=mean(singlePersonCV[,3])

knnVarInternal=singlePersonCV[,2]^2
knnVarExternal=var(singlePersonCV[,1])
knnVar=knnVarExternal+mean(knnVarInternal)
svmVarInternal=singlePersonCV[,4]^2
svmVarExternal=var(singlePersonCV[,3])
svmVar=svmVarExternal+mean(svmVarInternal)

singlePersonAllCV[["knnAccuracySD"]]=sqrt(knnVar)
singlePersonAllCV[["svmAccuracySD"]]=sqrt(svmVar)

singlePersonAllCV_filename = paste(c(
  "../data/singlePersonAllCV",
  "-dpi",DPI,
  "-sigma",sigma,
  "-k",best_k_fewer,
  "-degree",best_degree,
  "-scale",best_scale,
  "-C",best_C_fewer,
  ".csv"),collapse="")
write.table(singlePersonAllCV,
            file=singlePersonAllCV_filename,
            row.names=FALSE,
            col.names=TRUE,
            sep=',')


#




# ---------------------------------------------------------------
# -----------------------Prediction time-------------------------
# ---------------------------------------------------------------
#timer = proc.time()
# ...
#total_time=(proc.time() - timer)[[1]]
DPI=100
sigma=1
best_k_fewer=5
n_repeats=1
knnCV_filename="../data/pcaknnAllLOOCV-dpi"
knnCV_filename=paste(c(knnCV_filename,DPI,"-sig",sigma,"-k",best_k_fewer,"-rep",n_repeats,".RData"),collapse="")
svmCV_filename="../data/pcasvmAllLOOCV-dpi"
svmCV_filename=paste(c(svmCV_filename,DPI,"-sig",sigma,"-rep",n_repeats,".RData"),collapse="")
load(knnCV_filename) #knnCV
load(svmCV_filename) #svmCV

library("kernlab")
knn_times=matrix(nrow=0,ncol=1)
svm_times=matrix(nrow=0,ncol=1)
time_runs = 30
for(i in 1:time_runs) {
  cat("Run",i,"of",time_runs)
  timer = proc.time()
  predict(knnCV,data_pcaPD$data)
  knn_time = (proc.time() - timer)[[1]]
  cat(".")
  timer = proc.time()
  predict(svmCV,data_pcaPD$data)
  svm_time = (proc.time() - timer)[[1]]
  cat(".\n\r")
  knn_times=rbind(knn_times,knn_time)
  svm_times=rbind(svm_times,svm_time)
}
knn_time_mean=mean(knn_times)
knn_time_sdev=sd(knn_times)
svm_time_mean=mean(svm_times)
svm_time_sdev=sd(svm_times)
write.table(data.frame(knn_time_mean,knn_time_sdev,svm_time_mean,svm_time_sdev),
      file = paste(c("../data/prediction_times_",time_runs,"_AllLOO_on_one_person.csv"),collapse=""),
      row.names=FALSE,
      col.names=TRUE,
      sep=','
      )













































































preloadEverything(FALSE)



# Multiple people kNN accuracy vs. k test

#Create fold indices for CV.
#Set seed to 4578 to always create the same folds,
#so that the obtained results are reproducible.
dataset_filename = getLabeledDatasetFilename(
  persons_fewer,
  DPI=100,sigma=loading_setups$fewer[[1]]$sigma_list[1])
data_fewer_labels = loadLabeledDataset(dataset_filename)$labels
set.seed(4578)
fewerMixedFolds = createMultiFolds(data_fewer_labels,k=n_folds,times=n_repeats)

fewerMixedTrCtrl = trainControl(
  method = "repeatedcv",  #Repeated k-fold cross validation
  number = n_folds,       #Number of folds
  repeats = n_repeats,    #Number of repetitions (complete sets of folds to cross validate)
  verboseIter = TRUE,     #Print training log
  returnData = FALSE,     #A logical for saving the data???
  seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
  #while a value of NULL will set the seeds using a random set of integers.
  savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
  index = fewerMixedFolds
)

# Data generation loop 1: fewer, mixed
cat("Time: ",format(Sys.time(), "%X"),"\n\rGenerate accuracy vs. k data for dataset \"fewer, mixed\":\n\r")
cl = parallelizeMe()
for(loading_setup in loading_setups$fewer) {
  DPI = loading_setup$DPI
  if(DPI==100) {
    sigma_list = loading_setup$sigma_list
    cat("Time: ",format(Sys.time(), "%X"),"\n\rDPI: ",DPI,", sigmas: ", sigma_list, "\n\r")
    #Calculate k-NN accuracy for all k-sigma combinations
    for(sigma in sigma_list) {
      cat("Time: ",format(Sys.time(), "%X"),"\n\r sigma: ", sigma, "\n\r")
      dataset_filename = getLabeledDatasetFilename(persons_fewer,DPI=DPI,sigma=sigma)
      data_fewer = loadLabeledDataset(dataset_filename)
      for(myK in k_list) {
        knnCV_filename = "../data/knnFewerMixedCV-"
        knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
        if(!file.exists(knnCV_filename)) {
          knnCV = train(
            x = data_fewer$data,
            y = data_fewer$labels,
            preProcess = NULL,
            method = "knn",
            tuneGrid = expand.grid(k = myK),
            trControl=fewerMixedTrCtrl
          )
          cat("DPI =",DPI, "sigma =",sigma,"k =",knnCV$results[[1]],"Accuracy = ",100*knnCV$results[[2]],"%\n\r")
          gc()
          save(knnCV,file = knnCV_filename)
        }
      }
    }
  }
}
unParallelizeMe(cl)
# Data processing loop 1: fewer, mixed
# Make table of sigma, k, Accuracy, Kappa, AccuracySD, KappaSD
knnCVResultsBest=data.frame()
for(loading_setup in loading_setups$fewer) {
  DPI = loading_setup$DPI
  if(DPI==100) {
    sigma_list = loading_setup$sigma_list
    cat("Writing",DPI,"DPI results for \"fewer, mixed\" to a csv file.")
    knnCVResults=data.frame()
    best_sigma = sigma_list[1]
    best_k = k_list[1]
    best_acc=0.0
    for(sigma in sigma_list) {
      for(myK in k_list) {
        knnCV_filename = "../data/knnFewerMixedCV-"
        knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
        load(file = knnCV_filename)
        knnCVResults=rbind(knnCVResults,c("sigma"=sigma,knnCV$results))
        if(knnCV$results$Accuracy[1] > best_acc) {
          best_acc = knnCV$results$Accuracy[1]
          best_sigma = sigma
          best_k = myK
        }
      }
    }
    knnCVResultsBest=rbind(knnCVResultsBest,c("DPI" = DPI, "sigma" = best_sigma, "k" = best_k))
    knnCV_filename = "../data/knnFewerMixedCV-"
    knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-rep",n_repeats,"-results.csv"),collapse="")
    write.table(knnCVResults,
                file=knnCV_filename,
                row.names=FALSE,
                col.names=c("sigma","k","Accuracy","Kappa","AccuracySD","KappaSD"),
                sep=',')
    cat("Done\n\r")
  }
}
save(x=knnCVResultsBest,file=paste(c(
  "../data/knnFewerMixedCV",
  "-rep",n_repeats,"-best_sigma_k.RData"),collapse=""))
# Data processing loop 2: fewer, mixed
# Make confusion matrices for optimal sigma-k combinations
# from already carried out cross validation results.
load(file=paste(c("../data/knnFewerMixedCV",
                  "-rep",n_repeats,"-best_sigma_k.RData"),collapse=""))
for(i in 1:nrow(knnCVResultsBest)) {
  DPI=knnCVResultsBest[[1]][i]
  sigma=knnCVResultsBest[[2]][i]
  myK=knnCVResultsBest[[3]][i]
  knnCV_filename = "../data/knnFewerMixedCV-"
  knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
  load(file = knnCV_filename)
  conMat = confusionMatrix(knnCV,norm="none")
  filenameConMat=paste(c("../data/knnFewerMixedCV",
                         "-dpi",DPI,
                         "-sig",sigma,
                         "-k",myK,
                         "-confusionMatrix.csv"), collapse = "")
  #Rows: Prediction. Columns: Reference. So conMat[10,4] is number of 3's incorrectly predicted as 9's.
  write.csv(x=(n_folds*n_repeats)*conMat$table,file=filenameConMat)
}







#Create fold indices for CV.
# We only have one set of 10 folds, so just 1 run.
fewerLOOFolds = list()#createMultiFolds(data_fewer_labels,k=n_folds,times=n_repeats)
np=length(persons_fewer) #10 persons
allIdx = 1:(np*4000)
#Each list element is a vector of integers corresponding to the rows used for training at that iteration.
#For each person, make a fold.
for(idx in 0:(np-1)) {
  #Remove current person's zeroes from this fold.
  rem=(idx*400+1):(400+idx*400)
  #Remove current person's ones, twos, ... , nines from this fold.
  for(idx2 in 1:9) {
    rem=c(rem,    (np*idx2*400+1):(400+np*idx2*400)     )
  }
  #Append to fewerLOOFolds, the new fold which does not contain current person.
  fewerLOOFolds[[idx+1]]=allIdx[-(rem)]
}
cat("Index:\n\r")
str(fewerLOOFolds)

fewerLOOTrCtrl = trainControl(
  method = "repeatedcv",  #Repeated k-fold cross validation
  number = n_folds,       #Number of folds
  repeats = 1,    #Number of repetitions (complete sets of folds to cross validate)
  verboseIter = TRUE,     #Print training log
  returnData = FALSE,     #A logical for saving the data???
  seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
  #while a value of NULL will set the seeds using a random set of integers.
  savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
  index = fewerLOOFolds
)

# Data generation loop: fewer, LOO
cat("Time: ",format(Sys.time(), "%X"),"\n\rGenerate accuracy vs. k data for dataset \"fewer, LOO\":\n\r")
cl = parallelizeMe()
for(loading_setup in loading_setups$fewer) {
  DPI = loading_setup$DPI
  if(DPI==100) {
    sigma_list = loading_setup$sigma_list
    cat("Time: ",format(Sys.time(), "%X"),"\n\rDPI: ",DPI,", sigmas: ", sigma_list, "\n\r")
    #Calculate k-NN accuracy for all k-sigma combinations
    for(sigma in sigma_list) {
      cat("Time: ",format(Sys.time(), "%X"),"\n\r sigma: ", sigma, "\n\r")
      dataset_filename = getLabeledDatasetFilename(persons_fewer,DPI=DPI,sigma=sigma)
      data_fewer = loadLabeledDataset(dataset_filename)
      for(myK in k_list) {
        knnCV_filename = "../data/knnFewerLOOCV-"
        knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep1.RData"),collapse="")
        if(!file.exists(knnCV_filename)) {
          knnCV = train(
            x = data_fewer$data,
            y = data_fewer$labels,
            preProcess = NULL,
            method = "knn",
            tuneGrid = expand.grid(k = myK),
            trControl=fewerLOOTrCtrl
          )
          cat("DPI =",DPI, "sigma =",sigma,"k =",knnCV$results[[1]],"Accuracy = ",100*knnCV$results[[2]],"%\n\r")
          gc()
          save(knnCV,file = knnCV_filename)
        }
      }
    }
  }
}
unParallelizeMe(cl)
# Data processing loop: fewer, LOO
# Make table of sigma, k, Accuracy, Kappa, AccuracySD, KappaSD
knnCVResultsBest=data.frame()
for(loading_setup in loading_setups$fewer) {
  DPI = loading_setup$DPI
  if(DPI==100) {
    sigma_list = loading_setup$sigma_list
    cat("Writing",DPI,"DPI results for \"fewer, LOO\" to a csv file.")
    knnCVResults=data.frame()
    best_sigma = sigma_list[1]
    best_k = k_list[1]
    best_acc=0.0
    for(sigma in sigma_list) {
      for(myK in k_list) {
        knnCV_filename = "../data/knnFewerLOOCV-"
        knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep1.RData"),collapse="")
        load(file = knnCV_filename)
        knnCVResults=rbind(knnCVResults,c("sigma"=sigma,knnCV$results))
        if(knnCV$results$Accuracy[1] > best_acc) {
          best_acc = knnCV$results$Accuracy[1]
          best_sigma = sigma
          best_k = myK
        }
      }
    }
    knnCVResultsBest=rbind(knnCVResultsBest,c("DPI" = DPI, "sigma" = best_sigma, "k" = best_k))
    knnCV_filename = "../data/knnFewerLOOCV-"
    knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-rep1-results.csv"),collapse="")
    write.table(knnCVResults,
                file=knnCV_filename,
                row.names=FALSE,
                col.names=c("sigma","k","Accuracy","Kappa","AccuracySD","KappaSD"),
                sep=',')
    cat("Done\n\r")
  }
}
save(x=knnCVResultsBest,file=paste(c(
  "../data/knnFewerLOOCV",
  "-rep1-best_sigma_k.RData"),collapse=""))
# Data processing loop 2: fewer, LOO
# Make confusion matrices for optimal sigma-k combinations
# from already carried out cross validation results.
load(file=paste(c("../data/knnFewerLOOCV",
                  "-rep1-best_sigma_k.RData"),collapse=""))
for(i in 1:nrow(knnCVResultsBest)) {
  DPI=knnCVResultsBest[[1]][i]
  sigma=knnCVResultsBest[[2]][i]
  myK=knnCVResultsBest[[3]][i]
  knnCV_filename = "../data/knnFewerLOOCV-"
  knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep1.RData"),collapse="")
  load(file = knnCV_filename)
  conMat = confusionMatrix(knnCV,norm="none")
  filenameConMat=paste(c("../data/knnFewerLOOCV",
                         "-dpi",DPI,
                         "-sig",sigma,
                         "-k",myK,
                         "-confusionMatrix.csv"), collapse = "")
  #Rows: Prediction. Columns: Reference. So conMat[10,4] is number of 3's incorrectly predicted as 9's.
  write.csv(x=(n_folds*n_repeats)*conMat$table,file=filenameConMat)
}

































# --------------------------------------------

# --------------------------------------------
# --------------------------------------------
# --------------------------------------------
# --------------------------------------------

#------------It is not recommended to run "all, mixed" data
#------------data generation loop, because it is 10 complete repeats
#------------of 10-fold cross validation = 100 sets of training/test data.
#------------Instead, see "all, LOO" below.

dataset_filename = getLabeledDatasetFilename(
  persons_all_loadable,
  DPI=100,sigma=loading_setups$all_loadable[[1]]$sigma_list[1])
data_all_labels = loadLabeledDataset(dataset_filename)$labels
set.seed(4578)
allMixedFolds = createMultiFolds(data_all_labels,k=n_folds,times=n_repeats)

allMixedTrCtrl = trainControl(
  method = "repeatedcv",  #Repeated k-fold cross validation
  number = n_folds,       #Number of folds
  repeats = n_repeats,    #Number of repetitions (complete sets of folds to cross validate)
  verboseIter = TRUE,     #Print training log
  returnData = FALSE,     #A logical for saving the data???
  seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
  #while a value of NULL will set the seeds using a random set of integers.
  savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
  index = allMixedFolds
)

# Data generation loop 2: all, mixed
cat("Time: ",format(Sys.time(), "%X"),"\n\rGenerate accuracy vs. k data for dataset \"all, mixed\":\n\r")
cl = parallelizeMe()
for(loading_setup in loading_setups$all_loadable) {
  DPI = loading_setup$DPI
  if(DPI==100) {
    sigma_list = loading_setup$sigma_list
    cat("Time: ",format(Sys.time(), "%X"),"\n\rDPI: ",DPI,", sigmas: ", sigma_list, "\n\r")
    #Calculate k-NN accuracy for all k-sigma combinations
    for(sigma in sigma_list) {
      cat("Time: ",format(Sys.time(), "%X"),"\n\r sigma: ", sigma, "\n\r")
      dataset_filename = getLabeledDatasetFilename(persons_all_loadable,DPI=DPI,sigma=sigma)
      data_all = loadLabeledDataset(dataset_filename)
      for(myK in k_list) {
        knnCV_filename = "../data/knnAllMixedCV-"
        knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
        if(!file.exists(knnCV_filename)) {
          knnCV = train(
            x = data_all$data,
            y = data_all$labels,
            preProcess = NULL,
            method = "knn",
            tuneGrid = expand.grid(k = myK),
            trControl=allMixedTrCtrl
          )
          cat("DPI =",DPI, "sigma =",sigma,"k =",knnCV$results[[1]],"Accuracy = ",100*knnCV$results[[2]],"%\n\r")
          gc()
          save(knnCV,file = knnCV_filename)
        }
      }
    }
  }
}
unParallelizeMe(cl)
# Data processing loop 1: all, mixed
# Make table of sigma, k, Accuracy, Kappa, AccuracySD, KappaSD
knnCVResultsBest=data.frame()
for(loading_setup in loading_setups$all_loadable) {
  DPI = loading_setup$DPI
  if(DPI==100) {
    sigma_list = loading_setup$sigma_list
    cat("Writing",DPI,"DPI results for \"all, mixed\" to a csv file.")
    knnCVResults=data.frame()
    best_sigma = sigma_list[1]
    best_k = k_list[1]
    best_acc=0.0
    for(sigma in sigma_list) {
      for(myK in k_list) {
        knnCV_filename = "../data/knnAllMixedCV-"
        knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
        load(file = knnCV_filename)
        knnCVResults=rbind(knnCVResults,c("sigma"=sigma,knnCV$results))
        if(knnCV$results$Accuracy[1] > best_acc) {
          best_acc = knnCV$results$Accuracy[1]
          best_sigma = sigma
          best_k = myK
        }
      }
    }
    knnCVResultsBest=rbind(knnCVResultsBest,c("DPI" = DPI, "sigma" = best_sigma, "k" = best_k))
    knnCV_filename = "../data/knnAllMixedCV-"
    knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-rep",n_repeats,"-results.csv"),collapse="")
    write.table(knnCVResults,
                file=knnCV_filename,
                row.names=FALSE,
                col.names=c("sigma","k","Accuracy","Kappa","AccuracySD","KappaSD"),
                sep=',')
    cat("Done\n\r")
  }
}
save(x=knnCVResultsBest,file=paste(c(
  "../data/knnAllMixedCV",
  "-rep",n_repeats,"-best_sigma_k.RData"),collapse=""))
# Data processing loop 2: all, mixed
# Make confusion matrices for optimal sigma-k combinations
# from already carried out cross validation results.
load(file=paste(c("../data/knnAllMixedCV",
                  "-rep",n_repeats,"-best_sigma_k.RData"),collapse=""))
for(i in 1:nrow(knnCVResultsBest)) {
  DPI=knnCVResultsBest[[1]][i]
  sigma=knnCVResultsBest[[2]][i]
  myK=knnCVResultsBest[[3]][i]
  knnCV_filename = "../data/knnAllMixedCV-"
  knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
  load(file = knnCV_filename)
  conMat = confusionMatrix(knnCV,norm="none")
  filenameConMat=paste(c("../data/knnAllMixedCV",
                         "-dpi",DPI,
                         "-sig",sigma,
                         "-k",myK,
                         "-confusionMatrix.csv"), collapse = "")
  #Rows: Prediction. Columns: Reference. So conMat[10,4] is number of 3's incorrectly predicted as 9's.
  write.csv(x=(n_folds*n_repeats)*conMat$table,file=filenameConMat)
}














# --------------------------------------------
# --------------------------------------------
# --------------------------------------------
# --------------------------------------------
# --------------------------------------------










#Create fold indices for CV.
allLOOFolds = list()
np=length(persons_all_loadable)
allIdx = 1:(np*4000)
#Each list element is a vector of integers corresponding to the rows used for training at that iteration.
#For each person, make a fold.
for(idx in 0:(np-1)) {
  #Remove current person's zeroes from this fold.
  rem=(idx*400+1):(400+idx*400)
  #Remove current person's ones, twos, ... , nines from this fold.
  for(idx2 in 1:9) {
    rem=c(rem,    (np*idx2*400+1):(400+np*idx2*400)     )
  }
  #Append to fewerLOOFolds, the new fold which does not contain current person.
  allLOOFolds[[idx+1]]=allIdx[-(rem)]
}
cat("Index:\n\r")
str(allLOOFolds)

allLOOTrCtrl = trainControl(
  method = "repeatedcv",  #
  number = np,            #
  repeats = 1,    #Number of repetitions (complete sets of folds to cross validate)
  verboseIter = TRUE,     #Print training log
  returnData = FALSE,     #A logical for saving the data???
  seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
  #while a value of NULL will set the seeds using a random set of integers.
  savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
  index = allLOOFolds
)

# Data generation loop: all, LOO
cat("Time: ",format(Sys.time(), "%X"),"\n\rGenerate accuracy vs. k data for dataset \"all, LOO\":\n\r")
cl = parallelizeMe()
for(loading_setup in loading_setups$all_loadable) {
  DPI = loading_setup$DPI
  if(DPI==100) {
    sigma_list = loading_setup$sigma_list
    cat("Time: ",format(Sys.time(), "%X"),"\n\rDPI: ",DPI,", sigmas: ", sigma_list, "\n\r")
    #Calculate k-NN accuracy for all k-sigma combinations
    for(sigma in sigma_list) {
      cat("Time: ",format(Sys.time(), "%X"),"\n\r sigma: ", sigma, "\n\r")
      dataset_filename = getLabeledDatasetFilename(persons_all_loadable,DPI=DPI,sigma=sigma)
      data_fewer = loadLabeledDataset(dataset_filename)
      for(myK in k_list) {
        knnCV_filename = "../data/knnAllLOOCV-"
        knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep1.RData"),collapse="")
        if(!file.exists(knnCV_filename)) {
          knnCV = train(
            x = data_fewer$data,
            y = data_fewer$labels,
            preProcess = NULL,
            method = "knn",
            tuneGrid = expand.grid(k = myK),
            trControl=allLOOTrCtrl
          )
          cat("DPI =",DPI, "sigma =",sigma,"k =",knnCV$results[[1]],"Accuracy = ",100*knnCV$results[[2]],"%\n\r")
          gc()
          save(knnCV,file = knnCV_filename)
        }
      }
    }
  }
}
unParallelizeMe(cl)
# Data processing loop: all, LOO
# Make table of sigma, k, Accuracy, Kappa, AccuracySD, KappaSD
knnCVResultsBest=data.frame()
for(loading_setup in loading_setups$all_loadable) {
  DPI = loading_setup$DPI
  if(DPI==100) {
    sigma_list = loading_setup$sigma_list
    cat("Writing",DPI,"DPI results for \"all, LOO\" to a csv file.")
    knnCVResults=data.frame()
    best_sigma = sigma_list[1]
    best_k = k_list[1]
    best_acc=0.0
    for(sigma in sigma_list) {
      for(myK in k_list) {
        knnCV_filename = "../data/knnAllLOOCV-"
        knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep1.RData"),collapse="")
        load(file = knnCV_filename)
        knnCVResults=rbind(knnCVResults,c("sigma"=sigma,knnCV$results))
        if(knnCV$results$Accuracy[1] > best_acc) {
          best_acc = knnCV$results$Accuracy[1]
          best_sigma = sigma
          best_k = myK
        }
      }
    }
    knnCVResultsBest=rbind(knnCVResultsBest,c("DPI" = DPI, "sigma" = best_sigma, "k" = best_k))
    knnCV_filename = "../data/knnAllLOOCV-"
    knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-rep1-results.csv"),collapse="")
    write.table(knnCVResults,
                file=knnCV_filename,
                row.names=FALSE,
                col.names=c("sigma","k","Accuracy","Kappa","AccuracySD","KappaSD"),
                sep=',')
    cat("Done\n\r")
  }
}
save(x=knnCVResultsBest,file=paste(c(
  "../data/knnAllLOOCV",
  "-rep1-best_sigma_k.RData"),collapse=""))
# Data processing loop 2: all, LOO
# Make confusion matrices for optimal sigma-k combinations
# from already carried out cross validation results.
load(file=paste(c("../data/knnAllLOOCV",
                  "-rep1-best_sigma_k.RData"),collapse=""))
for(i in 1:nrow(knnCVResultsBest)) {
  DPI=knnCVResultsBest[[1]][i]
  sigma=knnCVResultsBest[[2]][i]
  myK=knnCVResultsBest[[3]][i]
  knnCV_filename = "../data/knnAllLOOCV-"
  knnCV_filename = paste(c(knnCV_filename,"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep1.RData"),collapse="")
  load(file = knnCV_filename)
  conMat = confusionMatrix(knnCV,norm="none")
  filenameConMat=paste(c("../data/knnAllLOOCV",
                         "-dpi",DPI,
                         "-sig",sigma,
                         "-k",myK,
                         "-confusionMatrix.csv"), collapse = "")
  #Rows: Prediction. Columns: Reference. So conMat[10,4] is number of 3's incorrectly predicted as 9's.
  write.csv(x=(n_folds*n_repeats)*conMat$table,file=filenameConMat)
}




























# --------------------------------------------
# --------------------------------------------
# --------------------------------------------
# PCA
DPI=100
sigma=1
# --------------------------------------------
# --------------------------------------------
dataset_filename = getLabeledDatasetFilename(persons_all_loadable,DPI=DPI,sigma=sigma)
data_pcaAll = loadLabeledDataset(dataset_filename)
normalized_data = normalizeZscore(data_pcaAll$data)
testPCAAll=smlPCA(normalized_data)

dataset_filename = getLabeledDatasetFilename(persons_persondependent,DPI=DPI,sigma=sigma)
data_pcaPD = loadLabeledDataset(dataset_filename)
normalized_data = normalizeZscore(data_pcaPD$data)
testPCAPD=smlPCA(normalized_data)

dataset_filename = getLabeledDatasetFilename(persons_fewer,DPI=DPI,sigma=sigma)
data_pcaFewer = loadLabeledDataset(dataset_filename)
normalized_data = normalizeZscore(data_pcaFewer$data)
testPCAFewer=smlPCA(normalized_data)
#2.1.2: Show the eigenvalues, variance and the accumulated variance of the principal components.
write.csv(testPCAPD$sdev,
          file=paste(c("../data/pca-G",
                       testPerson[1],"M",testPerson[2],
                       "-sdev.csv"),collapse=""))
write.csv(testPCAAll$sdev,
          file=paste(c("../data/pca-All-sdev.csv"),collapse=""))
write.csv(testPCAFewer$sdev,
          file=paste(c("../data/pca-Fewer-sdev.csv"),collapse=""))

for(i in 1:10) {
  displayDigit(data_pcaPD$data[77*i,],paste(c("dig-",77*i,"-",DPI,".png"),collapse=""))
  displayDigit((abs(testPCAPD$rotation[,i])-min(abs(testPCAPD$rotation[,i])))/(max(abs(testPCAPD$rotation[,i]))-min(abs(testPCAPD$rotation[,i]))),paste(c("pc",i,"-",DPI,".png"),collapse=""))
}

# --------------------------------------------
#2.1.3: Show the performance of selecting enough principal components to
# represent 80%, 90%, 95%, 99% of the accumulated variance. For each test
# vary K.
pdPCAThresholds = c(0.8,0.9,0.95,0.99) #PCA Cumulative variance cutoff threshold
pdPCAComp = NULL #maximum number of components. Overrides pdPCAThresh
#when calling train, specify preProcess = "pca" and trControl = pdPCATrCtrl
pdPCATrCtrl = list()
for(i in 1:length(pdPCAThresholds)) {
  pdPCATrCtrl[[i]] = trainControl(
    method = "repeatedcv",  #Repeated k-fold cross validation
    number = n_folds,       #Number of folds
    repeats = n_repeats,    #Number of repetitions (complete sets of folds to cross validate)
    verboseIter = TRUE,     #Print training log
    returnData = FALSE,     #A logical for saving the data???
    seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
    #while a value of NULL will set the seeds using a random set of integers.
    #savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
    preProcOptions=c(thresh=pdPCAThresholds[i],pcaComp=pdPCAComp),
    index = pdFolds #pdFolds already exists from no-preprocessing experiment
  )
}

allLOOPCATrCtrl = list()
for(i in 1:length(pdPCAThresholds)) {
  allLOOPCATrCtrl[[i]] = trainControl(
    method = "repeatedcv",  #
    number = length(persons_all_loadable),            #
    repeats = 1,    #Number of repetitions (complete sets of folds to cross validate)
    verboseIter = TRUE,     #Print training log
    returnData = FALSE,     #A logical for saving the data???
    seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
    #while a value of NULL will set the seeds using a random set of integers.
    savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
    preProcOptions=c(thresh=pdPCAThresholds[i],pcaComp=pdPCAComp),
    index = allLOOFolds
  )
}

allMixedPCATrCtrl = list()
for(i in 1:length(pdPCAThresholds)) {
  allMixedPCATrCtrl[[i]] = trainControl(
    method = "repeatedcv",  #
    number = length(persons_all_loadable),            #
    repeats = n_repeats,    #Number of repetitions (complete sets of folds to cross validate)
    verboseIter = TRUE,     #Print training log
    returnData = FALSE,     #A logical for saving the data???
    seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
    #while a value of NULL will set the seeds using a random set of integers.
    savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
    preProcOptions=c(thresh=pdPCAThresholds[i],pcaComp=pdPCAComp),
    index = allMixedFolds
  )
}

fewerMixedPCATrCtrl = list()
for(i in 1:length(pdPCAThresholds)) {
  fewerMixedPCATrCtrl[[i]] = trainControl(
    method = "repeatedcv",  #
    number = length(persons_fewer),            #
    repeats = n_repeats,    #Number of repetitions (complete sets of folds to cross validate)
    verboseIter = TRUE,     #Print training log
    returnData = FALSE,     #A logical for saving the data???
    seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
    #while a value of NULL will set the seeds using a random set of integers.
    savePredictions = "final", #"final" saves the predictions for the optimal tuning parameters.
    preProcOptions=c(thresh=pdPCAThresholds[i],pcaComp=pdPCAComp),
    index = fewerMixedFolds
  )
}

#Data generation, PCA, person dependent
cat("Time: ",format(Sys.time(), "%X"),"\n\rGenerate accuracy vs. k data for persondependent, PCA:\n\r")
cl = parallelizeMe()
for(i in 1:length(pdPCAThresholds)) {
  #data_pcaPD
  cat("Time: ",format(Sys.time(), "%X"),"\n\r PCA threshold:",pdPCAThresholds[i],"\n\r")
  for(myK in k_list) {
    knnCV_filename = "../data/pcaknnPersonDependentCV-"
    knnCV_filename = paste(c(knnCV_filename,"thresh",pdPCAThresholds[i],"-G",testPerson[1],"M",testPerson[2],
                             "dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
    if(!file.exists(knnCV_filename)) {
      knnCV = train(
        x = data_pcaPD$data,
        y = data_pcaPD$labels,
        preProcess = "pca",
        method = "knn",
        tuneGrid = expand.grid(k = myK),
        trControl=pdPCATrCtrl[[i]]
      )
      cat("DPI =",DPI, "sigma =",sigma,"PCA thresh = ", pdPCAThresholds[i],
          "k =",knnCV$results[[1]],"Accuracy = ",100*knnCV$results[[2]],"%\n\r")
      gc()
      save(knnCV,file = knnCV_filename)
    }
  }
}
unParallelizeMe(cl)
# Data processing, PCA, persondependent
# PCAthresh, k, Accuracy, Kappa, AccuracySD, KappaSD
pcaCVResults = data.frame()
for(i in 1:length(pdPCAThresholds)) {
  for(myK in k_list) {
    knnCV_filename = "../data/pcaknnPersonDependentCV-"
    knnCV_filename = paste(c(knnCV_filename,"G",testPerson[1],"M",testPerson[2],
                             "-dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
    load(file=knnCV_filename) #knnCV
    pcaCVResults = rbind(pcaCVResults,c("PCAthresh"=pdPCAThresholds[i],knnCV$results))
  }
}
knnCV_filename = "../data/pcaknnPersonDependentCV-"
knnCV_filename = paste(c(knnCV_filename,"thresh",pdPCAThresholds[i],"-G",testPerson[1],"M",testPerson[2],
                         "dpi",DPI,"-sig",sigma,"-rep",n_repeats,"-results.csv"),collapse="")
write.table(pcaCVResults,
            file=knnCV_filename,
            row.names=FALSE,
            col.names=c("PCAthresh","k","Accuracy","Kappa","AccuracySD","KappaSD"),
            sep=',')



#Data generation, PCA, all LOO
cat("Time: ",format(Sys.time(), "%X"),"\n\rGenerate accuracy vs. k data for ALL LOO, PCA:\n\r")
cl = parallelizeMe(5)
for(i in 1:length(pdPCAThresholds)) {
  #data_pcaAll
  cat("Time: ",format(Sys.time(), "%X"),"\n\r PCA threshold:",pdPCAThresholds[i],"\n\r")
  for(myK in k_list) {
    knnCV_filename = "../data/pcaknnAllLOOCV-"
    knnCV_filename = paste(c(knnCV_filename,"thresh",pdPCAThresholds[i],
                             "-dpi",DPI,"-sig",sigma,"-k-",myK,"-rep1.RData"),collapse="")
    if(!file.exists(knnCV_filename)) {
      knnCV = train(
        x = data_pcaAll$data,
        y = data_pcaAll$labels,
        preProcess = "pca",
        method = "knn",
        tuneGrid = expand.grid(k = myK),
        trControl=allLOOPCATrCtrl[[i]]
      )
      cat("DPI =",DPI, "sigma =",sigma,"PCA thresh = ", pdPCAThresholds[i],
          "k =",knnCV$results[[1]],"Accuracy = ",100*knnCV$results[[2]],"%\n\r")
      gc()
      save(knnCV,file = knnCV_filename)
    }
  }
}
unParallelizeMe(cl)
# Data processing, PCA, all LOO
# PCAthresh, k, Accuracy, Kappa, AccuracySD, KappaSD
pcaCVResults = data.frame()
for(i in 1:length(pdPCAThresholds)) {
  for(myK in k_list) {
    knnCV_filename = "../data/pcaknnAllLOOCV-"
    knnCV_filename = paste(c(knnCV_filename,
                             "dpi",DPI,"-sig",sigma,"-k-",myK,"-rep1.RData"),collapse="")
    load(file=knnCV_filename) #knnCV
    pcaCVResults = rbind(pcaCVResults,c("PCAthresh"=pdPCAThresholds[i],knnCV$results))
  }
}
knnCV_filename = "../data/pcaknnAllLOOCV-"
knnCV_filename = paste(c(knnCV_filename,
                         "dpi",DPI,"-sig",sigma,"-rep1-results.csv"),collapse="")
write.table(pcaCVResults,
            file=knnCV_filename,
            row.names=FALSE,
            col.names=c("PCAthresh","k","Accuracy","Kappa","AccuracySD","KappaSD"),
            sep=',')



#Data generation, PCA, fewer Mixed
cat("Time: ",format(Sys.time(), "%X"),"\n\rGenerate accuracy vs. k data for fewer mixed, PCA:\n\r")
cl = parallelizeMe()
for(i in 1:length(pdPCAThresholds)) {
  #data_pcaAll
  cat("Time: ",format(Sys.time(), "%X"),"\n\r PCA threshold:",pdPCAThresholds[i],"\n\r")
  for(myK in k_list) {
    knnCV_filename = "../data/pcaknnFewerMixedCV-"
    knnCV_filename = paste(c(knnCV_filename,"thresh",pdPCAThresholds[i],
                             "-dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
    if(!file.exists(knnCV_filename)) {
      knnCV = train(
        x = data_pcaFewer$data,
        y = data_pcaFewer$labels,
        preProcess = "pca",
        method = "knn",
        tuneGrid = expand.grid(k = myK),
        trControl=fewerMixedPCATrCtrl[[i]]
      )
      cat("DPI =",DPI, "sigma =",sigma,"PCA thresh = ", pdPCAThresholds[i],
          "k =",knnCV$results[[1]],"Accuracy = ",100*knnCV$results[[2]],"%\n\r")
      gc()
      save(knnCV,file = knnCV_filename)
    }
  }
}
unParallelizeMe(cl)
# Data processing, PCA, fewer mixed
# PCAthresh, k, Accuracy, Kappa, AccuracySD, KappaSD
pcaCVResults = data.frame()
for(i in 1:length(pdPCAThresholds)) {
  for(myK in k_list) {
    knnCV_filename = "../data/pcaknnFewerMixedCV-"
    knnCV_filename = paste(c(knnCV_filename,"thresh",pdPCAThresholds[i],
                             "dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
    load(file=knnCV_filename) #knnCV
    pcaCVResults = rbind(pcaCVResults,c("PCAthresh"=pdPCAThresholds[i],knnCV$results))
  }
}
knnCV_filename = "../data/pcaknnFewerMixedCV-"
knnCV_filename = paste(c(knnCV_filename,
                         "-dpi",DPI,"-sig",sigma,"-rep",n_repeats,"-results.csv"),collapse="")
write.table(pcaCVResults,
            file=knnCV_filename,
            row.names=FALSE,
            col.names=c("PCAthresh","k","Accuracy","Kappa","AccuracySD","KappaSD"),
            sep=',')




#Data generation, PCA, all Mixed
cat("Time: ",format(Sys.time(), "%X"),"\n\rGenerate accuracy vs. k data for ALL mixed, PCA:\n\r")
cl = parallelizeMe(4)
for(i in 1:length(pdPCAThresholds)) {
  #data_pcaAll
  cat("Time: ",format(Sys.time(), "%X"),"\n\r PCA threshold:",pdPCAThresholds[i],"\n\r")
  for(myK in k_list) {
    knnCV_filename = "../data/pcaknnAllMixedCV-"
    knnCV_filename = paste(c(knnCV_filename,"thresh",pdPCAThresholds[i],
                             "-dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
    if(!file.exists(knnCV_filename)) {
      knnCV = train(
        x = data_pcaAll$data,
        y = data_pcaAll$labels,
        preProcess = "pca",
        method = "knn",
        tuneGrid = expand.grid(k = myK),
        trControl=allMixedPCATrCtrl[[i]]
      )
      cat("DPI =",DPI, "sigma =",sigma,"PCA thresh = ", pdPCAThresholds[i],
          "k =",knnCV$results[[1]],"Accuracy = ",100*knnCV$results[[2]],"%\n\r")
      gc()
      save(knnCV,file = knnCV_filename)
    }
  }
}
unParallelizeMe(cl)

















#Bayes
#data_pcaPD
#pdFolds
nbPCAthresholds = seq(from=0.1,to=1.0,length.out = 20)
nbTrCtrl=list()
for(i in 1:length(nbPCAthresholds)) {
  nbTrCtrl[[i]]=trainControl(
    method="cv",
    number=10,
    index=pdFolds[1:10],
    verboseIter = TRUE,
    preProcOptions=c(thresh=nbPCAthresholds[i])
  )
}

cl = parallelizeMe()

i=1
nbPD = train(
  x=data_pcaPD$data,
  y=data_pcaPD$labels,
  preProcess="pca",
  method="nb",
  trControl=nbTrCtrl[[i]],
  tuneGrid=data.frame(.fL=c(0,1,0,1),.usekernel=c(TRUE,FALSE,FALSE,TRUE))
)
nbPDresults=cbind(thr=rbind(nbPCAthresholds[i],nbPCAthresholds[i],nbPCAthresholds[i],nbPCAthresholds[i]),nbPD$results)

for(i in 2:length(nbPCAthresholds)) {
  cat("thr =",nbPCAthresholds[i],"\n\r")
  nbPD = train(
    x=data_pcaPD$data,
    y=data_pcaPD$labels,
    preProcess="pca",
    method="nb",
    trControl=nbTrCtrl[[i]],
    tuneGrid=data.frame(.fL=c(0,1,0,1),.usekernel=c(TRUE,FALSE,FALSE,TRUE))
  )
  thresh=rbind(nbPCAthresholds[i],nbPCAthresholds[i],nbPCAthresholds[i],nbPCAthresholds[i])
  nbPDresults=rbind(nbPDresults,cbind(thr=thresh,nbPD$results))
}
unParallelizeMe(cl)
save(nbPDresults,file="../data/nbPDresults.RData")
load("../data/nbPDresults.RData")
write.csv(x=nbPDresults*1,file="../data/nbPDresults.csv")



#Help kiddi plot confMats
load("../data/confus_62.RData")
write.csv(x=confus, file="../data/confus_62.csv")
load("../data/confus_10.RData")
write.csv(x=confus, file="../data/confus_10.csv")
load("../data/confus_32.RData")
write.csv(x=confus, file="../data/confus_32.csv")