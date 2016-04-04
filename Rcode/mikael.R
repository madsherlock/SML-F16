source("preloading.R")
source("loadLabeledDataset.R")
source("parallelize.R")
source("knnPerformanceNoCV.R")
source("smlPCA.R")
library(stats)



getLabeledDatasetFilename <- function(list_of_persons, DPI, sigma, dataset_name = paste(unlist(list_of_persons),collapse="-")) {
  filename_prefix = paste(c("../data/labeled_datasets/data-labeled-",dataset_name),collapse="")
  filename = paste(c(filename_prefix,"-dpi",DPI,"-sigma",sigma,".RData"),collapse="")
  return(filename)
}




preloadEverything(FALSE)





#Smoothing test
# Input parameters
k_list = seq(1,11,1)
n_folds = 10
n_repeats = 10
testPerson = persons_persondependent[[1]]

#Create fold indices for CV.
#Each element of pdFolds is the indices for the training data
#of one fold. This means for 10-fold cross validation with 10 runs,
#pdFolds will have 100 elements, each being a set of indices to use for
#training. Set seed to 4578 to always create the same folds,
#so that the obtained results are reproducible.
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

# Data generation loop (Takes several days - about 3 days with 8 cores @2.4 GHz)
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
sigma=0.6
# --------------------------------------------
# --------------------------------------------
dataset_filename = getLabeledDatasetFilename(persons_all_loadable,DPI=DPI,sigma=sigma)
data_pcaAll = loadLabeledDataset(dataset_filename)
testPCAAll=smlPCA(data_pcaAll$data)

dataset_filename = getLabeledDatasetFilename(persons_persondependent,DPI=DPI,sigma=sigma)
data_pcaPD = loadLabeledDataset(dataset_filename)
testPCAPD=smlPCA(data_pcaPD$data)

dataset_filename = getLabeledDatasetFilename(persons_fewer,DPI=DPI,sigma=sigma)
data_pcaFewer = loadLabeledDataset(dataset_filename)
testPCAFewer=smlPCA(data_pcaFewer$data)
#2.1.2: Show the eigenvalues, variance and the accumulated variance of the principal components.
write.csv(testPCAPD$sdev,
          file=paste(c("../data/pca-G",
                       testPerson[1],"M",testPerson[2],
                       "-sdev.csv"),collapse=""))
write.csv(testPCAAll$sdev,
          file=paste(c("../data/pca-All-sdev.csv"),collapse=""))
write.csv(testPCAFewer$sdev,
          file=paste(c("../data/pca-Fewer-sdev.csv"),collapse=""))




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
