source("preloading.R")
source("loadLabeledDataset.R")

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
trCtrl = trainControl(
  method = "repeatedcv",  #Repeated k-fold cross validation
  number = n_folds,       #Number of folds
  repeats = n_repeats,    #Number of repetitions (complete sets of folds to cross validate)
  verboseIter = TRUE,     #Print training log
  returnData = FALSE,     #A logical for saving the data???
  seeds = NA,             #A value of NA will stop the seed from being set within the worker processes
                          #while a value of NULL will set the seeds using a random set of integers.
  savePredictions = "final" #"final" saves the predictions for the optimal tuning parameters.
)
# Output parameters

# Data generation loop
for(loading_setup in loading_setups$persondependent) {
  DPI = loading_setup$DPI
  sigma_list = loading_setup$sigma_list
  #Calculate k-NN accuracy for all k-sigma combinations
  for(sigma in sigma_list) {
    dataset_filename = getLabeledDatasetFilename(persons_persondependent,DPI=DPI,sigma=sigma)
    data_persondependent = loadLabeledDataset(dataset_filename)
    for(myK in k_list) {
      knnCV_filename = "../data/knnPersonDependentCV-"
      knnCV_filename = paste(c(filename,"G",testPerson[1],"M",testPerson[2],"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
      knnCV = train(
        x = data_persondependent$data,
        y = data_persondependent$labels,
        preProcess = NULL,
        method = "knn",
        tuneGrid = expand.grid(k = myK),
        trControl=trCtrl
      )
      save(knnCV,file = filename)
    }
  }
}
# Data processing loop
for(loading_setup in loading_setups$persondependent) {
  DPI = loading_setup$DPI
  sigma_list = loading_setup$sigma_list
  for(sigma in sigma_list) {
    knnCVResultsAll=data.frame()
    for(myK in k_list) {
      knnCV_filename = "../data/knnPersonDependentCV-"
      knnCV_filename = paste(c(filename,"G",testPerson[1],"M",testPerson[2],"dpi",DPI,"-sig",sigma,"-k-",myK,"-rep",n_repeats,".RData"),collapse="")
      load(file = filename)
      knnCVResultsAll=rbind(knnCVResultsAll,knnCV$results)
    }
    knnCV_filename = "../data/knnPersonDependentCV-"
    knnCV_filename = paste(c(knnCV_filename,"G",testPerson[1],"M",testPerson[2],"dpi",DPI,"-k-",myK,"-rep",n_repeats,"-results.csv"),collapse="")
    write.csv(knnCVResultsAll,file=knnCV_filename,row.names=FALSE)
  }
}