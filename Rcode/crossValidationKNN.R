library("caret")
library("foreach")
library("doParallel")
source("loadSinglePersonsData.R")
source("params_kNN.R")

#todo: knnEval does pretty much all this in one step..... Great.

# Performs k-NN training with cross validation.
#Returns the cross validation results (see ?train, results).
# k , Accuracy (success rate), Kappa (Accuracy vs. random chance), Accuracy SD, Kappa SD
#With 10 folds and 10 repeats, you basically perform
#10 repeated trainings and testings with 90/10 splits,
#and return the mean of the prediction rates, and their standard deviations
#for varying k.
#@todo: How should data be normalized/preprocessed before being passed to k-NN?
#       Edit knnPreprocess inside function when you have the answer.
crossValidationKNNOnePerson <- function(DPI=100,
                               GroupNumber,
                               MemberNumber,
                               Sigma, #Gaussian smoothing variance
                               numberOfFolds = 10, #90/10 split corresponds to 10 folds
                               numberOfCVRepeats, #Cross validation repeats
                               k_list=expand.grid(k=c(1,2,3,4,5,6,7,8,9,10,11)) #k's to test
) {
  #k_list = NULL #k. See http://topepo.github.io/caret/modelList.html.
  #k_list = expand.grid(k=c(1,2,3,4,5,6,7,8,9,10,11))
  #k_list = expand.grid(k=c(1,2,3,4,5))
  #knnPreprocess = c("center","scale") #see ?preprocess. center subtracts µ. Scale divices by SD.
  #knnPreprocess = c("range") #see ?preprocess. range scales to [0;1] range based on training data.
  knnPreprocess = NULL
  
  #Load ONE persons data
  DataList = loadSinglePersonsData(DPI,GroupNumber,MemberNumber,Sigma)
  
  #We have lots of useless crap in memory now.
  #Therefore, we do garbage collection:
  gc()
  
  #DataList is 10 elements, each element is 400 by X.
  #X is the number of pixels in the digit descriptor.
  #What we want instead is a 4000 by X matrix of digit descriptors,
  #AND a 4000 by 1 list of labels aka. digit Class.
  #The first 400 elements of this list will be '1', etc.
  #This is what we setup below
  #For each data class (there are 10 different kinds of digit)
  for(j in 1:10)    
  {    
    #On the paper sheets, we started by writing zeroes, then ones, etc.,
    #so the current digit is (j-1).
    
    
    #This is the first digit, so we must initialize the data structure.
    if(j == 1)      
    {
      #Training data is initialized to all the 400 descriptors of
      #hand written zeros:
      data = DataList[[j]];   
      #We initialize the class/label list to 400 zeros:
      dataClass = rep(j-1,nrow(DataList[[j]]));
      #By now, we have data equal to a 400 by X matrix
      #of pixel intensities for hand written zeros,
      #and dataClass is equal to a 400 element vector of zeros.
      #This means dataClass is the true
      #value of what data describes.
    }    
    else      
    {
      #Append the pixel intensity descriptors of the next digit
      data = rbind(data, DataList[[j]]);
      #Append 400 labels of this digit (j-1) to the class list.
      dataClass = append(dataClass, rep(j-1, nrow(DataList[[j]]) ) );      
    }    
  }
  
  #Right now, dataClass is numeric.
  #Some times, you may want to have it
  #in factor form (categorical form).
  #For classification with the "train" method of "caret" library,
  #that is exactly what we want!
  #This is how you make that:
  dataClassF = factor(dataClass)
  
  
  #Cleanup memory before proceeding with k-NN:
  gc();
  
  #Setup cross validation. How to split data.
  control = trainControl(method="repeatedcv",
                         number = numberOfFolds,
                         repeats=numberOfCVRepeats)
  knnFit = train(x=data, #Training AND testing data (will be split internally, according to "control")
                 y=dataClassF, #Correct labels, same length as data.
                 method="knn",
                 trControl = control, #Repeated Cross Validation
                 #metric = "Kappa", #See internet for difference between Kappa and Accuracy
                 tuneGrid = k_list, #method parameter value list. knn has one parameter k. This is a list of k's to test.
                 preProcess = knnPreprocess #Normalization etc. before passing data to k-NN classifier. @todo: Is this necessary?
  )
  
  #Optimal k:
  #knnFit$bestTune
  #You can also (easily) find optimal k from the returned results!
  
  return(knnFit$results)
}

#Same as crossValidationKNNArbritrary, except
#data can be from multiple persons, or reduced
#data set from one person.
#data is a matrix where each row represents one example.
#dataClassF is a list of length nrow(data),
#where each element is the correct class corresponding to the row in data.
crossValidationKNNArbritrary <- function(DPI=100,
                                          data, #training and testing data (unsplit)
                                          dataClassF, #labels for data, factor form
                                        numberOfFolds = 10, #90/10 split corresponds to 10 folds
                                        numberOfCVRepeats, #Cross validation repeats
                                        k_list=expand.grid(k=c(1,2,3,4,5,6,7,8,9,10,11)) #k's to test
) {
  #k_list = NULL #k. See http://topepo.github.io/caret/modelList.html.
  #k_list = expand.grid(k=c(1,2,3,4,5,6,7,8,9,10,11))
  #k_list = expand.grid(k=c(1,2,3,4,5))
  #knnPreprocess = c("center","scale") #see ?preprocess. center subtracts µ. Scale divices by SD.
  #knnPreprocess = c("range") #see ?preprocess. range scales to [0;1] range based on training data.
  knnPreprocess = NULL

  
  #Setup cross validation. How to split data.
  control = trainControl(method="repeatedcv",
                         number = numberOfFolds,
                         repeats=numberOfCVRepeats)
  knnFit = train(x=data, #Training AND testing data (will be split internally, according to "control")
                 y=dataClassF, #Correct labels, same length as data.
                 method="knn",
                 trControl = control, #Repeated Cross Validation
                 #metric = "Kappa", #See internet for difference between Kappa and Accuracy
                 tuneGrid = k_list, #method parameter value list. knn has one parameter k. This is a list of k's to test.
                 preProcess = knnPreprocess #Normalization etc. before passing data to k-NN classifier. @todo: Is this necessary?
  )
  
  #Optimal k:
  #knnFit$bestTune
  #You can also (easily) find optimal k from the returned results!
  
  return(knnFit$results)
}

runCrossValidationKNNOnePerson <- function(GroupNumber,MemberNumber) {
  fn_prefix = paste(c("../data/cv-",GroupNumber,"-",MemberNumber,"-"), collapse = "")
  cat("Starting k-NN cross validation loop for group ", GroupNumber, ", member ", MemberNumber,"\n\r")
  for(DPI in DPI_list) {
    cat("DPI = ", DPI,"\n\r")
    for(sigma in sigma_list) {
      cat(" sigma = ", sigma,"\n\r")
      filename = paste(c(fn_prefix,DPI,"-",sigma,"-",n_repeats,".RData"),collapse = "")
      if(file.exists(filename)){
        #load(filename)
        #return(cv_result)
        
      }
      else {
        cv_result = crossValidationKNNOnePerson(DPI=DPI,
                                                GroupNumber=GroupNumber,
                                                MemberNumber=MemberNumber,
                                                Sigma=sigma,
                                                numberOfFolds=n_folds,
                                                numberOfCVRepeats = n_repeats,
                                                k_list = expand.grid(k=k_list))
        
        gc()
        
        #RData output
        save(cv_result,file=filename)
        
        #CSV output
        # filename = paste(c(fn_prefix,DPI,"-",sigma,"-",n_repeats,".csv"),collapse = "")
        # write.csv(cv_result,file=filename)
      }
    }
  }
  cat("Done!\n\r")
}





############################### Testing section ##### TODO

#DPI_list = c(100)
#n_repeats = 10
#runCrossValidationKNNOnePerson(2,2)

# DPI_list = c(200,300)
# n_repeats = 3
# runCrossValidationKNNOnePerson(2,2)
# 
# DPI_list = c(100)
# n_repeats = 10
# runCrossValidationKNNOnePerson(2,1)

# DPI_list = c(100,200,300)
# n_repeats = 3
# runCrossValidationKNNOnePerson(2,2)
# runCrossValidationKNNOnePerson(2,1)
# 

