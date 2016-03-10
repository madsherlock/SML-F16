#-------------------------------------------------------------
library("png")
library("EBImage")
library("class")
library("gmodels")
library("ggplot2")
library("caret")
library("fun")
library("lattice")
library("dplyr")
library("doMC")
library("parallel")
library("iterators")
registerDoMC(cores = 4)
#-------------------------------------------------------------



knn_cv <- function()
{
  DPI_loop = c(100,200,300)
  sigma_loop = c(0.2, 0.3, 0.6, 1.0, 1.5, 2.0, 2.5)
  for(DPI in 1:length(DPI_loop)){
    for (Sigma in 1:length(sigma_loop)) {
      message(DPI_loop[DPI])
      message(sigma_loop[Sigma])
      filename = paste("../data/data-<",2,">-<",1,">-<", DPI_loop[DPI],">-<", sigma_loop[Sigma],">.RData",sep="")
      #variable = paste(DPI_loop[DPI], sigma_loop[Sigma])
      #variable <-read.table(filename) 
      load(filename)
      message("Loaded")
    }
  }
}
#knn_cv()

filename = paste("../data/data-<",2,">-<",2,">-<", 300,">-<", 0.2,">.RData",sep="")
load(filename)
load("../data/testClass.RData")

label <- list(testClass[1:400],testClass[401:800],testClass[801:1200],testClass[1201:1600],testClass[1601:2000],testClass[2001:2400],testClass[2401:2800],testClass[2801:3200],testClass[3201:3600],testClass[3601:4000])

control = trainControl(method="repeatedcv",
                       number = 10,
                       repeats=10,
                       p = 0.9)

k_list=expand.grid(k=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))
labelF = factor(label[[4]])

knnFit = train(x=trainingDigit[[4]], #Training AND testing data (will be split internally, according to "control")
               y=labelF, #Correct labels, same length as data.
               method="knn",
               trControl = control, #Repeated Cross Validation
               tuneGrid = k_list, #method parameter value list. knn has one parameter k. This is a list of k's to test.
               preProcess = NULL #Normalization etc. before passing data to k-NN classifier. @todo: Is this necessary?
)

knnFit$results


for(j in 1:10)    
{    
  #On the paper sheets, we started by writing zeroes, then ones, etc.,
  #so the current digit is (j-1).
  
  
  #This is the first digit, so we must initialize the data structure.
  if(j == 1)      
  {
    #Training data is initialized to all the 400 descriptors of
    #hand written zeros:
    data = trainingDigit[[j]];   
    #We initialize the class/label list to 400 zeros:
    dataClass = rep(j-1,nrow(trainingDigit[[j]]));
    #By now, we have data equal to a 400 by X matrix
    #of pixel intensities for hand written zeros,
    #and dataClass is equal to a 400 element vector of zeros.
    #This means dataClass is the true
    #value of what data describes.
  }    
  else      
  {
    #Append the pixel intensity descriptors of the next digit
    data = rbind(data, trainingDigit[[j]]);
    #Append 400 labels of this digit (j-1) to the class list.
    dataClass = append(dataClass, rep(j-1, nrow(trainingDigit[[j]]) ) );      
  }    
}

#Right now, dataClass is numeric.
#Some times, you may want to have it
#in factor form (categorical form).
#For classification with the "train" method of "caret" library,
#that is exactly what we want!
#This is how you make that:
dataClassF = factor(dataClass)

trainingDigit[[1]]
dataClass[[1]]
#Cleanup memory before proceeding with k-NN:
gc();


#Setup cross validation. How to split data.
control = trainControl(method="repeatedcv",
                       number = 10,
                       repeats=10)
knnFit = train(x=data, #Training AND testing data (will be split internally, according to "control")
               y=dataClassF, #Correct labels, same length as data.
               method="knn",
               trControl = control, #Repeated Cross Validation
               #metric = "Kappa", #See internet for difference between Kappa and Accuracy
               tuneGrid = k_list, #method parameter value list. knn has one parameter k. This is a list of k's to test.
               preProcess = NULL #Normalization etc. before passing data to k-NN classifier. @todo: Is this necessary?
)
