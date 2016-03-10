source("Rcode2.R")
#Example for loading data from multiple people:
DPI = 100;

#These values are used to specify which of the full data list indices 
#are used for training and testing
NrOfTrainingSets = 1;
NrOfTestingSets = 0;
NrOfDataSets = NrOfTrainingSets + NrOfTestingSets;

fullDataList = list(1:NrOfDataSets)


#add as many people as you have specified training and test data
fullDataList[[1]] = loadSinglePersonsData(DPI,2,1); gc();#this is the first training set
#fullDataList[[2]] = loadSinglePersonsData(DPI,2,2); gc();#this is the second
#fullDataList[[3]] = loadSinglePersonsData(DPI,1,2); gc();#this is the only test set


#first load the training set
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

#then load the test set
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

#some classifiers need the class label as factors, so
#here the classes are mapped to factors:
trainClassF = factor(trainClass)
testClassF = factor(testClass)

#otherwice these functions print the data in a format that is ready for
#evaluation:
str(training)
str(trainClass)
str(testing)
str(testClass)


