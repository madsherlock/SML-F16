listOfDigits2classificationData <- function(dataList) {
  #dataList is 10 elements, each element is eg. 400 by X.
  #X is the number of pixels in the digit descriptor.
  #What we want instead is a 4000 by X matrix of digit descriptors,
  #AND a 4000 by 1 list of labels aka. digit Class.
  #The first 400 elements of this list will be '1', etc.
  #This is what we setup below
  #For each datmat class (there are 10 different kinds of digit)
  for(j in 1:10)    
  {    
    #On the paper sheets, we started by writing zeroes, then ones, etc.,
    #so the current digit is (j-1).
    
    
    #This is the first digit, so we must initialize the datmat structure.
    if(j == 1)      
    {
      #Training datmat is initialized to all the 400 descriptors of
      #hand written zeros:
      datmat = dataList[[j]];   
      #We initialize the class/label list to 400 zeros:
      dataClass = rep(j-1,nrow(dataList[[j]]));
      #By now, we have datmat equal to a 400 by X matrix
      #of pixel intensities for hand written zeros,
      #and dataClass is equal to a 400 element vector of zeros.
      #This means dataClass is the true
      #value of what datmat describes.
    }    
    else      
    {
      #Append the pixel intensity descriptors of the next digit
      datmat = rbind(datmat, dataList[[j]]);
      #Append 400 labels of this digit (j-1) to the class list.
      dataClass = append(dataClass, rep(j-1, nrow(dataList[[j]]) ) );      
    }    
  }
  
  #Right now, dataClass is numeric.
  #Some times, you may want to have it
  #in factor form (categorical form).
  #For classification with the "train" method of "caret" library,
  #that is exactly what we want!
  #This is how you make that:
  dataClassF = factor(dataClass)
  
  return(data.frame(dat=datmat,dataClassF=dataClassF))
}


#Labels are "X0", "X1", ... , "X9" instead of "0", "1", ..., "9".
listOfDigits2classificationDataAlt <- function(dataList) {
  #dataList is 10 elements, each element is eg. 400 by X.
  #X is the number of pixels in the digit descriptor.
  #What we want instead is a 4000 by X matrix of digit descriptors,
  #AND a 4000 by 1 list of labels aka. digit Class.
  #The first 400 elements of this list will be '1', etc.
  #This is what we setup below
  #For each datmat class (there are 10 different kinds of digit)
  for(j in 1:10)    
  {    
    #On the paper sheets, we started by writing zeroes, then ones, etc.,
    #so the current digit is (j-1).
    
    
    #This is the first digit, so we must initialize the datmat structure.
    if(j == 1)      
    {
      #Training datmat is initialized to all the 400 descriptors of
      #hand written zeros:
      datmat = dataList[[j]];   
      #We initialize the class/label list to 400 zeros:
      dataClass = rep(j-1,nrow(dataList[[j]]));
      #By now, we have datmat equal to a 400 by X matrix
      #of pixel intensities for hand written zeros,
      #and dataClass is equal to a 400 element vector of zeros.
      #This means dataClass is the true
      #value of what datmat describes.
    }    
    else      
    {
      #Append the pixel intensity descriptors of the next digit
      datmat = rbind(datmat, dataList[[j]]);
      #Append 400 labels of this digit (j-1) to the class list.
      dataClass = append(dataClass, rep(j-1, nrow(dataList[[j]]) ) );      
    }    
  }
  
  #Right now, dataClass is numeric.
  #Some times, you may want to have it
  #in factor form (categorical form).
  #For classification with the "train" method of "caret" library,
  #that is exactly what we want!
  #This is how you make that:
  dataClassF = factor(make.names(factor(dataClass)))
  
  resultL = list(data=datmat,dataClassF=dataClassF)

  return(resultL)
}