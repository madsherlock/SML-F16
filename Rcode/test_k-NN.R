source("loadSinglePersonsData.R")
source("knnPerformanceNoCV.R")


DPI_list = c(100,200,300);
k_list = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
GroupNumber = 2;
MemberNumber = 2;
sigma_list = c(1.5,2.5,3.5);

DPI_tests <- list(1:3)

for(i in 1:length(DPI_list))
{
  DPI=DPI_list[i]
  sigma=sigma_list[i]
  #Load one persons data into a 10 x 400 x X structure
  DataList = loadSinglePersonsData(DPI,GroupNumber,MemberNumber,sigma)
  gc(); #garbage collection
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
  #Cleanup memory before proceeding with splitting data:
  remove(DataList);
  gc();
  
  DPI_tests[[i]]=list(sigma=sigma,knnPerformance=knnPerformanceNoCV(data=data,dataClassF=dataClassF,trainPercent = 0.9,k_list=k_list))
}

save(DPI_tests,file="../data/DPI_tests.RData")
