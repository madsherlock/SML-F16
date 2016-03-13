#
# This script carries out a running time test of the pure k-NN algorithm
# on data from one person.
# For each DPI, running time is measured vs. k=1,...,40 and dataset size n<=4000.
# No cross validation.
# An example confusion matrix is also exported.
#

source("loadSinglePersonsData.R")
source("knnPerformanceNoCV.R")
source("toFraction.R")


DPI_list = c(100,200,300);
k_list = seq(1,40,1);
GroupNumber = 2;
MemberNumber = 2;
sigma_list = c(1.5,2.5,3.5);
n_list = seq(500,4000,500);
DPI_tests <- list()

for(i in 1:length(DPI_list))
{
  DPI=DPI_list[i]
  sigma=sigma_list[i]
  cat("DPI = ",DPI,", sigma = ",sigma,". ");
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
  
  cat("N = {");
  for(kx in 1:(length(n_list)-1))
  {
    cat(n_list[kx],",");
    frac=toFraction(as.double(n_list[kx])/as.double(nrow(data)));
    indices=seq.int(from=1L,to=nrow(data),by=frac$denominator);
    if(frac$numerator != 1)
    {
      for(l in 2L:frac$numerator)
      {
        indices = c(indices,seq.int(from=l,to=nrow(data),by=frac$denominator));
      }
    }
    DPI_tests[[length(DPI_tests)+1]] = list(sigma=sigma,
                                            knnPerformance=knnPerformanceNoCV(data=data[indices,],
                                                                              dataClassF=dataClassF[indices],
                                                                              trainPercent = 0.9,
                                                                              k_list=k_list)
    )
  }
  cat(nrow(data));
  DPI_tests[[length(DPI_tests)+1]] = list(sigma=sigma,
                                          knnPerformance=knnPerformanceNoCV(data=data,
                                                                            dataClassF=dataClassF,
                                                                            trainPercent = 0.9,
                                                                            k_list=k_list)
  )
  cat("}\n\r");
  
  #N=4000
  #DPI_tests[[i]]=list(sigma=sigma,knnPerformance=knnPerformanceNoCV(data=data,dataClassF=dataClassF,trainPercent = 0.9,k_list=k_list))
}

save(DPI_tests,file="../data/DPI_tests.RData")






#Write an example confusion matrix

load("../data/DPI_tests.RData")
testConMat = DPI_tests[[length(n_list)]]
filenameConMat=paste(c("../data/confmatkNN-",
                       GroupNumber,"-",MemberNumber,
                       "-sig",testConMat$sigma,
                       "-k",testConMat$knnPerformance$k_list[1],
                       "-n",testConMat$knnPerformance$dataset_size,
                       ".csv"), collapse = "")
conMat=as.table(testConMat$knnPerformance$knnConfusionMatrix[[1]])
#Rows: Prediction. Columns: Reference. So conMat[10,4] is number of 3's incorrectly predicted as 9's.
write.csv(x=conMat,file=filenameConMat)



#Write table of x=n, y=k, z=running time for each DPI
#to justify DPI choice and to gain insight into k-NN search method complexity.
x=vector(mode="integer")
y=vector(mode="integer")
for( i in 1:length(n_list)) {
  for( j in 1:length(k_list)) {
    x=c(x,n_list[i])
  }
  y=c(y,k_list)
}
for( i in 1:length(DPI_list) ) {
  filenameRunningTime = paste(c("../data/runningTimekNN-",
                                GroupNumber,"-",MemberNumber,
                                "-sig",sigma_list[i],"-DPI",DPI_list[i],
                                ".csv"),collapse = "")
  z=vector(mode="numeric")
  for( j in 1:length(n_list) ) {
    z=c(z,DPI_tests[[(i-1)*(length(n_list))+j]]$knnPerformance$knnTime)
  }
  runningTime=data.frame(n=x,k=y,t=z)
  write.csv(x=runningTime,file=filenameRunningTime,row.names = FALSE)
}