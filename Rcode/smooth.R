#-------------------------------------------------------------
#load libraries
#-------------------------------------------------------------
library("EBImage")

#-------------------------------------------------------------
#Functions to be used in evaluation
#-------------------------------------------------------------

#inspiration for smoothing
smoothAverage <- function(grayImg,k){
  kernel <- matrix( 
    1, # the data elements 
    k,# number of rows 
    k)
  kernel <- kernel/(k*k)
  #print(kernel)
  #using r library for smoothing
  smoothed <- filter2(grayImg, kernel)
  return(smoothed)
}

#Gaussian blur: See gblur from EBImage
# gblur(grayImg,sigma,radius = 2*ceiling(3*sigma)+1)

smoothGaussian <- function(grayImg,sigma,k = 2*ceiling(3*sigma)+1){
  return(gblur(grayImg,sigma,k))
}


