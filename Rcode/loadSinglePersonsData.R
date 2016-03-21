#-------------------------------------------------------------
#load libraries
#-------------------------------------------------------------
library("png")
library("EBImage")
library("class")
library("gmodels")
library("ggplot2")
library("caret")
library("descr")
library("chemometrics")
library("foreach")
library("ggvis")
library("plotly")

#-------------------------------------------------------------
#Load custom stuff
#-------------------------------------------------------------
#source("setworkingdir.R")
source("smooth.R")


#loadOneVariable
loadOneVariable <- function(filename) {
  load(filename)
  return(trainingDigit)
}




#-------------------------------------------------------------
#This currently loads data according to the paths in the begining.
#Should be modified to load group members data.
#Uses the Gaussian smoothing filter
#k=1 is the same as no filtering.
#filename = data-<group#>-<member#>-<DPI>-<sigma>.RData
#Output:
#A large list with 10 elements. Each element is the dataset for a particular digit category.
#Each of the 10 elements contains 400 examples of the same digit.
#Each element is a 400 by 324 matrix of grayscale values in the range [0;1].
#That means each of the 400 rows contains a column of length 324
#describing the individual hand written digit.
#
#All 400 digits '4':
#data[[5]]
#The 166th '5':
#data[[6]][166,]
#The counting direction is as you would read it (digits 1-20 are in row 1, etc.).
#-------------------------------------------------------------
loadSinglePersonsDataOld <- function(DPI,groupNr,groupMemberNr,sigma){
  #TODO: See if file already exists.
  filename = paste(c("../data/data-",groupNr,"-",groupMemberNr,"-",DPI,"-",sigma,".RData"),collapse = "")
  if(file.exists(filename)){
    return(loadOneVariable(filename))
  }
  
  #ELSE
  cat("     Loading G",groupNr,"M",groupMemberNr," at ",DPI," DPI and sigma = ",sigma,".\n\r",sep="")
  
  #load the scanned images
  ciffers <- list(readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.png"), collapse = "")))
  
  #load the corner values
  cornerFilename = paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Corners.txt"), collapse = "")
  if(!(file.exists(cornerFilename))) {
    cornerFilename = paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/corners.txt"), collapse = "")
  }
  
  corners <- read.csv(cornerFilename)
  corners <- trunc(corners*DPI/300)
  #print(corners)
  
  
  #define lists to be used
  #  gray <- list(1:5)
  #   smoothed <- list(1:5)
  prepared <- list(1:5)
  
  
  #convert the images to gray scale.
  for(i in 1:5)
  {
    r <-ciffers[[i]][,,1]
    g <-ciffers[[i]][,,2]
    b <-ciffers[[i]][,,3]
    prepared[[i]] <- (r+g+b)/3
  }
  
  #smooth images with AUTOMATIC kernel size
  for(i in 1:5) {
    prepared[[i]] <- smoothGaussian(prepared[[i]],sigma)
  }
  
  #extract individual digits
  #Step is the "direction" of the 20x20 box with digits.
  xStep  <- (corners[1,7]-corners[1,1])/20;
  yStep  <- (corners[1,8]-corners[1,2])/20;
  #StepT is the size of ONE digit descriptor.
  xStepT <- 60*DPI/300#trunc(xStep)
  yStepT <- 60*DPI/300#trunc(yStep)
  
  #tempM is a temporary matrix for storing one 20x20 box
  #of digit descriptors as a 400 element vector,
  #where each element's size (dimensions) is defined by StepT.
  tempM <- matrix(data=NA,20*20,(yStepT-2)*(xStepT-2))
  #trainingDigit is a 10 element list.
  #Each element is a stored tempM for one digit, eg. '1' or '2'...
  trainingDigit <- list(1:10);
  
  #For each of the 5 pages
  for(page in 1:5)
  {
    #For each of the two 20x20 boxes on each page
    for(box in 1:2)
    {
      #For each column in the 20x20 box
      for(cifX in 1:20)
      {
        #aXbase is the x-coordinate of the upper left corner of one hand written digit.
        aXbase <- corners[(page-1)*2 + box,1] + xStep*(cifX-1)
        #For each row in the 20x20 box
        for(cifY in 1:20)
        {
          #aXbase is the y-coordinate of the upper left corner of one hand written digit.
          aYbase <- corners[(page-1)*2 + box,2] + yStep*(cifY-1)
          
          #For each pixel coordinate (px,py)
          for(px in 1:xStepT-2)
          {
            for(py in 1:yStepT-2)
            {
              #Insert the pixel intensity into the temporary matrix.
              tempM[(cifY-1)*20 + cifX, (px-1)*(yStepT-2) + py] <- prepared[[page]][aYbase+py+1,aXbase+px+1]
            }
          }
        }
      }
      #Append the temporary matrix (one entire 20x20 box) to the
      #list of digits.
      trainingDigit[[(page-1)*2 + box]] <- tempM
    }
  }
  
  #color grid to show whats used for training
  #The grid is colored black on top of the image.
  for(page in 1:5)
  {
    for(box in 1:2)
    {
      for(cifX in 1:21)
      {
        #Color pixels downwards
        aXbase <- corners[(page-1)*2 + box,1] + xStep*(cifX-1)
        xStart <- aXbase-1
        xEnd <- aXbase+1
        for(px in xStart:xEnd)
        {
          for(py in corners[(page-1)*2 + box,2]:corners[(page-1)*2 + box,8])
          {
            prepared[[page]][py,px] <- 0.0
          }
        }
        
        #Color pixels from left to right
        aYbase <- corners[(page-1)*2 + box,2] + yStep*(cifX-1)
        yStart <- aYbase-1
        yEnd <- aYbase+1
        for(py in yStart:yEnd)
        {
          for(px in corners[(page-1)*2 + box,1]:corners[(page-1)*2 + box,7])
          {
            prepared[[page]][py,px] <- 0.0
          }
        }
      }
    }
  }
  
  #show image
  #display(prepared[[1]])
  #display(prepared[[2]])
  #display(prepared[[3]])
  #display(prepared[[4]])
  #display(prepared[[5]])
  

  #Store the loaded and smoothed data so
  #it is automatically loaded next time
  #you call this function
  #with the same parameters.
  save(trainingDigit,file=filename)
  return(trainingDigit)
}



loadSinglePersonsDataCropped <- function(DPI,groupNr,groupMemberNr,sigma,year=2016){
  #TODO: See if file already exists.
  filename = paste(c("../data/data-",groupNr,"-",groupMemberNr,"-",DPI,"-",sigma,".RData"),collapse = "")
  if(file.exists(filename)){
    return(loadOneVariable(filename))
  }
  
  #ELSE
  cat("     Loading G",groupNr,"M",groupMemberNr," at ",DPI," DPI and sigma = ",sigma,".\n\r",sep="")
  
  #load the scanned images
  ciffers <- list(readPNG(paste(c("../data/cropped_images/cropY",year,"G",groupNr,"M",groupMemberNr,"-",DPI,"-0.png"), collapse = "")),
                  readPNG(paste(c("../data/cropped_images/cropY",year,"G",groupNr,"M",groupMemberNr,"-",DPI,"-1.png"), collapse = "")),
                  readPNG(paste(c("../data/cropped_images/cropY",year,"G",groupNr,"M",groupMemberNr,"-",DPI,"-2.png"), collapse = "")),
                  readPNG(paste(c("../data/cropped_images/cropY",year,"G",groupNr,"M",groupMemberNr,"-",DPI,"-3.png"), collapse = "")),
                  readPNG(paste(c("../data/cropped_images/cropY",year,"G",groupNr,"M",groupMemberNr,"-",DPI,"-4.png"), collapse = "")),
                  readPNG(paste(c("../data/cropped_images/cropY",year,"G",groupNr,"M",groupMemberNr,"-",DPI,"-5.png"), collapse = "")),
                  readPNG(paste(c("../data/cropped_images/cropY",year,"G",groupNr,"M",groupMemberNr,"-",DPI,"-6.png"), collapse = "")),
                  readPNG(paste(c("../data/cropped_images/cropY",year,"G",groupNr,"M",groupMemberNr,"-",DPI,"-7.png"), collapse = "")),
                  readPNG(paste(c("../data/cropped_images/cropY",year,"G",groupNr,"M",groupMemberNr,"-",DPI,"-8.png"), collapse = "")),
                  readPNG(paste(c("../data/cropped_images/cropY",year,"G",groupNr,"M",groupMemberNr,"-",DPI,"-9.png"), collapse = "")))
  
  prepared <- list(1:10)
  
  #convert the images to gray scale.
  for(i in 1:10) {
    r <-ciffers[[i]][,,1]
    g <-ciffers[[i]][,,2]
    b <-ciffers[[i]][,,3]
    prepared[[i]] <- (r+g+b)/3
  }
  
  #smooth images with AUTOMATIC kernel size
  for(i in 1:10) {
    prepared[[i]] <- smoothGaussian(prepared[[i]],sigma)
  }
  
  xStepT <- 60*DPI/300
  yStepT <- 60*DPI/300
  
  tempM <- matrix(data=NA,20*20,(yStepT-2)*(xStepT-2))
  trainingDigit <- list(1:10);
  
  for(digit in 1:10) {
    for(cifX in 1:20) {
      aXbase <- xStepT*(cifX-1)
      for(cifY in 1:20) {
        aYbase <- yStepT*(cifY-1)
        for(px in 1:xStepT-2) {
          for(py in 1:yStepT-2) {
            tempM[(cifY-1)*20 + cifX, (px-1)*(yStepT-2) + py] <- prepared[[digit]][aYbase+py+1,aXbase+px+1]
          }
        }
      }
    }
    trainingDigit[[digit]] <- tempM
  }

  save(trainingDigit,file=filename)
  return(trainingDigit)
}


loadSinglePersonsData <- function(DPI,groupNr,groupMemberNr,sigma,year=2016) {
  return(loadSinglePersonsDataCropped(DPI,groupNr,groupMemberNr,sigma,year))
}