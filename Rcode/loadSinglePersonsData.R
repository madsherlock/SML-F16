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
source("setworkingdir.R")
source("loadOneVariable.R")
source("smooth.R")

#-------------------------------------------------------------
#This currently loads data according to the paths in the begining.
#Should be modified to load group members data.
#Uses the Gaussian smoothing filter
#k=1 is the same as no filtering.
#filename = data-<group#>-<member#>-<DPI>-<sigma>-<kernel size>.RData
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
loadSinglePersonsData <- function(DPI,groupNr,groupMemberNr,sigma,k){
  #TODO: See if file already exists.
  filename = paste(c("../data/data-",groupNr,"-",groupMemberNr,"-",DPI,"-",sigma,"-",k,".RData"),collapse = "")
  if(file.exists(filename)){
    return(loadOneVariable(filename))
  }
  
  #ELSE
  
  #load the scanned images
  ciffers <- list(readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.png"), collapse = "")))
  #load the corner values
  corners <- read.csv(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Corners.txt"), collapse = ""))
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
  
  #smooth images
  for(i in 1:5) {
    prepared[[i]] <- smoothGaussian(prepared[[i]],sigma,k)
    #prepared[[i]] <- smoothImage(prepared[[i]],k)
  }
  
  #extract individual digits
  xStep  <- (corners[1,7]-corners[1,1])/20;
  yStep  <- (corners[1,8]-corners[1,2])/20;
  xStepT <- 60*DPI/300#trunc(xStep)
  yStepT <- 60*DPI/300#trunc(yStep)
  
  tempM <- matrix(data=NA,20*20,(yStepT-2)*(xStepT-2))
  trainingDigit <- list(1:10);
  
  for(page in 1:5)
  {
    for(box in 1:2)
    {
      #     trainingDigit[[(pages-1)*2 + box]] <- matrix(,20*20,(yStepT-2)*(xStepT-2))) 
      for(cifX in 1:20)
      {
        aXbase <- corners[(page-1)*2 + box,1] + xStep*(cifX-1)
        for(cifY in 1:20)
        {
          aYbase <- corners[(page-1)*2 + box,2] + yStep*(cifY-1)
          
          for(px in 1:xStepT-2)
          {
            for(py in 1:yStepT-2)
            {
              tempM[(cifY-1)*20 + cifX, (px-1)*(yStepT-2) + py] <- prepared[[page]][aYbase+py+1,aXbase+px+1]
            }
          }
        }
      }
      trainingDigit[[(page-1)*2 + box]] <- tempM
    }
  }
  
  #color grid to show whats used for training
  for(page in 1:5)
  {
    for(box in 1:2)
    {
      for(cifX in 1:21)
      {
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
  #   img <- ciffers[[1]]
  #   img[,,1] <- prepared[[1]]
  #   img[,,2] <- prepared[[1]]
  #   img[,,3] <- prepared[[1]]
  display(prepared[[1]])
  #display(prepared[[2]])
  #display(prepared[[3]])
  #display(prepared[[4]])
  #display(prepared[[5]])
  
  #use the generated training data to apply learning
  #print( trainingDigit[[1]] )
  
  save(trainingDigit,file=filename)
  return(trainingDigit)
}

loadSinglePersonsDataAverage <- function(DPI,groupNr,groupMemberNr,k){
  #TODO: See if file already exists.
  filename = paste(c("../data/data-",groupNr,"-",groupMemberNr,"-",DPI,"-",k,".RData"),collapse = "")
  if(file.exists(filename)){
    return(loadOneVariable(filename))
  }
  
  #ELSE
  
  #load the scanned images
  ciffers <- list(readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.png"), collapse = "")))
  #load the corner values
  corners <- read.csv(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Corners.txt"), collapse = ""))
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
  
  #smooth images
  for(i in 1:5) {
    #prepared[[i]] <- smoothGaussian(prepared[[i]],sigma,k)
    prepared[[i]] <- smoothAverage(grayImg=prepared[[i]],k=k)
  }
  
  #extract individual digits
  xStep  <- (corners[1,7]-corners[1,1])/20;
  yStep  <- (corners[1,8]-corners[1,2])/20;
  xStepT <- 60*DPI/300#trunc(xStep)
  yStepT <- 60*DPI/300#trunc(yStep)
  
  tempM <- matrix(data=NA,20*20,(yStepT-2)*(xStepT-2))
  trainingDigit <- list(1:10);
  
  for(page in 1:5)
  {
    for(box in 1:2)
    {
      #     trainingDigit[[(pages-1)*2 + box]] <- matrix(,20*20,(yStepT-2)*(xStepT-2)))
      for(cifX in 1:20)
      {
        aXbase <- corners[(page-1)*2 + box,1] + xStep*(cifX-1)
        for(cifY in 1:20)
        {
          aYbase <- corners[(page-1)*2 + box,2] + yStep*(cifY-1)
          
          for(px in 1:xStepT-2)
          {
            for(py in 1:yStepT-2)
            {
              tempM[(cifY-1)*20 + cifX, (px-1)*(yStepT-2) + py] <- prepared[[page]][aYbase+py+1,aXbase+px+1]
            }
          }
        }
      }
      trainingDigit[[(page-1)*2 + box]] <- tempM
    }
  }
  
  #color grid to show whats used for training
  for(page in 1:5)
  {
    for(box in 1:2)
    {
      for(cifX in 1:21)
      {
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
  #   img <- ciffers[[1]]
  #   img[,,1] <- prepared[[1]]
  #   img[,,2] <- prepared[[1]]
  #   img[,,3] <- prepared[[1]]
  #display(prepared[[1]])
  #display(prepared[[2]])
  #display(prepared[[3]])
  #display(prepared[[4]])
  #display(prepared[[5]])
  
  #use the generated training data to apply learning
  #print( trainingDigit[[1]] )
  
  save(trainingDigit,file=filename)
  return(trainingDigit)
}



