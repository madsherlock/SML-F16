library("png")
library("EBImage")
library("class")
library("gmodels")
library("ggplot2")
library("caret")
library("base")
smoothGaussian <- function(grayImg,sigma,k = 2*ceiling(3*sigma)+1){
  return(gblur(grayImg,sigma,k))
}

Preprocessing <- function(DPI, Sigma ,  groupNr,groupMemberNr )
{
  filename = paste("../data/data-<", groupNr,">-<", groupMemberNr,">-<", DPI,">-<", Sigma,">.RData",sep="")
  message(filename)
  ## Load images
  ciffers <- list(readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-0.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-1.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-2.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-3.png"), collapse = "")),
                  readPNG(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Ciphers",DPI,"-4.png"), collapse = "")))
  #load the corner values
  corners <- read.csv(paste(c("../../SML-database/2016/group",groupNr,"/member",groupMemberNr,"/Corners.txt"), collapse = ""))
  corners <- trunc(corners*DPI/300)
  
  #define lists to be used
  #  gray <- list(1:5)
  #   smoothed <- list(1:5)
  prepared <- list(1:5)
  
  ##Conver to gray scale
  for(i in 1:5)
  {
    r <-ciffers[[i]][,,1]
    g <-ciffers[[i]][,,2]
    b <-ciffers[[i]][,,3]
    prepared[[i]] <- (r+g+b)/3
  }
  
  #smooth images
  for(i in 1:5)
  {
    prepared[[i]] <- smoothGaussian(prepared[[i]],Sigma)
  }
  
  #extract individual ciffers
  xStep  <- (corners[1,7]-corners[1,1])/20;
  yStep  <- (corners[1,8]-corners[1,2])/20;
  xStepT <- 60*DPI/300#trunc(xStep)
  yStepT <- 60*DPI/300#trunc(yStep)
  
  tempM <- matrix(,20*20,(yStepT-2)*(xStepT-2))
  trainingDigit <- list(1:10);
  
  for(pages in 1:5)
  {
    for(box in 1:2)
    {
      #     trainingDigit[[(pages-1)*2 + box]] <- matrix(,20*20,(yStepT-2)*(xStepT-2))) 
      for(cifX in 1:20)
      {
        aXbase <- corners[(pages-1)*2 + box,1] + xStep*(cifX-1)
        for(cifY in 1:20)
        {
          aYbase <- corners[(pages-1)*2 + box,2] + yStep*(cifY-1)
          
          for(px in 1:xStepT-2)
          {
            for(py in 1:yStepT-2)
            {
              tempM[(cifY-1)*20 + cifX, (px-1)*(yStepT-2) + py] <- prepared[[pages]][aYbase+py+1,aXbase+px+1]
            }
          }
        }
      }
      trainingDigit[[(pages-1)*2 + box]] <- tempM
    }
  }
  
  # #color grid to show whats used for training
  # for(pages in 1:5)
  # {
  #   for(box in 1:2)
  #   {
  #     for(cifX in 1:21)
  #     {
  #       aXbase <- corners[(pages-1)*2 + box,1] + xStep*(cifX-1)
  #       xStart <- aXbase-1
  #       xEnd <- aXbase+1
  #       for(px in xStart:xEnd)
  #       {
  #         for(py in corners[(pages-1)*2 + box,2]:corners[(pages-1)*2 + box,8])
  #         {
  #           prepared[[pages]][py,px] <- 0.0
  #         }
  #       }
  #       
  #       aYbase <- corners[(pages-1)*2 + box,2] + yStep*(cifX-1)
  #       yStart <- aYbase-1
  #       yEnd <- aYbase+1
  #       for(py in yStart:yEnd)
  #       {
  #         for(px in corners[(pages-1)*2 + box,1]:corners[(pages-1)*2 + box,7])
  #         {
  #           prepared[[pages]][py,px] <- 0.0
  #         }
  #       }
  #     }
  #   }
  # }
  # 
  # #show image
  # #   img <- ciffers[[1]]
  # #   img[,,1] <- prepared[[1]]
  # #   img[,,2] <- prepared[[1]]
  # #   img[,,3] <- prepared[[1]]
  # display(prepared[[1]])
  # display(prepared[[2]])
  # display(prepared[[3]])
  # display(prepared[[4]])
  # display(prepared[[5]])
  # 
  save(trainingDigit, file = filename)
  
  #use the generated training data to apply learning
  #print( trainingDigit[[1]] )
  
  #return(trainingDigit)
  
}


DPI_loop = c(100,200,300)
sigma_loop = c(0.2, 0.3, 0.6, 1.0, 1.5, 2.0, 2.5)

for(DPI in 1:length(DPI_loop)){
  for (Sigma in 1:length(sigma_loop)) {
    message(DPI_loop[DPI])
    message(sigma_loop[Sigma])
    Preprocessing(DPI_loop[DPI],sigma_loop[Sigma],2,1)
  }
}
