library("gplots")
library("ggplot2")

displayDigit <- function(digit, filename="../data/image.png", width = 18) {
  
  height = length(digit)/width;
  
  # 	print(height)
  # 	print(width)
  img = matrix(NA,height,width)
  
  for( d in 1:height) {
    img[d,] = digit[(d*width):((d-1)*width+1)]
  }
  
  sum = 0
  for( y in 1:height ) {
    for ( x in 1:width ){
      sum = sum + img[x,y]
    }
  }
  print(c("sum: ",sum))
  
  #setEPS()
  #postscript(filename,height = height, width = width)
  #png(filename,height=height,width=width,units="px",antialias = "none")
  #image(img, xaxt="n", yaxt="n",col=colorpanel(256, "black", "white"))
  
  #q = dev.off()
}


dkiddi <- loadSinglePersonsDataAverage(300,2,1,1)
displayDigit(dkiddi[[6]][166,],width=18)
#dkiddi[[3]][,1]
