library("gplots")
library("ggplot2")

#width = { 18, 38, 58 }
displayDigit <- function(digit, filename="../data/image.png", width = 18) {
  
  height = length(digit)/width;
  
  # 	print(height)
  # 	print(width)
  img = matrix(NA,nrow=height,ncol=width)
  
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
  #bmp(filename=filename,width=width,height=height,units="px",antialias="none")
  writePNG(image=img,target = filename)
  
  #q = dev.off()
}


dkiddi <- loadSinglePersonsDataAverage(100,2,2,1)
dut=dkiddi[[9]][2,]
displayDigit(digit=dut,width=18,filename="../data/mikael_8_2_dpi100_k1.png")
dkiddi <- loadSinglePersonsDataAverage(100,2,2,3)
dut=dkiddi[[9]][2,]
displayDigit(digit=dut,width=18,filename="../data/mikael_8_2_dpi100_k3.png")
dkiddi <- loadSinglePersonsDataAverage(100,2,2,5)
dut=dkiddi[[9]][2,]
displayDigit(digit=dut,width=18,filename="../data/mikael_8_2_dpi100_k5.png")
#dkiddi[[3]][,1]
