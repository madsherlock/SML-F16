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

# 
# dkiddi <- loadSinglePersonsData(300,2,2,sigma=0.1,k=15)
# dut=dkiddi[[9]][2,]
# displayDigit(digit=dut,width=58,filename="../data/mikael_8_2_dpi300_k15_sig0.1.png")
# dkiddi2 <- loadSinglePersonsData(300,2,2,sigma=0.4,k=15)
# dut2=dkiddi2[[9]][2,]
# displayDigit(digit=dut2,width=58,filename="../data/mikael_8_2_dpi300_k15_sig0.4.png")
# dkiddi3 <- loadSinglePersonsData(300,2,2,sigma=0.7,k=15)
# dut3=dkiddi3[[9]][2,]
# displayDigit(digit=dut3,width=58,filename="../data/mikael_8_2_dpi300_k15_sig0.7.png")
#dkiddi[[3]][,1]
