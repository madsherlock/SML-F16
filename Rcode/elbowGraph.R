# Program Name: elbowGrahp.R
# Author: Alberto Negron
# date: 23/06/2013
# Description: Plot an elbow Graph for choosing k number of clusters
# Input Params:
#   multiKmeansObj:data frame produced by multiKmeans function
# Output:
#   ggplot graph

elbowGraph = function(multiKmeansObj){
  p = plot(x = multiKmeansObj$cluster , y = multiKmeansObj$ss, xlab =  "# of clusters", ylab = "Sum of squared")
  title(main = "Elbow plot")
  max = max(multiKmeansObj$ss)
  ticks<-c(0,10,20,30,40,50,60,70,80,90,100,200,300,400)
  axis(1,at=ticks,labels=ticks)
  #p = qplot(x=as.numeric(cluster),y=ss,data=multiKmeansObj)
  #p = p + geom_point(colour = "red", size = 2) #+ geom_text(aes(label=as.numeric(cluster),size=1,hjust=-0.5, vjust=0))
  #p = p + labs(title = "Elbow Graph") + xlab("# of Clusters") + ylab("Sum of Squares") + theme_bw()
  #p = p +  theme(legend.position = "none") 
  #p = p + facet_grid(~., drop = TRUE )
  return(p)
}

elbowGraphMult = function(multiKmeansObj, multiKmeansObj_2, multiKmeansObj_4, multiKmeansObj_8, multiKmeansObj_16 ){
  max = max(multiKmeansObj$ss,multiKmeansObj_2$ss,multiKmeansObj_4$ss,multiKmeansObj_8$ss,multiKmeansObj_16$ss)
  print(max)
  plot(x = multiKmeansObj$cluster , y = multiKmeansObj$ss,  col = "red" ,xlab =  "# of clusters", ylab = "Sum of squared", ylim = c(0,max), xaxt = "n")
  par(new = TRUE)
  plot(x = multiKmeansObj_2$cluster , y = multiKmeansObj_2$ss,  col = "green" ,xlab =  "# of clusters", ylab = "Sum of squared",ylim = c(0,max), xaxt = "n")
  par(new=TRUE)
  plot(x = multiKmeansObj_4$cluster , y = multiKmeansObj_4$ss,  col = "blue" ,xlab =  "# of clusters", ylab = "Sum of squared",ylim = c(0,max), xaxt = "n")
  par(new=TRUE)
  plot(x = multiKmeansObj_8$cluster , y = multiKmeansObj_8$ss,  col = "cyan" ,xlab =  "# of clusters", ylab = "Sum of squared",ylim = c(0,max), xaxt = "n")
  par(new=TRUE)
  plot(x = multiKmeansObj_16$cluster , y = multiKmeansObj_16$ss, col = "black" ,xlab =  "# of clusters", ylab = "Sum of squared",ylim = c(0,max), xaxt = "n")
  title(main = "Elbow plot")
  legend(300,max, c("Full data","1/2 data", "1/4 data", "1/8 data", "1/16 data"),lty = c(1,1,1,1,1), lwd = c(2.5,2.5,2.5,2.5,2.5), col = c("red","green","blue","cyan","black"),  bty = "n")
  ticks<-c(0,10,20,30,40,50,60,70,80,90,100,200,300,400)
  axis(1,at=ticks,labels=ticks)
  
  #p = qplot(x=as.numeric(cluster),y=ss,data=multiKmeansObj)
  #p = p + geom_point(colour = "red", size = 2) #+ geom_text(aes(label=as.numeric(cluster),size=1,hjust=-0.5, vjust=0))
  #p = p + labs(title = "Elbow Graph") + xlab("# of Clusters") + ylab("Sum of Squares") + theme_bw()
  #p = p +  theme(legend.position = "none") 
  #p = p + facet_grid(~., drop = TRUE 
}

