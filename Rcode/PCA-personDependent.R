##Peform PCA Person Dependent -  Kiddi
library("stats")
library("factoextra")
library("wnominate")
library("device")
library("base")
library("latticeExtra")
library("doMC")
registerDoMC(cores = 4)
library("caret")

filename = paste("../data/data-",1,"-",1,"-", 100,"-", 1.5,".RData",sep="")
load(filename)

PCA = prcomp(trainingDigit[[1]],scale = TRUE)
names(PCA)
head(PCA$sdev)
head(PCA$rotation)
summary(PCA)
png = plot(1:(length(PCA$sdev)),  (PCA$sdev)^2, type='b', main="Scree Plot - digit '0'", xlab="Number of Components", ylab="Eigenvalue Size")
screeplot(PCA)



##Vælger de første to PCA og træner på dem
summary(PCA)
summary(PCA$sdev)
PCA.1 <- PCA$x[,1]
PCA.2 <- PCA$x[,2]
train <- cbind(PCA.1, PCA.2)
model.knn <- knn(train,train, test=train, cl=label[[1]], k=19, prob=T)
summary(model.knn)
model.knn


##2.1.1
# Perform a PCA on the data for the person dependent and the person independent data set.
filename = paste("../data/data-",2,"-",2,"-", 100,"-", 1.5,".RData",sep="")
load(filename)
PCA = prcomp(trainingDigit[[1]],scale = TRUE)
names(PCA)
head(PCA$sdev)
head(PCA$rotation)
summary(PCA)
##Filename =  screeplot - group - member - digit
png(filename="/Users/keerthikanratnarajah/Dropbox/Eksamen/SML/SML-F16/data/screeplot-2-2-0.png")
png = plot(1:(length(PCA$sdev)),  (PCA$sdev)^2, type='b', main="Scree Plot - digit '0'", xlab="Number of Components", ylab="Eigenvalue Size")
dev.off()
#screeplot(PCA)


##2.1.2
#Show the eigenvalues, variance and the accumulated variance of the principal components.
#(In the report 10-20 values equally distributed, should be sufficient to illustrate the
#tendencies.)


#2.1.3 
#Show the performance of selecting enough principal components to represent 80%, 90%,
#95%, 99% of the accumulated variance. For each test vary K.

filename = paste("../data/data-",2,"-",1,"-", 100,"-", 1.5,".RData",sep="")
load(filename)
load("../data/testClass.RData")
digit_0=testClass[1:400]
PCA = prcomp(trainingDigit[[1]],scale = TRUE)
#names(PCA)
#head(PCA$sdev)
#head(PCA$rotation)
summary(PCA)
cum_var = cumsum(PCA$sdev^2 / sum(PCA$sdev^2))
cum_var
plot(cum_var, type = 'p', main = "Accumulated variance - digit '0'", xlab = "Number of Components", ylab = "Variance percentage")
abline(v=c(8,12,17,182),col=c(1,2,3,4),lty=5)


for(j in 1:10)    
{    
  if(j == 1)      
  {
    data = trainingDigit[[j]];   
    dataClass = rep(j-1,nrow(trainingDigit[[j]]));
  }    
  else      
  {
    data = rbind(data, trainingDigit[[j]]);
    dataClass = append(dataClass, rep(j-1, nrow(trainingDigit[[j]]) ) );      
  }    
}

dataClassF = factor(dataClass)
##Training
k_list=expand.grid(k=c(1,2))
control = trainControl(method="cv",
                       number = 10,
                       repeats=1,
                       p = 0.9,
                       preProcOptions = list(thresh = 0.8), # SET PCA threshold
                       verboseIter = TRUE
                       )

knnFit = train(x=data, #Training AND testing data (will be split internally, according to "control")
               y=dataClassF, #Correct labels, same length as data.
               method ='knn',
               trControl = control, #Repeated Cross Validation
               tuneGrid = k_list, #method parameter value list. knn has one parameter k. This is a list of k's to test.
               preProcess = c('pca') #Normalization etc. before passing data to k-NN classifier. @todo: Is this necessary?
               )




