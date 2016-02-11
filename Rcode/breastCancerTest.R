#install.packages("class") install.packages("gmodels")
library(class)
library(gmodels)
require(caret)
normalize <- function(x) 
{     
  return ((x - min(x)) / (max(x) - min(x)))   
}


wbcd <- read.csv("/home/keerthikan/Dropbox/Eksamen/SML/breastCancer/wdbc.data", header=FALSE, stringsAsFactors = FALSE)
wbcd <- wbcd[sample(nrow(wbcd)),]
features <- c("radius", "texture", "perimeter", "area", "smoothness", 
              "compactness", "concavity", "concave_points", "symmetry",
              "fractal_dimension")
calcs <- c("mean", "se", "worst")
colnames(wbcd) <- c("id","diagnosis",
                    paste0(rep(features, 3), "_", rep(calcs, each=10)))
str(wbcd)
#Remove irrevalent id slot
wbcd <- wbcd[c(2:32)]
str(wbcd)
#show how many of the patients had cancer
table(wbcd$diagnosis)
#change the diagnostic to be the label of the data
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M") , labeles = c("benign", "Malignent"))
#show probability of the diagnostics
round(prop.table(table(wbcd$diagnosis))*100,digits=1)
#analyse input values 
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])
#normalize input values
wbcd_n <- as.data.frame(lapply(wbcd[2:31],normalize))#normalize))
summary(wbcd_n[c("radius_mean","area_mean","smoothness_mean")])
#split data into training and label and test input and label
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
#do knn
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=14)
CrossTable(x = wbcd_test_labels, y= wbcd_test_pred,prop.chisq = FALSE)
#cross validation 
#install.packages("caret")
folds <- createFolds(wbcd_n[,1] , k =10)
str(folds)

