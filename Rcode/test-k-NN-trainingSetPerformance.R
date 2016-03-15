source("knnPerformanceNoCV.R")

DPI_list = c(100,200,300);
k_list = seq(1,40,1);
GroupNumber = 2;
MemberNumber = 2;
sigma_list = c(1.5,2.5,3.5);
n_list = seq(500,4000,500);
DPI_tests <- list()


i=1

DPI=DPI_list[i]
sigma=sigma_list[i]

DataList = loadSinglePersonsData(DPI,GroupNumber,MemberNumber,sigma)
for(j in 1:10)
{    
  if(j == 1)
  {
    data = DataList[[j]];   
    dataClass = rep(j-1,nrow(DataList[[j]]));
  }    
  else      
  {
    data = rbind(data, DataList[[j]]);
    dataClass = append(dataClass, rep(j-1, nrow(DataList[[j]]) ) );      
  }    
}
dataClassF = factor(dataClass)
remove(DataList);
gc();

knnTest=knnPerformanceOnTrainingSetNoCV(data,dataClassF,k_list)

knnTestFilenamePF=paste(c("../data/knnTrainingSetPerformance-",GroupNumber,"-",MemberNumber,"-",DPI,"-",sigma),collapse="")

save(knnTest,file=paste(c(knnTestFilenamePF,".RData"),collapse=""))





load(paste(c(knnTestFilenamePF,".RData"),collapse=""))

acc = vector(mode="double",length=length(k_list))
acclow = vector(mode="double",length=length(k_list))
acchigh = vector(mode="double",length=length(k_list))
for(i in 1:(length(k_list)))
{
  acc[i]=knnTest$knnConfusionMatrix[[i]]$overall[["Accuracy"]]
  acclow[i]=knnTest$knnConfusionMatrix[[i]]$overall[["AccuracyLower"]]
  acchigh[i]=knnTest$knnConfusionMatrix[[i]]$overall[["AccuracyUpper"]]
}
knnTestResults=data.frame(k=k_list,Accuracy=acc,Accuracy95Low=acclow,Accuracy95High=acchigh)

write.csv(x=knnTestResults,file=paste(c(knnTestFilenamePF,".csv"),collapse=""),row.names = FALSE)
