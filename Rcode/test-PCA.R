library(stats)
source("loadLabeledDataset.R")
source("parallelize.R")

#Load persondependent
#labeled_dataset=loadLabeledDataset("../data/labeled_datasets/data-labeled-2-2-dpi100-sigma1.5.RData")

#Load fewer
labeled_dataset = loadLabeledDataset("../data/labeled_datasets/data-labeled-1-1-2-1-3-1-4-1-5-1-6-1-7-1-8-1-10-1-11-1-dpi200-sigma2.5.RData")


cl = parallelizeMe()

myPCA=list()

myPCA$center = prcomp(
  x = labeled_dataset$data,
  retx = TRUE,
  center = TRUE,
  scale. = FALSE,
  # tol = sqrt(.Machine$double.eps)
  )
myPCA$centerScale = prcomp(
  x = labeled_dataset$data,
  retx = TRUE,
  center = TRUE,
  scale. = TRUE,
  # tol = sqrt(.Machine$double.eps)
)


#summary(myPCA$noPreProcess)

#screeplot(myPCA$noPreProcess)
#biplot(myPCA$noPreProcess)


unParallelizeMe(cl)

plotCumulativeVarianceProportion <- function(stdDeviations,num_components=length(stdDeviations),titleOfPlot="PCA") {
  variances = stdDeviations^2
  variances = variances/sum(variances)
  cumulativeProportion=100*cumsum(variances)
  selection=seq_len(num_components)
  
  if(num_components<51) {
    barplot(cumulativeProportion[selection],
            names.arg = names(cumulativeProportion[selection]),
            ylim = c(0,100),
            main=titleOfPlot,
            ylab="Cumulative proportion of total variance [%]",
            xlab = "Number of principal components")
  } else {
    plot(selection,
         cumulativeProportion[selection],
         type="b",
         axes=FALSE,
         main=titleOfPlot,
         xlab = "Number of principal components",
         ylab = "Cumulative proportion of total variance [%]")
    axis(2)
    axis(1, at = selection, labels = names(cumulativeProportion[selection]))
  }
  invisible()
}

plotCumulativeVarianceProportion(stdDeviations=myPCA$center$sdev,num_components=150,titleOfPlot = "PCA, dataset \"fewer\", centering, no scaling")






