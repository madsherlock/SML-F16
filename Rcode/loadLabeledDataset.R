source("loadSinglePersonsData.R")
source("loadMultiplePersonsData.R")
source("listOfDigits2classificationData.R")

loadLabeledDataset <- function(filename) {
  load(filename)
  return(labeled_dataset)
}