#loadMultiplePersonsData.R
source("loadSinglePersonsData.R")

#DPI=100
#sigma=1.5
#persons = list(c(2,1),c(2,2))

loadMultiplePersonsDataByPerson <- function(DPI,persons,sigma, reload=FALSE){
  idx=1
  allDataByPerson = list()
  for (p in persons) {
    allDataByPerson[[idx]]=loadSinglePersonsData(DPI,p[1],p[2],sigma,reload=reload)
    idx=idx+1
  }
  whatThe=c(allDataByPerson,recursive=TRUE)
  return(allDataByPerson)
}

rearrangeMultiplePersonsDataByDigit <- function(dataByPerson) {
  num_persons = length(dataByPerson)
  dataByDigit = list()
  
  for(digitIdx in 1:10) {
    dataByDigit[[digitIdx]]=list(dataByPerson[[1]][[digitIdx]])
  }
  if(num_persons>1) {
    for(personIdx in 2:num_persons) {
      for(digitIdx in 1:10) {
        dataByDigit[[digitIdx]]=c(dataByDigit[[digitIdx]],list(dataByPerson[[personIdx]][[digitIdx]]))
      }
    }
  }
  
  return(dataByDigit)
}

loadMultiplePersonsDataByDigit <- function(DPI,persons,sigma, reload=FALSE) {
  return(rearrangeMultiplePersonsDataByDigit(loadMultiplePersonsDataByPerson(DPI,persons,sigma,reload)))
}

loadMultiplePersonsDataByDigitMerged <- function(DPI,persons,sigma, reload=FALSE) {
  dat=loadMultiplePersonsDataByDigit(DPI,persons,sigma,reload)
  merged=list()
  num_persons = length(persons)
  # stopifnot(num_persons>1)
  for(digit in 1:10) {
    merged[[digit]]=dat[[digit]][[1]]
  }
  if(num_persons>1) {
    for(digit in 1:10) {
      for(p in 2:num_persons) {
        merged[[digit]]=rbind(merged[[digit]],dat[[digit]][[p]])
      }
    }
  }
  return(merged)
}



