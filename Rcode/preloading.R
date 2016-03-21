#Data preloading
if(require("checkpoint")) {
  setSnapshot("2016-03-15")
} else {
  cat("You are not using Microsoft R Open.\n\r")
}

source("loadMultiplePersonsData.R")
source("listOfDigits2classificationData.R")

#persons_all_loadable: Datasets which can actually be loaded.
persons_all_loadable = list(
  c(1,1),
  c(1,2),
  c(2,1), #Keerthikan
  c(2,2), #Mikael
  c(3,1),
  c(3,2),
  c(4,1),
  c(4,2),
  c(4,3),
  c(5,1),
  c(5,2),
  c(5,3),
  c(6,1),
  # #c(6,2), #Bad file naming
  c(7,1),
  c(8,1),
  c(10,1),
  c(10,2),
  c(11,1),
  c(11,2),
  c(11,3), #Updated his/her Ciphers recently. Should be OK.
  c(13,1)
  # #c(14,1), #Bug X
  # #c(14,2) #Bug X
)

#persons_fewer: Datasets from a large, limited number of people
persons_fewer = list(
  c(1,1),
  c(2,1),
  c(3,1),
  c(4,1),
  c(5,1),
  c(6,1),
  c(7,1),
  c(8,1),
  c(10,1),
  c(11,1)
)

#persons_persondependent: Datasets for which person dependent parameter tuning will be carried out.
#Currently, should hold only ONE element!
persons_persondependent = list(
  c(2,2) #Mikael
)

#When loading datasets, which DPI's and sigmas to use
loading_setups = list(
  all_loadable = list( #Which setups to load when loading data from all people
    list(
      DPI = 100,
      sigma_list = c(
        1,
        1.5
      )
    ),
    list(
      DPI = 200,
      sigma_list = c(
        2.5
      )
    ),
    list(
      DPI = 300,
      sigma_list = c(
        3.5
      )
    )
  ),
  fewer = list(
    list(
      DPI = 100,
      sigma_list = c(
        1,
        1.5
      )
    ),
    list(
      DPI = 200,
      sigma_list = c(
        2.5
      )
    ),
    list(
      DPI = 300,
      sigma_list = c(
        3.5
      )
    )
  ),
  persondependent = list(
    list(
      DPI = 100,
      sigma_list = c(
        0.2,
        0.3,
        0.6,
        1,
        1.5,#
        2.0,
        2.5,
        3,
        3.5
      )
    ),
    list(
      DPI = 200,
      sigma_list = c(
        2,
        2.1,
        2.2,
        2.3,
        2.4,
        2.5,#
        2.6,
        2.7,
        2.8,
        2.9,
        3,
        3.5
      )
    ),
    list(
      DPI = 300,
      sigma_list = c(
        0.2,
        0.3,
        0.6,
        1,
        1.5,#
        2.0,
        2.5,
        3,
        3.5,
        4,
        5
      )
    )
  )
)


preloadData <- function(
  list_of_persons,
  loading_setup,
  dataset_name = paste(unlist(list_of_persons),collapse="-"),
  overwrite_existing_labeled_dataset = FALSE
)
{
  filename_prefix = paste(c("../data/labeled_datasets/data-labeled-",dataset_name),collapse="")
  cat("Trying to preload data with dataset name:\n\r   ", dataset_name,"\n\r")
  cat("   Will attempt following DPI-sigma combinations:\n\r")
  for(DPI_sigma_combination in loading_setup) {
    cat("       DPI =",DPI_sigma_combination$DPI,". sigma ={")
    for(sigma in DPI_sigma_combination$sigma_list) {
      cat(" ",sigma,",",sep="")
    }
    cat("\b }\n\r")
  }
  for(DPI_sigma_combination in loading_setup) {
    cat("   Current DPI =",DPI_sigma_combination$DPI,":\n\r")
    for(sigma in DPI_sigma_combination$sigma_list) {
      cat("   Current sigma = ",sigma,"...",sep="")
      labeled_dataset=list()
      labeled_dataset$list_of_persons = list_of_persons
      labeled_dataset$DPI = DPI_sigma_combination$DPI
      labeled_dataset$sigma = sigma
      data_and_labels =
        listOfDigits2classificationDataAlt(
          dataList =
            loadMultiplePersonsDataByDigitMerged(
              DPI = DPI_sigma_combination$DPI,
              persons = list_of_persons,
              sigma = sigma
            )
        )
      labeled_dataset$data = data_and_labels$data
      labeled_dataset$labels = data_and_labels$dataClassF
      cat("\n\r   Loaded.\n\r      Saving labeled dataset")
      filename = paste(c(filename_prefix,"-dpi",DPI_sigma_combination$DPI,"-sigma",sigma,".RData"),collapse="")
      cat(" to:\n\r     \"",filename,"\"...\n\r",sep="")
      if( ( !file.exists(filename) ) || overwrite_existing_labeled_dataset ) {
        save(
          labeled_dataset,
          file = filename,
          ascii = FALSE,
          compress = "gzip"
        )
      }
      cat("\n\r     Done.\n\r")
    }
  }
  cat("   Dataset:\n\r   ", dataset_name,"\n\r   preloaded.\n\r")
}

preloadEverything <- function(overwrite_existing_labeled_dataset=TRUE) {
  cat("Time: ",format(Sys.time(), "%X"),"\n\rPreload all loadable data:\n\r")
  preloadData(
    list_of_persons = persons_all_loadable,
    loading_setup = loading_setups$all_loadable,
    overwrite_existing_labeled_dataset = overwrite_existing_labeled_dataset
  )
  cat("Time: ",format(Sys.time(), "%X"),"\n\rPreload fewer data:\n\r")
  preloadData(
    list_of_persons = persons_fewer,
    loading_setup = loading_setups$fewer,
    overwrite_existing_labeled_dataset = overwrite_existing_labeled_dataset
  )
  cat("Time: ",format(Sys.time(), "%X"),"\n\rPreload person dependent data:\n\r")
  preloadData(
    list_of_persons = persons_persondependent,
    loading_setup = loading_setups$persondependent,
    overwrite_existing_labeled_dataset = overwrite_existing_labeled_dataset
  )
  cat("Time: ",format(Sys.time(), "%X"),"\n\rDone preloading!\n\r")
}