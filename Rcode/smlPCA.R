# smlPCA
# Performs principal component analysis.
# Returns pretty much the same as prcomp,
# and probably works in a similar way.
# Assumes normalization (at least centering) has already been applied.
smlPCA <- function(data,
                   return_rotated_data = TRUE
                   )
{
  # Convert to matrix form so we can decompose
  data = as.matrix(data)
  # Perform SVD of data to get numerically stable decomposition
  decomp = svd(data, nu = 0)
  # Principal components are equal to the right singular values if
  # you first scale by the mean and divide by standard deviation.
  # Scale singular values decomp$d to eigenvalues using number of rows
  if(nrow(data)>2) {
    decomp$d = decomp$d / sqrt(nrow(data)-1)
  }
  # Rename right singular vectors decomp$v (loading vectors)
  # to PC1, PC2, ...
  dimnames(decomp$v) = list(colnames(data),
                            paste0("PC",seq_len(ncol(decomp$v))))
  result = list(
    sdev=decomp$d,
    rotation=decomp$v
    )
  if(return_rotated_data) {
    result$rotated_data = data %*% result$rotation
  }
  class(result)="smlPCA"
  return(result)
}

# Applies an existing PCA rotation to new data.
# Assumes new data is already normalized in the same way
# the original data for the PCA was.
predict.smlPCA <- function(input_smlPCA, newdata, ...) {
  return(as.matrix(newdata) %*% input_smlPCA$rotation)
}

plot.smlPCA <- function(input_smlPCA,
                        max_cumulative_proportion=1,
                        max_principal_components = length(input_smlPCA$sdev),
                        main = deparse(substitute(input_smlPCA)), ...)
{
  variances = input_smlPCA$sdev^2
  variances = variances/sum(variances)
  cumulativeProportion=cumsum(variances)
  maxCumIdx = which(cumulativeProportion >= max_cumulative_proportion)[1]
  if(!is.na(maxCumIdx)) {
    max_principal_components = min(maxCumIdx,max_principal_components)
  }
  cumulativeProportion=100*cumulativeProportion
  selection=seq_len(max_principal_components)
  if(max_principal_components<51) {
    barplot(cumulativeProportion[selection],
            names.arg = names(cumulativeProportion[selection]),
            ylim = c(0,100),
            main=main,
            ylab="Cumulative proportion of total variance [%]",
            xlab = "Number of principal components",
            ...)
  } else {
    plot(selection,
         cumulativeProportion[selection],
         type="b",
         axes=FALSE,
         main=main,
         xlab = "Number of principal components",
         ylab = "Cumulative proportion of total variance [%]",
         ...)
    axis(2)
    axis(1, at = selection, labels = names(cumulativeProportion[selection]))
  }
  invisible()
}

smlPCAtransformTrainingAndTesting <- function(trainingDataOrig, testingDataOrig) {
  myPCA = smlPCA(data = trainingDataOrig,
                 return_rotated_data = TRUE)
  testingDataRotated = predict(input_smlPCA = myPCA, newdata = testingDataOrig)
  result = list(
    trainingDataRotated = myPCA$rotated_data,
    testingDataRotated = testingDataRotated
    )
  return(result)
}
