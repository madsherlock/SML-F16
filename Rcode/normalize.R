# Normalizes data matrix (eg. training data) so that each column
# has mean 0 and variance 1.
# For raw data, this means each pixel value is normalized according
# to how much it varies in the different images.
normalizeZscore <- function(data) {
  return(scale(data,center=TRUE,scale=TRUE))
}

#Don't apply min-max normalization before PCA, as it needs data to be centered.