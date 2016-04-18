## Insert new row to a data.frame having a matrix 
# existingDF = Using data.frame
# newRow = row yout want to add 
# r last filled line. 
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}