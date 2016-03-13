library("MASS")

toFraction <- function(x) {
  frac = fractions(x);
  if(frac==1)
  {
    return(list(numerator=1,denominator=1))
  }
  fracsplit <- strsplit(attr(frac,"fracs"), "/")[[1]]
  return(list(numerator=as.numeric(fracsplit[1]),denominator=as.numeric(fracsplit[2])))
}