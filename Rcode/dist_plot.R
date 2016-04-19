#Plot distributions


plot.func <- function(distr=c("beta", "binom", "cauchy", "chisq",
                              "exp", "f","gamma", "geom", "hyper",
                              "logis", "lnorm", "nbinom", "norm",
                              "pois", "t", "unif", "weibull"),
                      what=c("pdf", "cdf"), params=list(), type="b", 
                      xlim=c(0, 1), log=FALSE, n=101, add=FALSE, ...) {
  what <- match.arg(what)
  d <- match.fun(paste(switch(what, pdf = "d", cdf = "p"), 
                       distr, sep=""))
  # Define x-values (because we won't use 'curve') as last parameter
  # (with pdf, it should be 'x', while for cdf it is 'q').
  len <- length(params)
  params[[len+1]] <- seq(xlim[1], xlim[2], length=n)
  if (add) lines(params[[len+1]], do.call(d, params), type, ...)
  else plot(params[[len+1]], do.call(d, params), type, ...)
}