#Parallelization!
library("doParallel")

parallelizeMe <- function(num_threads = 0) {
  nodes <- num_threads
  if(nodes == 0)
    nodes <- detectCores()
  else if(nodes > detectCores())
    nodes <- detectCores()
  cl <- makeCluster(nodes, outfile = "")
  registerDoParallel(cl)
  return(cl)
}

unParallelizeMe <- function(cl) {
  stopCluster(cl)
}