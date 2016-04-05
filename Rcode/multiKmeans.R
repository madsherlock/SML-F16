# Program Name: multiKmeans.R
# Author: Alberto Negron
# date: 23/06/2013
# Description: Run Kmeans for a different number of clusters and calculates
# the sum of squares by cluster
# Input Params:
#   data: data frame feature variables
#   max.clusters: maximun number of cluster to run kmeans. ie max.cluster=5
#                kmeans will run for 1,2,3,4,5 clusters
#   iter: the maximum number of iterations allowed
# Output:
#   Return list with 2 objects:
#         - data frame with 2 variables: sum of squares and number of clusters
#         - list of kmeans objects
# Run Example: Kmeans.css = multiKmeans(water.treatment.transf,70,100)

multiKmeans = function(data,max.clusters,iter){
  # cluster sum of squares
  css = list()
  km = list()
  
  for(i in seq(2:max.clusters)){
    print(i)
    cl = NULL
    cl = kmeans(data,centers=i,iter.max=iter)
    css[i]=cl$tot.withinss   
    km[[i]]=cl
    
  } 
  df = data.frame(ss=matrix(unlist(css), nrow=max.clusters-1, byrow=T))
  cluster = seq(2,max.clusters)
  df = cbind(df,as.data.frame(cluster))
  return(list(css=df,cluster=km))  
  
}

