## Helper functions
#------------------

# Creat Environment
if (any(search() %in% "projEnvironment")) detach("projEnvironment")
projEnvironment <- new.env()

# The following function takes a vectorized version of an image
# and plots the image in its original form:
# Rotates 90 degrees (counterclockwise)
projEnvironment$rot90 <- function(x, n = 1){
  r90 <- function(x){
    y <- matrix(rep(NA, prod(dim(x))), nrow = nrow(x))
    for(i in seq_len(nrow(x))) y[, i] <- rev(x[i, ])
    y
  }
  for(i in seq_len(n)) x <- r90(x)
  return(x)
}

# Plots Face given image vector x
projEnvironment$plot.face = function(x,zlim=c(-1,1)) {
  x = pmin(pmax(x,zlim[1]),zlim[2])
  cols = gray.colors(100)[100:1]
  image(rot90(matrix(x,nrow=250)[,250:1],3),col=cols,
        zlim=zlim,axes=FALSE)  
}

# The following code will help you reformat the output of the `agnes` and `diana`
# functions in order to apply the presented methods to find the optimal number of clusters:
projEnvironment$agnes.reformat<-function(x, k){
  # x: Data matrix or frame, k: Number of clusters
  x.agnes = agnes(x,method="ward",stand=T)
  x.cluster = list(cluster=cutree(x.agnes,k=k))
  return(x.cluster)
}

projEnvironment$diana.reformat<-function(x, k){
  # x: Data matrix or frame, k: Number of clusters
  x.diana = diana(x,stand=T)
  x.cluster = list(cluster=cutree(x.diana,k=k))
  return(x.cluster)
}

attach(projEnvironment)
rm(projEnvironment)


