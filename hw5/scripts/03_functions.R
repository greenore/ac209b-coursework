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


attach(projEnvironment)
rm(projEnvironment)


