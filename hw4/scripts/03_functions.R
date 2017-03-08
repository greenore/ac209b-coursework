## Helper functions
#------------------

# Creat Environment
if (any(search() %in% "projEnvironment")) detach("projEnvironment")
projEnvironment <- new.env()

# function to scale a column vector between 0 and 1
projEnvironment$scale <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

attach(projEnvironment)
rm(projEnvironment)


