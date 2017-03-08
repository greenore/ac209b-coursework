## Helper functions
#------------------

# Creat Environment
if (any(search() %in% "projEnvironment")) detach("projEnvironment")
projEnvironment <- new.env()

# Scale a vector between zero and one
projEnvironment$scale <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

attach(projEnvironment)
rm(projEnvironment)


