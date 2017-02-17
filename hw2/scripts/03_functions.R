## Creat Environment
if (any(search() %in% "projEnvironment")) detach("projEnvironment")
projEnvironment <- new.env()

## Get the class of variables in a dataframe
projEnvironment$getVarClass <- function(data){
  vec <- sapply(data, class)
  df.class <- data.frame(var_name = names(vec), class_long = as.character(vec), 
                         class_short = substr(as.character(vec), 1, 1), stringsAsFactors = FALSE)
  df.class
}

# Function to compute k-fold cross-validation accuracy for a given classification model
projEnvironment$cv_accuracy = function(data, param_val, k) {
  # Number of parameters
  num_param <- length(param_val) 
  cv_acc <- rep(0., num_param)
  
  # Loop over parameter values
  for(i in 1:num_param){
    gam_formula <- as.formula(sprintf("HeartDisease ~ s(Age, spar=%1$f) + Sex + s(RestBP,spar=%1$f) + ExAng + ChestPain + Thal", param_val[i]))
    model <- gam(gam_formula, family = binomial(link = "logit"), data = data)
    acc <- 1 - cv.glm(data, model, K = k)$delta[1]
    cv_acc[i] <- acc
  }
  return(cv_acc)
}

## Colors
projEnvironment$sanCol <- function(col = NULL, alpha = 255){
  df.col <- list()
  df.col$green1 <- rgb(0, 118, 90, alpha = alpha, maxColorValue = 255)
  df.col$green2 <- rgb(91, 172, 38, alpha = alpha, maxColorValue = 255)
  df.col$green3 <- rgb(148, 191, 59, alpha = alpha, maxColorValue = 255)
  df.col$green4 <- rgb(0, 78, 70, alpha = alpha, maxColorValue = 255)
  df.col$green5 <- rgb(0, 116, 117, alpha = alpha, maxColorValue = 255)
  df.col$black <- rgb(0, 0, 0, alpha = alpha, maxColorValue = 255)
  df.col$grey <- rgb(155, 155, 155, alpha = alpha, maxColorValue = 255)
  if (is.null(col)) {
    as.character(unlist(df.col))
  } else {
    df.col[col]
  }
}

attach(projEnvironment)
rm(projEnvironment)
