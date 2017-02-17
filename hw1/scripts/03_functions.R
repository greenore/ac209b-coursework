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

# R^2 calculation
projEnvironment$calc_rsq = function(x, predict) {
  predict[is.na(predict)] <- 0
  
  tss = sum((x - mean(x))^2)
  rss = sum((x - predict)^2)
  rsq = max(0, 1 - rss/tss)
  
  return(rsq)
}

# R^2 calculation for splines
projEnvironment$calc_rsq_splines = function(pred, x) {
  tss = sum((x - mean(x))^2)
  rss = sum((x - pred)^2)
  rsq = max(0, 1 - rss/tss)
  
  return(rsq)
}

# 10 fold loess CV function
projEnvironment$loess_10foldcv = function(data){
  data$splits <- cut(sample(1:nrow(data), 
                            nrow(data)), 
                     breaks=10, 
                     labels=FALSE)
  spans <- seq(0.1, 0.8, by=0.1)
  
  perform_10fold <- list()
  for(split in data$splits) {
    train <- data$splits == split
    mt_train <- data[train, ]
    mt_test <- data[-train, ]
    
    train_r2 <- c()
    test_r2 <- c()

    ## Loop over the splits
    for(sp in spans) {
      mod <- loess(PickupCount ~ TimeMin, span=sp, data=mt_train)
      train_r2 <- c(train_r2, calc_rsq(mt_train$PickupCount, predict(mod, newdata=mt_train)))
      test_r2 <- c(test_r2, calc_rsq(mt_test$PickupCount, predict(mod, newdata=mt_test)))
    }
    
    loess_r2 <- data.frame(span = c(spans, spans),
                           set = rep(c("train", "test"), each=length(spans)),
                           r2 = c(train_r2, test_r2))
    perform_10fold <- c(perform_10fold, list(loess_r2))
  }
  
  ## Gather the model statistics
  perform_10fold <- do.call(rbind, perform_10fold)
  
  ## Aggregate accros the statistics
  perform_10fold <- aggregate(perform_10fold$r2, perform_10fold[c("span", "set")], FUN=mean)
  
  perform_10fold
}

# Multiple plot function
# Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
projEnvironment$multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Calculate model performance
projEnvironment$model_performance <- function(model){
  c(train_r2 = calc_rsq(df_train2$ViolentCrimesPerPop, predict(model, newdata=df_train2)),
    test_r2 = calc_rsq(df_test2$ViolentCrimesPerPop, predict(model, newdata=df_test2)))
}

attach(projEnvironment)
rm(projEnvironment)
