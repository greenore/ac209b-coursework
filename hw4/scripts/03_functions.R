## Helper functions
#------------------

# Creat Environment
if (any(search() %in% "projEnvironment")) detach("projEnvironment")
projEnvironment <- new.env()

#####################
# Provided function #
#####################

# We provide you with an `R` function (`posterior_pA`) to calculate the posterior
# predictive probability under the above model, i.e. the posterior probability
# that a given test article was written by author $A$, based on the training data.
# The input to this function is 
# 
# - the Dirichlet parameter $\alpha$, 
# - the total word counts $(y^A_1, y^A_2, \ldots, y^A_K)$ from all articles by author $A$ in the training set, 
# - the total word counts $(y^B_1, y^B_2, \ldots, y^B_K)$  from all articles by author $B$ in the training set, and
# - the word counts $(\tilde{y}_1, \ldots, \tilde{y}_K)$ for a new test article. 
# 
# The output is the posterior probability $P(A \,|\, data)$ that the test article was written by author $A$.

projEnvironment$posterior_pA = function(alpha, yA=NULL, yB=NULL, y_til=NULL){
  # number of features
  K = length(yA)
  
  # total word counts
  n = sum(y_til)
  nA = sum(yA)
  nB = sum(yB)
  
  # posterior predictive distribution of being class A
  A1 = lfactorial(n) + lfactorial(nA) - lfactorial(n + nA)
  A2 = sum(lfactorial(y_til + yA)) - sum(lfactorial(y_til)) - sum(lfactorial(yA))
  A3 = lfactorial(n + nA) + lgamma(K*alpha) - lgamma(n + nA + K*alpha)
  A4 = sum(lgamma(y_til + yA + alpha) - lfactorial(y_til + yA) - lgamma(alpha))
  A5 = lfactorial(nB) + lgamma(K*alpha) - lgamma(nB + K*alpha)
  A6 = sum(lgamma(yB + alpha) - lfactorial(yB) - lgamma(alpha))
  R_A = exp(A1 + A2 + A3 + A4 + A5 + A6)
  
  # posterior predictive distribution of being class B
  B1 = lfactorial(n) + lfactorial(nB) - lfactorial(n + nB)
  B2 = sum(lfactorial(y_til + yB)) - sum(lfactorial(y_til)) - sum(lfactorial(yB))
  B3 = lfactorial(n + nB) + lgamma(K*alpha) - lgamma(n + nB + K*alpha)
  B4 = sum(lgamma(y_til + yB + alpha) - lfactorial(y_til + yB) - lgamma(alpha))
  B5 = lfactorial(nA) + lgamma(K*alpha) - lgamma(nA + K*alpha)
  B6 = sum(lgamma(yA + alpha) - lfactorial(yA) - lgamma(alpha))
  R_B = exp(B1 + B2 + B3 + B4 + B5 + B6)
  
  # probability of being class A
  pA = R_A/(R_A + R_B)
  return(pA)
}

# This function is an approximation of the above exact calculation of p(A|Data):
#
# 1. Make sure to install the MCMCpack and MGLM packages to use this funciton
#
# 2. It isn't written very efficiently, notice that a new simulation from posterior 
#   is drawn each time. A more efficient implementation would be to instead 
#   simulate the posteriors (post_thetaA, etc.) once and hand them to
#   approx_posterior_pA to calculate the probability.

projEnvironment$approx_posterior_pA = function(alpha=1, yA=NULL, yB=NULL, y_til=NULL, n.sim=NULL){
  # number of features
  K = length(yA)
  alpha0 = rep(alpha, K)
 
  # simulate parameters from the posterior of the Dirichlet-Multinomial model
  post_thetaA = MCmultinomdirichlet(yA, alpha0, mc = n.sim)
  post_thetaB = MCmultinomdirichlet(yB, alpha0, mc = n.sim)
  
  # calculate the likelihood of the observation y_til under simulated posteriors
  # note: ddirm calculates by-row likelihoods for (data, parameter) pairs
  y_til_mat = matrix(rep(y_til, n.sim), nrow=n.sim, byrow=TRUE)
  likeA = exp(ddirm(y_til_mat, post_thetaA))
  likeB = exp(ddirm(y_til_mat, post_thetaB))
  
  # integrate over simulated parameters
  marginal_pA = sum(likeA)
  marginal_pB = sum(likeB)
  
  # calculate probability of A
  pA = marginal_pA/(marginal_pA + marginal_pB)
  
  return(pA)
}

# This function claculates an approximation to E[R_k|data] described above. 
projEnvironment$posterior_mean_R = function(alpha=1, yA=NULL, yB=NULL, n.sim=NULL){
  # number of features
  K = length(yA)
  alpha0 = rep(alpha, K)
  
  # posterior parameter values	
  post_thetaA = MCmultinomdirichlet(yA, alpha0, mc = n.sim)
  post_thetaB = MCmultinomdirichlet(yB, alpha0, mc = n.sim)
  
  # empirical values of R_k
  R = post_thetaA/(post_thetaA + post_thetaB)
  
  # calculate approximation to E[R_k|data]
  ER = apply(R, 2, mean)
  return(ER)
}

#################
# OWN FUNCTIONS #
#################

projEnvironment$dmm_model <- function(alpha, train_a, train_b, test_set) {
  n <- nrow(test_set)
  posterior_prob <- rep(0, n)
  
  for (i in 1:n) {
    posterior_prob[i] <- posterior_pA(alpha, train_a, train_b, test_set[i, ])
  }
  
  return(posterior_prob)
}

# Function to calculate the log-loss
projEnvironment$log_loss <- function(author_identity, posterior_probability){
  n <- length(author_identity)
  log_loss_probability <- 0
  
  # See formula in the main Rmd document for the details
  for (i in 1:n) {
    if (author_identity[i] == 1) {
      log_loss_probability <- log_loss_probability + log(posterior_probability[i])
    } else {
      log_loss_probability <- log_loss_probability + log(1 - posterior_probability[i])
    }
  }
  return(-1/n * log_loss_probability)
}

# Function to compute CV accuracy
projEnvironment$cross_val_fun <- function(data, param_val, k) {

  # Split dataset according to author
  train_A <- data[data$Author == "AaronPressman", ]
  train_B <- data[data$Author == "AlanCrosby", ]
  num_param <- length(param_val) # Number of parameters
  
  # Divide training set into k folds
  folds <- sample(1:k, nrow(train_A), replace=TRUE) 
  cv_log_loss <- rep(0, num_param)
  
  # Loop over parameters
  for(i in 1:num_param){
    for(j in 1:k){
      train_A_total <- colSums(train_A[folds!=j, ][-1])
      train_B_total <- colSums(train_B[folds!=j, ][-1])
      
      # Combine
      val_set <- rbind(train_A[folds == j, ], train_B[folds == j, ])
      posterior_prob <- dmm_model(param_val[i], train_A_total, train_B_total, val_set[-1])
      val_identity <- as.numeric(val_set$Author == "AaronPressman")
      cv_log_loss[i] <- cv_log_loss[i] + log_loss(val_identity, posterior_prob)
    }
    cv_log_loss[i] <- cv_log_loss[i] / k
  }
  
  # Return cross-validated log-loss values
  return(cv_log_loss)
}

# Calculate class probability
projEnvironment$calc_class_probability <- function(train, test, class, alpha) {
  
  # Apply data and class
  y <- lapply(split(train, class),
              function(x) {
                apply(x, 2, sum)
              })
  
  # Apply posterier probabilty 
  post_pA <- apply(test, 1, function(x) {
    posterior_pA(alpha=alpha,
                 yA=y[[1]],
                 yB=y[[2]],
                 y_til=x)
  })
  
  post_pA
}

# Calculate class probability for monte carlo
projEnvironment$calc_class_probability_mc <- function(train, test, class, alpha, n_sim) {
  
  # Apply data and class
  y <- lapply(split(train, class),
              function(x) {
                apply(x, 2, sum)
              })
  
  # Apply posterier probabilty 
  post_pA <- apply(test, 1, function(x) {
    approx_posterior_pA(alpha=alpha,
                        yA=y[[1]],
                        yB=y[[2]],
                        y_til=x,
                        n.sim=n_sim)
  })
  
  post_pA
}

# Log loss function (found on stackoverflow) 
# Source: http://stackoverflow.com/questions/42323432/using-custom-multi-log-loss-function-with-caret-in-r
projEnvironment$MultiLogLoss <- function(act, pred){
  eps <- 1e-15
  pred <- pmin(pmax(pred, eps), 1 - eps)
  sum(act * log(pred) + (1 - act) * log(1 - pred)) * -1 / NROW(act)
}

attach(projEnvironment)
rm(projEnvironment)


