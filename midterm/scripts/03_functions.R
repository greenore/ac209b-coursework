## Helper functions
#------------------

# Creat Environment
if (any(search() %in% "projEnvironment")) detach("projEnvironment")
projEnvironment <- new.env()

#####################
# Provided function #
#####################

projEnvironment$posterior_pA = function(alpha, yA = NULL, yB = NULL, y_til = NULL){
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
  
  # posterior predictive distribution of being class B
  B1 = lfactorial(n) + lfactorial(nB) - lfactorial(n + nB)
  B2 = sum(lfactorial(y_til + yB)) - sum(lfactorial(y_til)) - sum(lfactorial(yB))
  B3 = lfactorial(n + nB) + lgamma(K*alpha) - lgamma(n + nB + K*alpha)
  B4 = sum(lgamma(y_til + yB + alpha) - lfactorial(y_til + yB) - lgamma(alpha))
  B5 = lfactorial(nA) + lgamma(K*alpha) - lgamma(nA + K*alpha)
  B6 = sum(lgamma(yA + alpha) - lfactorial(yA) - lgamma(alpha))
  
  ratio_AB = exp(B1 + B2 + B3 + B4 + B5 + B6 - (A1 + A2 + A3 + A4 + A5 + A6))
  
  # probability of being class A
  pA = 1/(1 + ratio_AB)
  
  return(pA)
}

# This function calculates an approximation to E[R_k|data] described above.
projEnvironment$posterior_mean_R = function(alpha = 1, yA = NULL, yB = NULL, n.sim = NULL){
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

# Visualize splines
projEnvironment$plot_splines <- function(model, term){
  pre_plt <- preplot(model, terms=term)[[1]]
  df <- data.frame(x=pre_plt$x, y=pre_plt$y, se=pre_plt$se.y)
  ggplot(df, aes(x=x, y=y)) +
    geom_line(size=0.9) +
    geom_ribbon(aes(ymin=df$y - df$se, ymax=df$y + df$se),
                alpha=0.2, fill="black") +
    scale_y_continuous(limits=c(min(df$y*4), max(df$y*4))) +
    ylab(label=pre_plt$ylab) +
    xlab(label=pre_plt$xlab) +
    theme_bw()
}

attach(projEnvironment)
rm(projEnvironment)


