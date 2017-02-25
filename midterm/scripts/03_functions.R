## Helper functions
#------------------

# Creat Environment
if (any(search() %in% "projEnvironment")) detach("projEnvironment")
projEnvironment <- new.env()

# Posterior function
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

# Plot map
projEnvironment$plot_map <- function(df_state){
  # Get geo data
  map_states <- map_data("state")
  
  # Prepare states dataset
  df_state2 <- df_state
  df_state2$state_name <- as.character(df_state2$state_name)
  df_state2 <- merge(data.frame("region"=unique(map_states$region)),
                     df_state[, c("state_name", "perc")],
                     by.x="region", by.y="state_name", all.x=TRUE)
  df_state2$perc[is.na(df_state2$perc)] <- 0
  
  # Set keys
  df_states <- data.table(map_states)
  setkey(df_states, region, subregion)
  df_perc <- data.table(df_state2)
  setkey(df_states, region)
  
  # Merge
  df_map <- df_states[df_perc]
  
  # Get state names and locations
  state_names <- aggregate(cbind(long, lat) ~ region, data=data.frame(map_states),
                           FUN=mean)
  
  # Adjustments
  state_names$long[state_names$region == "washington"] <-
    state_names$long[state_names$region == "washington"] + 3
  state_names$long[state_names$region == "north dakota"] <-
    state_names$long[state_names$region == "north dakota"] - 2
  state_names$long[state_names$region == "south dakota"] <-
    state_names$long[state_names$region == "south dakota"] - 2
  state_names$long[state_names$region == "nebraska"] <-
    state_names$long[state_names$region == "nebraska"] - 2
  state_names$long[state_names$region == "minnesota"] <-
    state_names$long[state_names$region == "minnesota"] - 1
  state_names$long[state_names$region == "missouri"] <-
    state_names$long[state_names$region == "missouri"] - 1
  state_names$long[state_names$region == "arizona"] <-
    state_names$long[state_names$region == "arizona"] + 1.5
  state_names$long[state_names$region == "california"] <-
    state_names$long[state_names$region == "california"] + 1
  state_names$long[state_names$region == "nevada"] <-
    state_names$long[state_names$region == "nevada"] - 1
  state_names$long[state_names$region == "kansas"] <-
    state_names$long[state_names$region == "kansas"] - 2
  state_names$long[state_names$region == "montana"] <-
    state_names$long[state_names$region == "montana"] + 2
  
  state_names$lat[state_names$region == "illinois"] <-
    state_names$lat[state_names$region == "illinois"] + 1
  state_names$lat[state_names$region == "south dakota"] <-
    state_names$lat[state_names$region == "south dakota"] + 1
  state_names$lat[state_names$region == "nevada"] <-
    state_names$lat[state_names$region == "nevada"] + 2
  state_names$lat[state_names$region == "montana"] <-
    state_names$lat[state_names$region == "montana"] + 1
  state_names$lat[state_names$region == "idaho"] <-
    state_names$lat[state_names$region == "idaho"] - 2
  state_names$lat[state_names$region == "oregon"] <-
    state_names$lat[state_names$region == "oregon"] - 1
  state_names$lat[state_names$region == "oklahoma"] <-
    state_names$lat[state_names$region == "oklahoma"] + 1
  state_names$lat[state_names$region == "texas"] <-
    state_names$lat[state_names$region == "texas"] + 1.5
  ggplot() + 
    geom_polygon(data=df_map, aes(x=long, y=lat, group=group, fill=perc),
                 colour="white") + 
    coord_map() +
    theme_bw() +
    labs(title="Plot I: Restaurant reviews in the different US states") +
    ylab("Latitude") + 
    xlab("Longitude") +
    scale_fill_continuous(low="#292626", high="darkred", guide="colorbar") +
    geom_text(data=state_names, aes(long, lat, label=region), size=3, color="white")
}


# This function calculates the posterior mean prbabilities
projEnvironment$posterior_mean <- function(alpha=1, y=NULL, n_sim=NULL){
  # number of features
  k <- length(y)
  alpha0 <- rep(alpha, k)
  
  # posterior parameter values  
  post_thetaA <- rdirichlet(n_sim, alpha + y)
  ER_A <- apply(post_thetaA, 2, mean)
  return(ER_A)
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
