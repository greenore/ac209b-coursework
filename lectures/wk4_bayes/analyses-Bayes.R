# analyses for Bayesian notes

x = (0:100)/100

# uniform prior
postscript(file="uniform-prior.ps",hori=T)
plot(x, dunif(x), type='l', lwd=3, col="red",
main=expression(paste("Uniform prior distribution for ",theta)),
xlab=expression(theta),
ylab=expression(p(theta)))

# likelihood
postscript(file="likelihood-bern.ps",hori=T)
plot(x, x*(1-x)^4, type='l', lwd=3, col="red",
main="Likelihood function for 1 death, 4 survivals",
xlab=expression(theta),
ylab=expression(paste(L(theta* "|" *y[1],y[2],y[3],y[4],y[5]))))

# posterior
postscript(file="beta.ps",hori=T)
plot(x, dbeta(x,2,5), type='l', lwd=3, col="red",
main="Be(2,5)",
xlab=expression(theta),
ylab=expression(p(theta*"|"*y)))

# posterior with intervals
postscript(file='beta95.ps',hori=T)
plot(x, dbeta(x,2,5), type='l', lwd=3, col="red",
main="Posterior distribution, with 95% intervals",
xlab=expression(theta),
ylab=expression(p(theta*"|"*y)))
x025 = qbeta(0.025, 2, 5)
x975 = qbeta(0.975, 2, 5)
segments(x025, 0, x025, dbeta(x025,2,5), lty=5, lwd=3, col="blue")
segments(x975, 0, x975, dbeta(x975,2,5), lty=5, lwd=3, col="blue")
fr025 = 0.011
fr975 = 0.701
segments(fr025, 0, fr025, dbeta(fr025,2,5), lty=5, lwd=3, col="darkgreen")
segments(fr975, 0, fr975, dbeta(fr975,2,5), lty=5, lwd=3, col="darkgreen")
legend(0.6,2.0, legend=c("95% central posterior interval",
                         "95% confidence interval"),
  col=c("blue","darkgreen"),lty=c(5,5),lwd=c(3,3))

postscript(file="theta-seq.ps",hor=T)
par(mfrow=c(2,3))
plot(x, dbeta(x,1,1), type='l', lwd=3, col="red",
main=expression(paste("Uniform prior distribution for ",theta)),
xlab=expression(theta),
ylab=expression(paste(p(theta))))
plot(x, dbeta(x,1,2), type='l', lwd=3, col="red",
main=expression(y[1]==0),
xlab=expression(theta),
ylab=expression(paste(p(theta* "|" *y[1]))))
plot(x, dbeta(x,1,3), type='l', lwd=3, col="red",
main=expression(paste(y[1]==0,", ",y[2]==0)),
xlab=expression(theta),
ylab=expression(paste(p(theta* "|" *y[1],y[2]))))
plot(x, dbeta(x,1,4), type='l', lwd=3, col="red",
main=expression(paste(y[1]==0,", ",y[2]==0,", ",y[3]==0)),
xlab=expression(theta),
ylab=expression(paste(p(theta* "|" *y[1],y[2],y[3]))))
plot(x, dbeta(x,2,4), type='l', lwd=3, col="red",
main=expression(paste(y[1]==0,", ",y[2]==0,", ",y[3]==0,
", ",y[4]==1)),
xlab=expression(theta),
ylab=expression(paste(p(theta* "|" *y[1],y[2],y[3],y[4]))))
plot(x, dbeta(x,2,5), type='l', lwd=3, col="red",
main=expression(paste(y[1]==0,", ",y[2]==0,", ",y[3]==0,
", ",y[4]==1,", ",y[5]==0)),
xlab=expression(theta),
ylab=expression(paste(p(theta* "|" *y[1],y[2],y[3],y[4],y[5]))))

# next plot didn't make it into the lecture notes!

y0 = dbeta(x,1,1)
y1 = dbeta(x,1,2)
y2 = dbeta(x,1,3)
y3 = dbeta(x,2,3)
y4 = dbeta(x,2,4)
y5 = dbeta(x,2,5)
yylim=range(c(y0,y1,y2,y3,y4,y5))
postscript(file='theta-seq2.ps',hori=T)
par(mfrow=c(2,3))

plot(x,y0,type='n',
main="Sequential updating", xlab=expression(theta),ylim=yylim,
ylab=expression(p(theta)))
lines(x,y0,lwd=3,col="green")

plot(x,y0,type='n',
main="Sequential updating", xlab=expression(theta),ylim=yylim,
ylab=expression(p(theta)))
lines(x,y0,lwd=3,col="green")
lines(x,y1,lwd=3,col="purple")

plot(x,y0,type='n',
main="Sequential updating", xlab=expression(theta),ylim=yylim,
ylab=expression(p(theta)))
lines(x,y0,lwd=3,col="green")
lines(x,y1,lwd=3,col="purple")
lines(x,y2,lwd=3,col="orange")

plot(x,y0,type='n',
main="Sequential updating", xlab=expression(theta),ylim=yylim,
ylab=expression(p(theta)))
lines(x,y0,lwd=3,col="green")
lines(x,y1,lwd=3,col="purple")
lines(x,y2,lwd=3,col="orange")
lines(x,y3,lwd=3,col="seagreen")

plot(x,y0,type='n',
main="Sequential updating", xlab=expression(theta),ylim=yylim,
ylab=expression(p(theta)))
lines(x,y0,lwd=3,col="green")
lines(x,y1,lwd=3,col="purple")
lines(x,y2,lwd=3,col="orange")
lines(x,y3,lwd=3,col="seagreen")
lines(x,y4,lwd=3,col="darkturquoise")

plot(x,y0,type='n',
main="Sequential updating", xlab=expression(theta),ylim=yylim,
ylab=expression(p(theta)))
lines(x,y0,lwd=3,col="green")
lines(x,y1,lwd=3,col="purple")
lines(x,y2,lwd=3,col="orange")
lines(x,y3,lwd=3,col="seagreen")
lines(x,y4,lwd=3,col="darkturquoise")
lines(x,y5,lwd=3,col="red")

# simulate from Be(2,5)
theta = rbeta(10000, 2, 5)  # generate 10,000 values from Be(2,5)
head(theta)  # first six simulated values
mean(theta)  # sample mean
quantile(theta,probs=c(0.025,0.975))  # 2.5% and 97.5% of empirical distn

# simulate y from posterior predictive distribution
theta = rbeta(10000, 2, 5)  # generate 10,000 values from Be(2,5)
head(theta)
y = rbinom(10000, 1, theta) # generate 10,000 binary values
                            # with different probabilities
head(y)
table(y)/10000   # frequency of 0 and 1

require(lattice)

postscript(file="reaction-xy.ps",hori=T)
xyplot(Reaction ~ Days | Subject, sleepstudy, type = c("g","p","r"),
index = function(x,y) coef(lm(y ~ x))[1],
main="Sleep deprivation - reaction times with least-squares fits",
col="red",
pch=19,
xlab = "Days of sleep deprivation",
ylab = "Average reaction time (ms)", aspect = "xy")

library(MCMCpack)
sleep.mcmc = MCMChregress(fixed = Reaction ~ Days,
  random = ~Days,
  group = "Subject",
  data=sleepstudy,
  burnin = 5000,
  mcmc = 10000,
  thin = 1,
  verbose = 1,
  beta.start = NA,
  sigma2.start = NA,
  Vb.start = NA,
  mubeta = c(250,10),
  Vbeta = 10000, # 1.0E6,
  r = 3,
  R = diag(c(1, 0.1)),
  nu = 0.001,
  delta = 0.001)

summary(sleep.mcmc$mcmc)

library(latticeExtra)
mysleep = sleepstudy
mysleep$Reaction.pred = sleep.mcmc$Y.pred

postscript(file="reaction-pred-obs.ps",hori=T)
plot(mysleep$Reaction,sleep.mcmc$Y.pred,pch=19,
main = "Observed and fitted reaction times
from Bayesian hierarchical linear model",
xlab = "Observed reaction time (ms)",
ylab = "Predicted reaction time (ms)" )
abline(0,1,lwd=2, col="blue")

# add predicted Y to xyplot
postscript(file="reaction-xy-1.ps",hori=T)
xyplot(Reaction ~ Days | Subject, 
   mysleep, type = c("g","p","r"),
index = function(x,y) coef(lm(y ~ x))[1],
col="red",
lwd=2,
pch=19,
subscript=T,
main = "Sleep deprivation - pooling data across all subjects",
xlab = "Days of sleep deprivation",
ylab = "Average reaction time (ms)", aspect = "xy")  +
xyplot(Reaction~Days, 
  index = function(x,y) coef(lm(y ~ x))[1],
  type="r", aspect="xy",
  col="black",lwd=2,
  data=mysleep)

# add predicted Y to xyplot
postscript(file="reaction-xy-2.ps",hori=T)
xyplot(Reaction ~ Days | Subject, 
   mysleep, type = c("g","p","r"),
index = function(x,y) coef(lm(y ~ x))[1],
col="red",
lwd=2,
pch=19,
subscript=T,
main = "Sleep deprivation - hierarchical model fit",
xlab = "Days of sleep deprivation",
ylab = "Average reaction time (ms)", aspect = "xy")  +
xyplot(Reaction.pred~Days | Subject,
  index = function(x,y) coef(lm(y ~ x))[1],
  type="r", aspect="xy",
  col='darkgreen',lwd=2, data=mysleep) +
xyplot(Reaction~Days, 
  index = function(x,y) coef(lm(y ~ x))[1],
  type="r", aspect="xy",
  col="black",lwd=2,
  data=mysleep)
