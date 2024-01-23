# sample code on calculating significance values by randomization

# use linear regression as an example
# all of this is base R - I'm sure there are more elegant approaches using newer libraries

# create a simulated data set
x <- runif(50,1,25)

# specify slope and residual st for dependent var
int <- 5
slp <- 0.5
rsd <- 4

# generate y vars
y <- int + slp*x + rnorm(length(x),sd = rsd)

plot(y~x)

# calculate linear regression and significance
fit <- lm(y~x)
abline(fit)
summary(fit)

# model outputs can be extracted using coefficients
coefficients(fit)
coefficients(summary(fit))

# extract the p-value
coefficients(summary(fit))[2,4]

# save intercept, slope and significance value
fit.res <- c(coefficients(fit),
             coefficients(summary(fit))[2,4])
      
# first question is whether regression is an unbiased estimator of true coefficients. In other words, if we run the simulation above 1000 times, on average will the intercept and slope of the regression converge on the input variables? To do this, let's wrap up everything above in a function, loaded from accompanying file.
source('simRegression.R')

# try out function
sim.res <- simRegression(x,int=5,slp=0.5,rsd=4,show.plot=T,returny=F)
sim.res

# try it out returning y values
sim.res <- simRegression(x,int=5,slp=0.5,rsd=4,show.plot=T,returny=T)
sim.res[[1]]
sim.res[[2]]

# use replicate function to run this 1000 times (I tried using replicate function, but with a vector as an output it generated a list rather than a simple data frame. Using a loop here is sloppy, but works for this simple problem)

# set parameters again so they can be easily reset at this point in code
N <- 25
x <- runif(N,1,25)
int <- 5
slp <- 0.5
rsd <- 4
Nreps <- 1000

sim.res <- simRegression(x,int=int,slp=slp,rsd=rsd,T)
for (i in 2:Nreps) sim.res <- rbind(sim.res,simRegression(x,int=int,slp=slp,rsd=rsd))

head(sim.res)

# calculate average results over all trials
apply(sim.res,2,mean)

# visualize distribution of pvalues and calculate power
hist(log10(sim.res$pval));abline(v=mean(log10(sim.res$pval)),lwd=2)
abline(v=log10(0.05),col='red',lwd=2)
mod.power <- length(which(sim.res$pval<=0.05))/Nreps
print(mod.power)

# visualize distribution of outcomes
hist(sim.res$mod.int);abline(v=mean(sim.res$true.int),col='red',lwd=2);abline(v=mean(sim.res$mod.int),lwd=2)

hist(sim.res$mod.slp);abline(v=mean(sim.res$true.slp),col='red',lwd=2);abline(v=mean(sim.res$mod.slp),lwd=2)

hist(sim.res$mod.rsd);abline(v=mean(sim.res$true.rsd),col='red',lwd=2);abline(v=mean(sim.res$mod.rsd),lwd=2)

# PART 2 - calculate significance by simulation
# reset parameters
N <- 25
x <- runif(N,1,25)
int <- 5
slp <- 0.5
rsd <- 8 # increase rsd to decrease power

# generate a random data set
rd <- simRegression(x,int,slp,rsd,T,T)
rd[[1]]
(obs.pval <- rd[[1]]$pval)
(obs.slp <- rd[[1]]$mod.slp)
y <- rd[[2]]

# now randomize the y data, and calculate the p value of randomized data. The key here is to choose a test statistics, and determine whether the probability of the test statistic exceeding the observed value. We will use the slope of the regression as the test statistic
yr <- sample(y,length(y),replace=F)
fit <- lm(yr~x)
plot(yr~x);abline(fit)
(sim.slp <- coefficients(fit)[2])

# write function to randomize data and return slope
source('randRegression.R')

# start again with a new data set, and calculate significance by replicating randRegression
# generate a random data set
rd <- simRegression(x,int,slp,rsd,T,T)
rd[[1]]
(obs.pval <- rd[[1]]$pval)
(obs.slp <- rd[[1]]$mod.slp)
y <- rd[[2]]

Nreps <- 1000 # simulate one less than total desired, as the observed data is counted in the vector of outputs - explained below
sim.slps <- replicate(Nreps-1,randRegression(x,y))
sim.slps <- c(obs.slp,sim.slps)
hist(sim.slps)
abline(v=obs.slp)

# where does obs.slp fall in distribution of sim.slps
obs.slp
rank(sim.slps)[1]

#recall pval from regression model
obs.pval

# significance calculation
# two-tailed
length(which(abs(sim.slps)>=abs(obs.slp)))/Nreps

# one tailed, with prior hypothesis of positive slope
length(which(sim.slps>=obs.slp))/Nreps

#one trailed, with prior hypothesis of negative slope
length(which(sim.slps<=obs.slp))/Nreps
