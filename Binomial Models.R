## Binomial Models ##

# Set the stage # 

setwd("C:/Users/dnl0009/Desktop/WMAN633/")
gcki <- read.csv("GCKI.csv")
head(gcki)
# Flawed dataset because imperfect detection isn't accounted for
# subalpine habitat occurs in higher elevations

# Gaussian is one of many, many, many distributions

# Evaluating different realizations of our random variable Y
dbinom(0,size=3,prob=0.5) # calculates probability density 
dbinom(1,size=3,prob=0.5) # d for distribution
dbinom(2,size=3,prob=0.5)
dbinom(3,size=3,prob=0.5)

# The sum of probability of all possible realizations of Y = 1.00

rbinom(n=5,size=3,prob=0.5) # r for random variables 
# n is how many values; size = N, prob = p

sims <- rbinom(n=100000, size = 3, prob=0.5)
mean(sims); 3 * 0.5 # Calculate mean and compare to estimated E(Y)
var(sims); 3 * 0.5 * (1-0.5)
table(sims) # To prove that when we evaluate our PMF at a certain value, it tells
# the ratio of values?

table(sims)[2]/length(sims) # Number of values that had 1 trial success over total
dbinom(1,3,0.5) # Compare
# With increased iterations, it gets closer to the .375 value

## Bernoulli distribution ##

dbinom(0,1,0.2)
dbinom(1,1,0.2)

rbinom(5,1,0.2)
sims <- rbinom(100000,1,0.2); mean(sims); 0.2 # Bernoulli E(Y) = p

y <- c(1,0,1,1,1,1,0,1,0)
blik <- function(p,y){
  sum(dbinom(y,size=1,prob=p,log=T)) # Size always = 1 for Bernoulli
} # Don't forget the d in DBINOM

p <- seq(0,1,length.out=100)
ll <- numeric(100)
for(i in 1:100){
  ll[i] <- blik(p[i],y)
}

plot(x=p,y=ll,ylab='log likelihood',xlab='p',type='l')
abline(v=mean(y),lty=2)

log(0.5 / (1-0.5)) # 0

(exp(0) / (1 + exp(0))) # 0.5

(exp(1) / (1 + exp(1)))
plogis(1) # Calculating tail area probabilities # Logistic distribution
# Cumulative distribution/density function 

x <- seq(-5, 5, length.out = 100)
p <- plogis(x)
plot(y = p, x = x, type = 'l', cex.axis = 1.5, cex.lab = 1.5)

## Fitting generalized linear models in R ##

# Intercept-only or null model (no regression coefficients)
fit <- glm(y~1,family=binomial,data=gcki)
summary(fit)

#Intercept represents the log odds 
# What is the probability of a GCKI being at a point count?
plogis(-1.24575)

# Test the value

mean(gcki$y)

fit <- glm(y~latitude+longitude,binomial,gcki)
summary(fit)


# Verify that coefficients are the log odds ratios

betas <- coef(fit)
betas

p_45 <- plogis(betas[1] + betas[2] * 45 + betas[3] * -114); p_45

p_46 <- plogis(betas[1] + betas[2] * 46 + betas[3] * -114); p_46

# 0.33 is not the probability of increase

log((p_46 / (1 - p_46)) / (p_45 / (1 - p_45))); betas[2] # IT'S THE SAME.

# Add subalpine categorical variable

fit <- glm(y~latitude+longitude+subalp,binomial,gcki)
summary(fit)
# subalp coefficient is the log odds ratio between non-sub and subalp habitat
# OR the difference of change in log odds between non-sub and subalp habitat

# Verify

betas <- coef(fit)
betas
non <- plogis(betas[1]+ betas[2] * 45 + betas[3] * -114) # x = 0
sub <- plogis(betas[1]+ betas[2] * 45 + betas[3] * -114 + betas[4]) # x = 1
log((sub / (1 - sub)) / (non / (1 - non))) # x + 1 / x 
# Remember this is the LOG odds ratio
betas[4] # Boom, got it.

# Latitude
betas[2] / summary(fit)[['coefficients']]['latitude', 'Std. Error']
summary(fit)[['coefficients']]['latitude', 'z value'] # SAME! :D

# Longitude
# Remember theta-hat - theta / SE(theta-hat); theta in this case = 0
betas[3] / summary(fit)[['coefficients']]['longitude','Std. Error']
ts <- summary(fit)[['coefficients']]['longitude','z value'] # SAME! :D

# P-value
2 * pnorm(-1 * abs(ts),0,1)

lat <- seq(from = min(gcki$latitude), to = max(gcki$latitude),length.out = 100)
y <- betas[1] + betas[2] * lat + betas[3] * mean(gcki$longitude) + betas[4]
plot(x = lat, y = plogis(y), ylab = 'Probability of GCKI presence', xlab = 'latitude', cex.axis = 1.5, cex.lab = 1.5, type = 'l',ylim=c(0,1))

install.packages('mvtnorm')
library(mvtnorm)


## Multivariate Gaussian Distribution ##

# k = 3
mu <- c(0,0,0)
sigma <- diag(3)
sigma

rmvnorm(n=1,mean=mu,sigma=sigma)
rmvnorm(n=5,mean=mu,sigma=sigma)
y <- rmvnorm(n=10000,mean=mu,sigma=sigma)
apply(X=y,MARGIN=2,FUN=mean)
cov(y)

fit <- glm(y~latitude+longitude+subalp,binomial,gcki)
betas <- coef(fit)
coef(fit) # full model with subalp
vcov(fit) # variance covariance
# parameters covary

sqrt(diag(vcov(fit))) # Square root of variance is standard deviation
summary(fit)[['coefficients']][,'Std. Error'] # SAME! :)

# Design matrix # Each of these predictions are Gaussian random variables
x <- matrix(c(1, gcki[1, 'latitude'], gcki[1, 'longitude'],gcki[1, 'subalp']), nrow = 1)
x # the parameter associated with the intercept is the first 1
x %*% coef(fit) 
coef(fit)[1] + coef(fit)[2] * x[2] + coef(fit)[3] * x[3] + coef(fit)[4] * x[4]


# 90% of the time, this range will include theta
# (1-0.05)/2 = .975 # Divided by 2 because it's a two-tailed test
qnorm(0.05)
pnorm(-1.644854) # Verify
# Probability x Multiply by Standard Error to account for fatter tails 

qnorm(0.025) # 95% confidence

lat <- seq(from = min(gcki$latitude), to = max(gcki$latitude),length.out = 100)
lon <- rep(mean(gcki$longitude), times = 100)
sub <- rep(1, times = 100)
y <- betas[1] + betas[2] * lat + betas[3] * lon + betas[4] * sub

X <- cbind(rep(1,100),lat,lon,sub)
y_mat <- (X %*% betas)[,1]
y[1:5]
y_mat[1:5] # Same

var_y <- X %*% vcov(fit) %*% t(X) # transpose
var_y[1:5,1:5]
se_y <- sqrt(diag(var_y))
se_y[1:5,1:5]

low <- y - qnorm(0.975) * se_y # qnorm = quantile of the width of the Confidence interval
# 0.975 comes from 1 - (alpha value wanted for CI width / 2 (for one tail))
low[1:5] # low value

y[1:5] # actual

high <- y + qnorm(0.975) * se_y
high[1:5] # high value

# Predict.glm requires a new data frame
newdat <- data.frame(
  latitude = lat,
  longitude = lon,
  subalp = sub
)
head(newdat)

# Apply function
y_pred <- predict.glm(fit,newdat,'link',TRUE) # need log odds scale, TRUE = est SE
y_pred[1:5]
y[1:5] # Same

y_low <- y_pred$fit - qnorm(0.975) * y_pred$se.fit
y_low[1:5]

y_high <- y_pred$fit + qnorm(0.975) * y_pred$se.fit
y_high[1:5]

# Plot CI

plot(y = plogis(y_pred$fit), x = newdat$latitude,
     ylab = 'Probability of GCKI Occurrence',
     xlab = 'latitude', cex.axis = 1.5, cex.lab = 1.5,
     type = 'l', ylim = c(min(plogis(y_low)),
                          max(plogis(y_high))))
lines(y = plogis(y_low), x = newdat$latitude, lty = 2)
lines(y = plogis(y_high), x = newdat$latitude, lty = 2)
# The confidence intervals get narrower closer to the mean b/c
# there is more information the closer you get to the mean
# with more extrapolation the farther you get

### Change over longitudinal gradient ##
# All of this information is done with predict.glm function
# Just give these values to the newdata data frame
lon <- seq(from=min(gcki$longitude), to=max(gcki$longitude),length.out=100) # lon changes
lat <- rep(mean(gcki$latitude,100)) # constant
sub <- rep(0,100) # not in subalpine habitat (constant)
ylon <- betas[1] + betas[2] * lat + betas[3] * lon + betas[4] * sub

newdat <- data.frame( # This is used for creating a new data set with above given params
  latitude = lat,
  longitude = lon,
  subalp = sub
)
head(newdat) # Report new data
y_pred <- predict.glm(fit,newdat,'link',TRUE) # need log odds scale, TRUE = estimate SE
# Need SE for plotting confidence intervals
# y-pred is plotting the estimated fit line (similar to plotting the best fit model); 
# This y-pred is based on actual data but NOT equal to actual data, hence PREDICT.glm
y_high<- y_pred$fit + qnorm(0.975)*y_pred$se.fit
y_low <- y_pred$fit - qnorm(0.975)*y_pred$se.fit
plot(y=plogis(y_pred$fit),x=newdat$longitude,type='l',ylim=c(min(plogis(y_low)),max(plogis(y_high))))
lines(y=plogis(y_low),x=newdat$longitude,lty=3,col='red')
lines(y=plogis(y_high),x=newdat$longitude,lty=3,col='green') # don't forget plogis

####### POISSON MODELS #######


lambda <- 2
dpois(0,lambda) # probability of observing that quantity
dpois(1,lambda)
dpois(2,lambda) # These don't add up to 1 because this isn't the full support
dpois(4,lambda)
dpois(100,lambda)

rpois(n=5,lambda) # simulate random data
sims <- rpois(n=100,lambda)
mean(sims); lambda; var(sims)
table(sims)/length(sims)
dpois(0:10,lambda)

###
y <- c(3, 0, 6, 0, 2, 2, 2, 2, 2, 1)
plik <- function(lambda,y){
  sum(dpois(y,lambda=lambda,log=T))
}

lam <- seq(from=0,to=6,length.out=100)
ll <- numeric(100)
for(i in 1:100){
  ll[i] <- plik(lam[i],y)
}

plot(x=lam,y=ll,type='l') # How to plot -Inf to Inf?

###

x <- seq(-5,2,length.out=100)
lambda <- exp(x)
plot(y=lambda,x=x,type='l') 
# This is 1:1 as any number on the real number line has a corresponding 
# POSITIVE value

bird <- read.csv("Foraging Data.csv")
head(bird)

fit <- glm(Count~1,family=poisson,data=bird) # Intercept-only model
summary(fit)
exp(0.005722) # On average, each woodpecker got this many prey during a foraging survey
mean(bird$Count) # Makes sense that these are the same in the intercept-only model

fit <- glm(Count ~ DBH, poisson, bird)
summary(fit)
betas <- coef(fit)
betas
c_10 <- betas[1] + betas[2] * 10
c_10
c_11 <- betas[1] + betas[2] * 11
c_11

exp(c_10)
exp(c_11)
log(exp(c_11)/exp(c_10)) # Log proportional change = regression coefficient; WOO!

exp(c_11)/exp(c_10)
exp(betas[2]) # SAME.

fit <- glm(Count~DBH+Habitat,poisson,bird)
summary(fit)

# How to interpret the wildfire coefficient
betas <- coef(fit)
# Mountain Pine Beetle
mpb <- betas[1] + betas[2]
# Rx
rx <- betas[1] + betas[2] + betas[3] # Doesn't really matter at this point
# Wildfire
wild <- betas[1] + betas[2] + betas[4]

log(exp(wild)/exp(mpb)) # Calculate
betas[4] # Compare
# OR
exp(wild)/exp(mpb) # Calculate # Subtract this value from 1 if you want percent change
exp(betas[4]) # Compare

# Wald test

summary(fit)[['coefficients']]['DBH','z value']
w <- betas[2] / summary(fit)[['coefficients']]['DBH','Std. Error']
w

# P values
summary(fit)[['coefficients']]['DBH','Pr(>|z|)']
2*pnorm(-1*abs(w)) # Reject null hypothesis; DBH DOES influence prey

# CI; same as logistic regression (binomial)

newdat <- data.frame( # This is used for creating a new data set with above given params
  DBH = seq(from=min(bird$DBH), to=max(bird$DBH),length.out=100),
  Habitat = rep('Rx',100) # explicitly call it a factor
)
head(newdat) # Report new data

y_pred <- predict.glm(fit,newdat,'link',TRUE) # need log odds scale, TRUE = estimate SE
# Need SE for plotting confidence intervals
# y-pred is plotting the estimated fit line (similar to plotting the best fit model); 
# This y-pred is based on actual data but NOT equal to actual data, hence PREDICT.glm
y_high<- y_pred$fit + qnorm(0.975)*y_pred$se.fit
y_low <- y_pred$fit - qnorm(0.975)*y_pred$se.fit
plot(y=exp(y_pred$fit),x=newdat$DBH,type='l',ylim=c(min(exp(y_low)),max(exp(y_high))))
lines(y=exp(y_low),x=newdat$DBH,lty=3,col='red')
lines(y=exp(y_high),x=newdat$DBH,lty=3,col='green') # don't forget plogis
abline(v=mean(newdat$DBH),lty=1,col='blue')
# EXP INSTEAD OF PLOGIS IN POISSON REGRESSION -- p(LOGIS) is for LOGIStic regression