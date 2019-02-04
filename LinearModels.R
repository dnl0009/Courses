### Linear Models ###

m <- 2
x <- seq(from=1,to=5,by=0.5) # can add length.out for how many pts
b <- 4
y <- m * x + b # results as a vector

plot(x=x,y=y,type='l',xlab='x',ylab='y') # line, point, or Both
abline(h=4) # can use lines() as well

lines(x = c(-3, 3), y = c(4, 4), lty = 2)
lines(x = c(0, 0), y = c(-2, 10), lty = 2)

# STOP OVERTHINKING THINGS
yx <- b + m * 719015761
yx1 <- b + m * 719015762
yx1-yx

b0 <- 1
b1 <- 2
b2 <- 3
b3 <- 4
x2 <- 6
x1 <- seq(from=-5, to=5,length.out=100)
y <- b0 + b1 * x1 + b2 * x2 + b3 * x1 * x2
plot(y,type='l',xlab='x1',ylab='y',main='Cray',col="blue")
# the plot is a line instead of a plane because x2 is constant

# change x2 to verify that x1 is dependent on x2
x2a <- 8
ya <- b0 + b1 * x1 + b2 * x2 + b3 * x1 * x2a
lines(ya,lty=2,col="orange")

###############################################
########### LECTURE THREE #####################
###############################################

# lm() is used for linear models

setwd("C:/Users/dnl0009/Desktop/WMAN633/")
setwd(choose.dir()) # Much easier than manually inputting it :)
getwd()
fake_data <- read.csv("fake data.csv",sep=",",header=TRUE)
plot(fake_data$x,fake_data$y,cex.axis=2, cex.lab=2,xlab='x',ylab='y',main='Fake Data Plot')

# Check class of data to predict()
class(fake_data)

# Create fit line to add to the plot
# Response always on left of tilde; predictor(s) on right
fit <- lm(formula= y ~ x, data=fake_data)

# For plotting fit line
x_new <- seq(from=-5,to=5,length.out=100)
y_new <- -0.2018 + 0.6436 * x_new
lines(x=x_new,y=y_new)

#### USING BBWO DATA
setwd("C:/Users/dnl0009/Desktop/WMAN633/")
BBWO <- read.csv("BBWO.csv",header=TRUE)

plot(BBWO$years,BBWO$ha)
fit1 <- lm(ha~wild,BBWO)

# Proving that R is simply using dummy coding when conducting
# linear regression
y_wild <- 242.81 - 76.23 * 1
y_rx <- 242.81 - 76.23 * 0 
y_wild - y_rx # finding the difference (should be the wild slope)

# An easier way to call 
diff <- coef(fit1)[1] + coef(fit1)[2]
coef(fit1)[1] - diff # finding the difference (should be wild slope)

###############################################
########### LECTURE FOUR ######################
###############################################

# Continued from Monday

fit2 <- lm(ha~fire_type,data=BBWO); fit2
lm(ha~fire_type,data=BBWO)
fit3 <- lm(ha~years+fire_type,BBWO) # fitting a model with multiple variables
# is almost just as easy as a single variable

# 152.17 is the number of ha increase per year burned
# -117.608 at year 0 burned when rx burned 
#### Not meaningful because negative

betas <- coef(fit3)
betas[1] + betas[2] * (1:4) + betas[3]

#############

fit_i <- lm(ha~years*fire_type,BBWO); fit_i
betas <- coef(fit_i) # save coefficients in linear model

# BeachIDB and BeachIDC coefficients are the difference of initial 
# beach size from beach A

# Call specific beta coefficients to create the equation
### KEEP IN MIND OF EFFECTIVE SLOPE COEFFICIENT
# all wildfire coefficients disappear when calculating Rx

betas[2] + betas[4] # for wild fire
betas[2] # for Rx fire

############ STATISTICAL INFERENCE #############

# Discrete uniform disribution
# Support is 1-10

y <- sample(x=1:10,size=100000,replace=T)
mean(y)
var(y) # sample variance of vector

# PDF for Gaussian distribution
mu <- 5
v <- 2
y <- 5
1/(sqrt(2*pi*v)) * exp(-(y-mu)^2/(2*v))

mu <- 5
v <- 2
y <- seq(from=0,to=10,length.out=100)
dens <- 1/(sqrt(2*pi*v)) * exp(-(y-mu)^2/(2*v))
plot(dens,type='l') # Looks familiar

# Examples 
setwd("C:/Users/dnl0009/Desktop/WMAN633/")
BBWO <- read.csv("BBWO.csv",header=TRUE)

fit <- lm(ha~years,data=BBWO)
summary(fit)
# Std error is the sqrt of estimated variance

# Fit into beta-1 point estimator to verify

sum((BBWO$years-mean(BBWO$years))*(BBWO$ha-mean(BBWO$ha)))/sum((BBWO$years-mean(BBWO$years))^2)

# Estimated standard error
sqrt((sum((BBWO$ha - (coef(fit)[1] + coef(fit)[2] * BBWO$years)) ^ 2) /
        (nrow(BBWO) - 2)) / sum((BBWO$years - mean(BBWO$years)) ^ 2))

## Visualizing Student's T distribution
y <- seq(from=-5,to=5,length.out=100)
# R has commands for density functions; e.g. dnorm <- normal dist
plot(x=y,y=dt(x=y,df=Inf),type='l',
     ylab='Probability density',
     cex.lab=1.5,cex.axis=1.5)
lines(x=y,y=dt(x=y,df=10),col='red')
lines(x=y,y=dt(x=y,df=3),col='blue')

###### NULL HYPOTHESIS SIGNIFICANCE TESTING #####

# Null in our lm() would be that there is no slope
# Alternative would be that there is an effect
# s{beta-1} is standard error

## p-values ##

pnorm(q=0,mean=0) # q = random variable (provided quantile)
pnorm(q=2,mean=0,sd=1) - pnorm(q=-2,mean=0,sd=1)

## Total area under the curve is 1

1-pnorm(q=1.5,mean=0,sd=1) # area under the curve that is greater than Y = 1.5

# 1.64 is associated with alpha value of 0.10
1-pnorm(1.64,mean=0,sd=1)
y <- seq(from = -3, to = 3, length.out = 100)
plot(x = y, y = dnorm(x = y), type = 'l',
     ylab = 'Probability density',
     cex.axis = 1.5, cex.lab = 1.5)
abline(v = 1.64, lty = 2)


summary(fit)
# Test H0 that B1 = 150
b1 <- coef(fit)[2]
B1 <- 150
sb <- summary(fit)[['coefficients']]['years','Std. Error']
t <- (b1-B1)/sb

## p-value for B1=150 null; alt B1 > 150
# This is only one sided ##

1-pt(q=t,df=45)

# fail to reject null because alpha is greater than 0.1

###### MAXIMUM LIKELIHOOD #######

# Defined as finding parameter values and regression coefficients that 
# correspond to the greatest (relative) probability of observing a sequence 
# of realizations of your random variable.

y <- 0
mu <- seq(-5,5,length.out=100)
ly <- dnorm(x=y,mean=mu)
plot(x=mu,y=ly,type='l',ylab='Likelihood')

# Joint likelihood

y <- c(-0.03, -1.24, -0.69, 1.26, 0.67)
mu <- 0
sigma <- sqrt(1)
prod(dnorm(x = y, mean = mu, sd = sigma))

# Incorporating loops
# Digression

y <- c(-0.03, -1.24, -0.69, 1.26, 0.67)
dy <- numeric(5)
for(i in 1:5){
  dy[i] <- dnorm(y[i], mean=0,sd=1)
}
dy

y <- c(-0.03, -1.24, -0.69, 1.26, 0.67)
mu <- seq(-5,5,length.out=100)
jl <- numeric(100)
for(i in 1:100){
  jl[i] <- prod(dnorm(x=y,mean=mu[i]))
}
plot(x=mu,y=jl,xlab='mu',ylab='likelihood',type='l') 
abline(v=mean(y),lty=3)
# Top of the curve corresponds to the mean of the observed data

# Log likelihood

a <- c(0.52,0.91,0.20)
log(prod(a))
sum(log(a))
# They're the same! :D

y <- c(-0.03, -1.24, -0.69, 1.26, 0.67)
mu <- seq(-5, 5, length.out = 100)
jl <- numeric(100) # initializing empty vector
for(i in 1:100){
  jl[i] <- sum(dnorm(x = y, mean = mu[i], log = T))
}
plot(x = mu, y = jl, xlab = 'mu', ylab = 'log likelihood',
     cex.axis = 1.5, cex.lab = 1.5, type = 'l')
abline(v = mean(y), lty = 2)

# L(??) = f (y1, y2, ..., yn|??); general max likelihood formula
# Theta can be one or many parameters (i.e. mu and sigma^2)

# Writing likelihood functions
# General idea
ab <- function(a,b){
  y <- a + b
  return(y)
}
ab(1,5)


# Applied
logl <- function(params, y){
  sum(dnorm(x = y, mean = params[1], sd = exp(params[2]),log = T))
} # exp(params[2]) ensures sd is always positive
y <- c(-0.03, -1.24, -0.69, 1.26, 0.67)
params <- c(0, 0)
logl(params, y)

# Range of values of sigma

y <- c(-0.03, -1.24, -0.69, 1.26, 0.67)
ll <- numeric(100)
log_sig <- seq(from = -5, to = 2, length.out = 100)
for(i in 1:100){
  ll[i] <- logl(y, c(0, log_sig[i])) #logl is the function above
}
plot(x = exp(log_sig), y = ll, ylab = 'log likelihood',type='l')

# optim = finds point at top of curve; multi-dimensional likelihood surface

y <- c(-0.03, -1.24, -0.69, 1.26, 0.67)
fit <- optim(par = c(0, 0),  # specified parameters; starting value
             fn = logl,      # what function to apply
             y = y,          # y of logl is equal to our observations
             control = list(fnscale = -1)) # tell equation to do max not min

fit
# Output; parameters: mean, variance (neg b/c exponentiated)
# Max likelihood output value

# Check function values
# Mean
fit$par[1]
mean(y)

# Variance
exp(fit$par[2]) # remember variance can't be negative
sqrt(mean((y-mean(y))^2)) # E((Y-E(Y))^2) Definition of variance

# Pretty close

summary(glm(y~1))

## Max likelihood with beach data ##
beach <- read.csv('beach.csv')

logl <- function(params,data){
  y <- params[1] + params[2] * data$Year + 
    params[3] * (data$BeachID =='B') + 
    params[4] * (data$BeachID =='C') +
    params[5] * (data$BeachID == 'B') * data$Year + 
    params[6] * (data$BeachID == 'C') * data$Year
  # params are the actual coefficients in the model
  ll <- numeric(nrow(data))
  for(i in 1:nrow(data)){
    ll[i] <- dnorm(data$OpenBeach[i],mean=y[i],sd=exp(params[7]),
                   log = T)
  }
  return(-1 * sum(ll))
}

beachoptim <- optim(par = c(35000,0,-30000, -30000,0,0,0), fn=logl, method = 'BFGS',data=beach)

