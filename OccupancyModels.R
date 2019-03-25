## Occupancy Models ##

setwd("C:/Users/dnl0009/Desktop/WMAN633/")
y <- read.csv('SOSP.csv')
head(y,15) # sosp.2,15 is still there, just undetected (closure)

# Format data
sosp_mat <- as.matrix(y)
occu_data <- unmarkedFrameOccu(y=sosp_mat)

# Intercept-only model
fit <- occu(~1~1,occu_data)
summary(fit)

# Don't forget plogis when interpreting numbers because they're on a logit scale
# Probability of detecting at least one bird on any one survey (psi=96%)
# Probability of detection (p=21%)

coef(fit)
plogis(coef(fit))

rho <- 1-(1-plogis(coef(fit)[2]))^2; rho # Probability of detection >= 1 at ANY site

phi <- sum(apply(X=sosp_mat,MARGIN=1,FUN=max)); phi

PHI <- phi/rho; PHI

PHI/nrow(sosp_mat) # Probability that any one site is occupied
plogis(coef(fit)[1]) # Pretty close to the modeled probability


# LISTS
ex.list <- list(
  characters = c('a', 'b', 'd'), # Naming frames is very important
  numbers = rnorm(10) # Frames don't have to be the same length in a list
)
ex.list

p_covs <- read.csv('p_covs.csv'); head(p_covs) # Detection covariates

det_covs <- list(
  time = data.frame(p_covs[,c('time.1','time.2')]),
  sky = data.frame(sky.1 = factor(p_covs$sky.1),
                   sky.2 = factor(p_covs$sky.2))
)

# Check work
head(det_covs$time); class(det_covs$time) # Check for data frames
head(det_covs$sky); class(det_covs$sky)
class((det_covs$sky)$sky.1) # Check for factors
class((det_covs$sky)$sky.2)

# Model sky and time
occu_data <- unmarkedFrameOccu(sosp_mat, # detection/non-detection
                               obsCovs=det_covs) # detection covariates
fit <- occu(~time+sky~1,occu_data) # Make sure to keep names consistent from det_covs list
summary(fit)

site_covs <- read.csv('psi_covs.csv')
head(site_covs)

# Ensure factors are listed as factors
site_covs$herb <- factor(site_covs$herb)
site_covs$shrub1 <- factor(site_covs$shrub1)
site_covs$bareground <- factor(site_covs$bareground)
site_covs$shrub5 <- factor(site_covs$shrub5)

# Add site covs
occu_data <- unmarkedFrameOccu(sosp_mat, # detection/non-detection
                               siteCovs=site_covs,  # Site level covariates
                               obsCovs=det_covs # detection covariates
) 
fit <- occu(~time+sky~size+water,occu_data)
summary(fit)

## INTERPRETATION ##
# with every 1-ha increase in wetland size, the log odds of detecting a SOSP increases by 0.02
# Think of Bernoulli interpretation. It's the same.

# Wald test #
w <- 0.0219/0.125;w 
2 * pnorm(-1*abs(w)) # Null hypothesis is the coefficient for wetland size = 0.
# DO NOT REJECT NULL (SE > slope, always reject.. good way to check)

# Plotting predictions
# Process (or state) model
new_psi <- data.frame(size=seq(from=min(site_covs$size),
                               to=max(site_covs$size),length.out=100),
                      water=rep(0,times=100))
predict <- predict(object=fit,newdata=new_psi,type='state') # state for state model
head(predict)

# Detection

new_p <- data.frame(
  time = seq(from=min(det_covs$time),
             to=max(det_covs$time),length.out=100),
  sky=factor(rep('1',times=100),
             levels=c('0','1','2','3'))
)

pred <- predict(object=fit,newdata=new_p,type='det') # det for detection

