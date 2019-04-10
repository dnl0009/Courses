##### MODEL CHECKING #####

setwd('C:/Users/dnl0009/Desktop/WMAN633/')
y <- read.csv('SOSP.csv')
site_covs <- read.csv('psi_covs.csv')
p_covs <- read.csv('p_covs.csv')

sosp_mat <- as.matrix(y)
det_covs <- list(
  time = data.frame(p_covs[, c('time.1', 'time.2')]),
  sky = data.frame(sky.1 = factor(p_covs$sky.1),
                   sky.2 = factor(p_covs$sky.2))
)

occu_data <- unmarkedFrameOccu(
  y = sosp_mat, # detection / non-detection
  siteCovs = site_covs, # site-level covs
  obsCovs = det_covs # detection covariates
)
# Step 1: Fit a model
fit <- occu(~ time + sky ~ size + water, occu_data)

# Step 2: Generate a test statistic
# Rota likes sum of squared Pearson residual statistic

# obtaining psi estimates
fitted_psi <- predict(fit, type = 'state')
fitted_psi$Predicted[1]

# obtaining estimates of p_ij
fitted_p <- predict(fit, type = 'det')
fitted_p$Predicted[1]

fitted_psi$Predicted[1] * fitted_p$Predicted[1]
fitted(fit)[1]
# SAME

chsq <- sum((fitted(fit)-sosp_mat)^2 / 
              (fitted(fit)*(1-fitted(fit)))); chsq
# chsq doesn't mean anything unless compared to another
# Similar to AIC values

chisq <- function(mod){
  obs <- getY(mod@data)
  ex <- fitted(mod)
  ts <- (ex-obs)^2/ (ex*(1-ex))
  return(sum(ts))
}
chisq(fit) # SAME as above.. wooooo.

# Step 3: Simulate new data

mu <- coef(fit)
mu

sigma <- vcov(fit)
library(mvtnorm)
# With these slope coefficients, we will simulate data
b <- rmvnorm(n=1,mean=mu,sigma=sigma); b
# Coefficients will be closer or farther apart depending 
# on SE of covariate

psi <- plogis(b[1] + b[2] * site_covs$size 
              + b[3] * site_covs$water)
z <- rbinom(n=length(psi),size=1,prob=psi)
z[1:5]

y_sim <- matrix(nrow = nrow(sosp_mat), ncol = ncol(sosp_mat))
for(i in 1:nrow(y)){ # looping through all sites
  for(j in 1:ncol(y)){ # looping through all replicate surveys
    # detection probability
    p <- plogis(b[4] + b[5] * det_covs$time[i, j] +
                  b[6] * (det_covs$sky[i, j] == "1") +
                  b[7] * (det_covs$sky[i, j] == "2") +
                  b[8] * (det_covs$sky[i, j] == "3"))
    
    # simulated observations
    y[i, j] <- rbinom(n = 1, size = 1, prob = p * z[i])
  }
}

# Step 4: Calculate test statistic on simulated data

simdat <- unmarkedFrameOccu(
  y = y, # simulated detection / non-detection data
  siteCovs = site_covs,
  obsCovs = det_covs
)
simfit <- occu(~ time + sky ~ size + water, simdat)
# calculating test statistic
chisq(simfit)

# parboot = Parametric bootstrapping

sims <- parboot(object=fit,statistic=chisq,nsim=1000)
hist(sims@t.star[,1],xlab='chisq')
lines(x=rep(chisq(fit),2),
      y=c(0,1000),
      col='red',lwd=3)

# Null hypothesis is that our fitted model is the data generated model

sum(sims@t.star[, 1] > chisq(fit)) / 1000
# FAIL TO REJECT