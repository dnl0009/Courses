## N-mixture models #

# What are the factors that lead to varied abundance while accounting for imperfect detection?

setwd("C:/Users/dnl0009/Desktop/WMAN633/")
y <- read.csv('sosp_nmix.csv')
head(y)
library(unmarked)
# two functions for fitting nmix models in unmarked: pcount & unmarkedFramePCount
# pcount(~detprob~localabund,dataufpc)

sosp_mat <- as.matrix(y)
nmix_data <- unmarkedFramePCount(y=sosp_mat)
fit <- pcount(~1~1,data=nmix_data,K=100)
summary(fit)

# Expected abundance would be about 12 birds at each site: exp(2.48) (Inverse log link)
# Probability of detecting any individual at any site: plogis(-2.57) (Inverse logit link)

# This works because it's an intercept-only model, assuming detprob and abundance are constant
e_y <- mean(sosp_mat)
p <- plogis(coef(fit)[2])
N <- exp(coef(fit)[1])
e_y/p; N # NOICE.

# Read in det covs
p_covs <- read.csv('p_covs_nmix.csv')
head(p_covs)

det_covs <- list(
  time = data.frame(p_covs[,c('time.1','time.2')]),
  sky = data.frame(sky.1 = factor(p_covs$sky.1),
                   sky.2 = factor(p_covs$sky.2))
)

nmix_data <- unmarkedFramePCount(y=sosp_mat,
                                 obsCovs=det_covs)
fit <- pcount(~time+sky~1,nmix_data,K=100)
summary(fit)

# Read in site-level covs
site_covs <- read.csv('n_covs_nmix.csv')
head(site_covs)

# Check class of categories
site_covs$herb <- factor(site_covs$herb)
site_covs$shrub1 <- factor(site_covs$shrub1)
site_covs$bareground <- factor(site_covs$bareground)
site_covs$shrub5 <- factor(site_covs$shrub5)

nmix_data <- unmarkedFramePCount(y=sosp_mat, # detection/non-detection
                                 siteCovs=site_covs,  # site covs
                                 obsCovs=det_covs) # det covs
fit <- pcount(~time+sky~size+type,
              data=nmix_data,K=100) # Don't forget K

summary(fit)
# exp <- inverse log (abundance) == Poisson ## LATENT ABUNDANCE; lambda is log-expected count
# plogis <- inverse logit (detection) == Binomial

# Predict
new_lam <- data.frame(size = rep(mean(site_covs$size), times = 2), # hold wetland size at its mean
                      type = factor(c('acep', 'reference'),
                                    levels = c('acep', 'reference'))
)

prd <- predict(object = fit, newdata = new_lam,
               type = 'state') # make sure to specify 'state' type
prd

# Plot site abundance predictions
plot(x = c(0, 0), y = prd[1, c('lower', 'upper')],
     ylim = c(0, prd[1, 'upper']), xlim = c(-0.5, 1.5),
     xaxt = 'n', type = 'l',
     xlab = '', ylab = 'Expected count',
     cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
lines(x = c(1, 1), y = prd[2, c('lower', 'upper')], lwd = 3)
points(x = c(0, 1), y = prd[, 'Predicted'], pch = 16,
       cex = 3)
axis(side = 1, at = c(0, 1),
     labels = c('ACEP', 'Reference'),
     cex.axis = 1.5)
# Nature of link function applied leads to uncertainty 
# (small changes lead to big changes in log function)


# Plot detection predictions
new_p <- data.frame(
  time = rep(mean(c(p_covs[, 'time.1'], p_covs[, 'time.2'])),
             times = 4),
  sky = factor(c('0', '1', '2', '3'),
               levels = c('0', '1', '2', '3'))
)
prd_p <- predict(object = fit, newdata = new_p,
                 type = 'det')
prd_p

plot(x = c(0, 0), y = prd_p[1, c('lower', 'upper')],
     xlim = c(-0.5, 3.5), ylim = c(0, max(prd_p[, 'upper'])),
     ylab = 'Detection probability', xlab = '',
     xaxt = 'n', cex.axis = 1.5, cex.lab = 1.5, type = 'l',
     lwd = 3)
lines(x = c(1, 1), y = prd_p[2, c('lower', 'upper')],
      lwd = 3)
lines(x = c(2, 2), y = prd_p[3, c('lower', 'upper')],
      lwd = 3)
lines(x = c(3, 3), y = prd_p[4, c('lower', 'upper')],
      lwd = 3)
points(x = 0:3, y = prd_p[, 'Predicted'], cex = 3, pch = 16)
axis(side = 1, at = 0:3, cex.axis = 1.5,
     labels = c('clear', 'part cld', 'overcast', 'precip'))

# CONTRASTS

x <- matrix(
  c(0, 0, 1, -1, 0,
    0, 0, 1, 0, -1,
    0, 0, 0, 1, -1),
  nrow = 3, byrow = T
)
x

lin_com <- linearComb(obj=fit,coefficients=x,type='det')
lin_com
w <- coef(lin_com)/SE(lin_com); w
2 * pnorm(-1 * abs(w))
