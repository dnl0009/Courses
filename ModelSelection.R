## MODEL SELECTION ##

# Seek the best model to represent those factors that are giving rise 
# to the data we observe

# Strike a balance between model fit and precision & how many variables 
# in the model (make it more parsimonious)

## Model selection: select a set of predictor variables and/or probability model
## that best approximates an unknown AND UNKNOWABLE process. 
## NOT THE SAME AS NULL HYPOTHESIS SIGNIFICANCE TEST; no p-values; CI are okay


# Candidate models: distinct models with different predictor variables and/or 
# probability distributions (e.g. neg binom vs. Poisson)

# Candidate models + Information criteria (e.g. AIC) = Model selection #


# AIC = -2LL + 2 * parameters --> measure of model fit
###

y <- c(5,8,11,10,9,10,10,12,8,11)
logl <- sum(dpois(y,lambda=mean(y),log=TRUE)); logl
-2 * logl + (2 * 1) # only one parameter (lambda = y-bar)
fit <- glm(y~1,poisson) # Another way to check AIC
AIC(fit) # Useless by itself; only helpful when comparing models

###

library(unmarked)
setwd("C:/Users/dnl0009/Desktop/WMAN633/")
y <- read.csv('sosp_nmix.csv')
sosp_mat <- as.matrix(y)

p_covs <- read.csv('p_covs_nmix.csv')
det_covs <- list(
  time = data.frame(p_covs[,c('time.1','time.2')]),
  sky = data.frame(sky.1 = factor(p_covs$sky.1),
                   sky.2 = factor(p_covs$sky.2))
)

site_covs <- read.csv('n_covs_nmix (1).csv')
site_covs$shrub1 <- factor(site_covs$shrub1)


nmix_data <- unmarkedFramePCount(y=sosp_mat, # detection/non-detection
                                 siteCovs=site_covs,  # site covs
                                 obsCovs=det_covs) # det covs
fit_1_P <- pcount(~time~shrub1,data=nmix_data,K=100,mixture='P') 
# Don't forget K

summary(fit_1_P)

fit_1_NB <- pcount(~time~shrub1,data=nmix_data,K=100,mixture='NB')
summary(fit_1_NB)

# MORE EFFICIENT WAY! #

install.packages('AICcmodavg')
library(AICcmodavg)
AICc(mod=fit_1_NB,second.order=F)

# Poisson models
fit_1_P <- pcount(~ time ~ shrub1, nmix_data, 100, "P")
fit_2_P <- pcount(~ time ~ type, nmix_data, 100, "P")
fit_3_P <- pcount(~ time ~ shrub1 + type, nmix_data, 100, "P")
fit_4_P <- pcount(~ sky ~ shrub1, nmix_data, 100, "P")
fit_5_P <- pcount(~ sky ~ type, nmix_data, 100, "P")
fit_6_P <- pcount(~ sky ~ shrub1 + type, nmix_data, 100, "P")
fit_7_P <- pcount(~ time + sky ~ shrub1, nmix_data, 100, "P")
fit_8_P <- pcount(~ time + sky ~ type, nmix_data, 100, "P")
fit_9_P <- pcount(~ time + sky ~ shrub1 + type, nmix_data, 100, "P")

# Negative Binomial models
fit_1_NB <- pcount(~ time ~ shrub1, nmix_data, 100, "NB")
fit_2_NB <- pcount(~ time ~ type, nmix_data, 100, "NB")
fit_3_NB <- pcount(~ time ~ shrub1 + type, nmix_data, 100, "NB")
fit_4_NB <- pcount(~ sky ~ shrub1, nmix_data, 100, "NB")
fit_5_NB <- pcount(~ sky ~ type, nmix_data, 100, "NB")
fit_6_NB <- pcount(~ sky ~ shrub1 + type, nmix_data, 100, "NB")
fit_7_NB <- pcount(~ time + sky ~ shrub1, nmix_data, 100, "NB")
fit_8_NB <- pcount(~ time + sky ~ type, nmix_data, 100, "NB")
fit_9_NB <- pcount(~ time + sky ~ shrub1 + type, nmix_data, 100, "NB")

# Create candidate set
cand.set <- list(
  P1 = fit_1_P, P2 = fit_2_P, P3 = fit_3_P, P4 = fit_4_P,
  P5 = fit_5_P, P6 = fit_6_P, P7 = fit_7_P, P8 = fit_8_P,
  P9 = fit_9_P,
  NB1 = fit_1_NB, NB2 = fit_2_NB, NB3 = fit_3_NB, NB4 = fit_4_NB,
  NB5 = fit_5_NB, NB6 = fit_6_NB, NB7 = fit_7_NB, NB8 = fit_8_NB,
  NB9 = fit_9_NB
)

mods <- aictab(cand.set=cand.set,second.ord=FALSE) 
# Second order is false because we don't want AICc
head(mods)

# NB9 is the best
summary(fit_9_NB)


##### MODEL AVERAGING #####

# Averaging wetland type variable
mods$Modnames
bhat <- c(
  coef(fit_9_NB)[3], coef(fit_8_NB)[2], 0, coef(fit_3_NB)[3], # zeros are in place of the models that do not have the variable in it
  coef(fit_6_NB)[3], 0, 0, coef(fit_2_NB)[2], coef(fit_5_NB)[2],
  coef(fit_9_P)[3], coef(fit_3_P)[3], 0, coef(fit_6_P)[3],
  coef(fit_8_P)[2], 0, 0, coef(fit_2_P)[2], coef(fit_5_P)[2]
) # Shrinkage avoids bias.
w <- exp(-0.5 * mods$Delta_AIC)/sum(exp(-0.5 * mods$Delta_AIC))
avg_type <- sum(w*bhat); avg_type

avg_type_2 <- modavgShrink(cand.set = cand.set,
                           parm = 'typereference',
                           second.ord = F,
                           parm.type = 'lambda')

avg_type_2$Mod.avg.beta
avg_type
# SAME! :)

vhat <- c(
  vcov(fit_9_NB)[3, 3], vcov(fit_8_NB)[2, 2], 0,
  vcov(fit_3_NB)[3, 3], vcov(fit_6_NB)[3, 3], 0, 0,
  vcov(fit_2_NB)[2, 2], vcov(fit_5_NB)[2, 2],
  vcov(fit_9_P)[3, 3], vcov(fit_3_P)[3, 3], 0,
  vcov(fit_6_P)[3, 3], vcov(fit_8_P)[2, 2], 0, 0,
  vcov(fit_2_P)[2, 2], vcov(fit_5_P)[2, 2]
)

unc_var <- sum(w*(vhat+(bhat-avg_type)^2))
sqrt(unc_var)
avg_type_2$Uncond.SE

new_dat <- data.frame(
  shrub1 = factor(c('1', '1'), levels = c('1', '2')),
  type = factor(c('acep', 'reference'),
                levels = c('acep', 'reference'))
  
)

# calculating model averaged predictions
avg_prd <- modavgPred(cand.set = cand.set,
                      newdata = new_dat,
                      second.ord = F,
                      parm.type = 'lambda')
avg_prd

plot(x = c(0, 1), y = avg_prd$mod.avg.pred,
     xlim = c(-0.25, 1.25),
     ylim = c(0, max(avg_prd$upper.CL)), pch = 16,
     cex = 3, xaxt = 'n', xlab = '',
     ylab = 'Expected abundance', cex.axis = 1.5,
     cex.lab = 1.5)
lines(x = c(0, 0),
      y = c((avg_prd$lower.CL)[1], (avg_prd$upper.CL)[1]),
      lwd = 3)
lines(x = c(1, 1),
      y = c((avg_prd$lower.CL)[2], (avg_prd$upper.CL)[2]),
      lwd = 3)
axis(side = 1, at = c(0, 1), labels = c('acep', 'ref'),
     cex.axis = 1.5)

# Much more precise differences with model averaging

