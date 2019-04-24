## Practice problems

setwd('C:/Users/dnl0009/Downloads/')
p1 <- read.csv('obs_covs_1.csv')
p2 <- read.csv('obs_covs_2.csv')
site_covs <- read.csv('site_covs.csv')
y <- read.csv('count.csv')
count_mat <- as.matrix(y)

det_covs <- list(
  d1 = p1,
  d2 = p2
)

library(unmarked)
count_dat <- unmarkedFramePCount(y=count_mat,
                                 siteCovs=site_covs,
                                 obsCovs=det_covs)

fit <- pcount(~d1+d2~(x1+x3) * x2,data=count_dat,K=100)
summary(fit)
# Detection is log odds; binomial
# logit link function == bound by 0-1 and expanding it to real number scale
# Interpretation of inverse logit (plogis) is not very meaningful 

# Abundance is log proportional change (log-scale); poisson
# log link function == real number scale shrunk to 0-1
# Intercept can be interpreted as the log expected count at category A 
# when x1 and x3 are fixed at zero.

# Only use 'significant' when looking at null hypothesis significance testing

x1a <- data.frame(x1=seq(min(site_covs$x1),max(site_covs$x1),length.out=100),
                  x2=factor(rep('a',100),levels=c('a','b','c')),
                  x3=rep(0,100)) # close enough to the mean
x1b <- data.frame(x1=seq(min(site_covs$x1),max(site_covs$x1),length.out=100),
                  x2=factor(rep('b',100),levels=c('a','b','c')),
                  x3=rep(0,100))
x1c <- data.frame(x1=seq(min(site_covs$x1),max(site_covs$x1),length.out=100),
                  x2=factor(rep('c',100),levels=c('a','b','c')),
                  x3=rep(0,100))

px1a <- predict(fit,x1a,type='state')
px1b <- predict(fit,x1b,type='state')
px1c <- predict(fit,x1c,type='state') # state is for abundance and occupancy

ggd <- rbind(px1a,px1b,px1c) # combine data frames above
ggd$level <- rep(c('a','b','c'),each=100) # create column for x2 category
ggd$x1 <- rep(x1a$x1,3)

ggplot(data=ggd,aes(x=x1,y=Predicted)) +
  facet_grid(. ~ level) +
  geom_line()