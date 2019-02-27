# CONTRASTS #
# Can use contrasts for any linear function

setwd("C:/Users/dnl0009/Desktop/WMAN633/")
food <- read.csv("Foraging Data.csv")
fit <- glm(Count~DBH+Habitat,poisson,food)
b <- coef(fit)

# Find the difference 
diff <- b[4] - b[3] # -0.5098

X <- matrix(data=c(0,0,-1,1),nrow=1) # ROW; contrast matrix
Y <- matrix(data=b,ncol=1) # COLUMN; coefficient matrix

X %*% Y # Matrix math: -0.5098; SAME AS BEFORE! :)

sigma <- vcov(fit) # Variance covariance
se <- sqrt(X %*% sigma %*% t(X)) 
se # 0.03792

Wald <- diff/se
Wald

p <- 2 * pnorm(-1 * abs(Wald)); p # YIPPEE, I GOT IT! :D
# Reject the null hypothesis that the DIFFERENCE in log expected counts
# between Rx and wildfires = 0. BECAUSE IT'S A TINY NUMBER.


install.packages('multcomp') # generalized linear hypothesis testing
library(multcomp)

cntr <- glht(model=fit,linfct=X) # Needs to be specified as a matrix
cntr
summary(cntr) # Same output as calculated earlier

bbwo <- read.csv("BBWO.csv",header=T)
fit <- glm(ha~fire_type*years,gaussian,bbwo)
b <- coef(fit)
x2 <- matrix(c(0,1,0,2),nrow=1) # contrasts for different levels of time
x4 <- matrix(c(0,1,0,4),nrow=1)

contr2 <- summary(glht(fit,x2)); contr2 # Don't reject null
contr4 <- summary(glht(fit,x4)); contr4 # Don't reject null; home range is 93 ha smaller than prescribed fire
# Big standard error in contr4 is explained by the small sample size