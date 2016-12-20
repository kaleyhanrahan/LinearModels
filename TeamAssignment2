# STAT 6021
# Team Assignment 2

# Names:
# Kaley Hanrahan (knh9yd)
# Chris Patrick (cpp4f)
# Kevin Sun (kws8de)
# Qi Tang (qt4ur)

###########################
#                         #
#   Team Assignment 2     #
#                         #
###########################

## Please submit one set of answers per team.            ##
## Your answers may be submitted as an annotated R file. ##
###########################################################

library(dplyr)
set.seed(7)
p1 <- read.csv("teamassign02data01.csv")
p2 <- read.csv("teamassign02data02.csv")

#################
## Question 1: ##
#################
# For this problem you will use the data in the file "teamassign02data01.csv" to implement   
# a simple form of the bootstrap resampling method.  Repeat (a) and (b) 1000 times:
#   (a) From the data set, select **with replacement** 100 random pairs (x,y).
#       You will have some repeats -- which is OK and expected.
#   (b) Use your sample to generate a regression equation. Save the values of 
#       hat(beta_0) and hat(beta_1).
#   (c) Find and report a 95% confidence interval for beta_0 and beta_1 by determining
#       the 2.5th and 97.5th percentiles for each set of values.  Do the confidence 
#       intervals contain the true parameter values?

b0all <- c()
b1all <- c()
for (i in 1:1000){
  ### Part a - Select **with replacement** 100 random pairs (x,y)
  sample <- sample_n(p1,100,replace=T)
  
  ### Part b - Use your sample to generate a regression equation
  lm.b <- lm(y~x, data=sample)
  b0 <- as.numeric(coef(lm.b)[1])
  b1 <- as.numeric(coef(lm.b)[2])
  # Save the values of hat(beta_0) and hat(beta_1)
  b0all <- rbind(b0all, b0)
  b1all <- rbind(b1all, b1)
}
### Part c - Find and report a 95% confidence interval for beta_0 and beta_1
quantile(b0all,probs=c(.025,.975)) # 95% CI of B0 = (13.52, 29.82)
quantile(b1all,probs=c(.025,.975)) # 95% CI of B1 = (3.87, 4.27)

true.lm <- lm(y~x, data=p1)
b0_true <- as.numeric(coef(true.lm)[1]) # True B0 = 21.27
b1_true <- as.numeric(coef(true.lm)[2]) # True B1 = 4.07

# The true values for B0 and B1 are contained in the 95% CIs we obtained for B0hat and B1hat


#################
## Question 2: ##
#################
# Import the data set "teamassign02data02.csv" which contains 100 sets of data for 
# the variables x1, x2, ..., x20.  Repeat (a)-(c) 100 times: 
#   (a) Generate 100 y values according to the model y ~ N(10, var=5^2) and pair up 
#       the y-values with corresponding rows from the data set of x-values.
#   (b) On the data set from part (a), generate a multiple regression model with
#       all of the x-values are explanatory variables.
#   (c) Determine the number of significant explanatory variables at the 5% level.
#   (d) Determine and report the proportion of significant variables in the 100
#       simulations. Compare this proportion with the expected theoretical value.

nSig = c()
for (i in 1:100){
  ### Part a - Pairing y-values with corresponding rows from data set 'p2'
  p2$y <- rnorm(100, 10, 5)
  
  ### Part b - Generate multiple regression model with all of the x-values as explanatory variables
  lm.2 <- lm(y~., data=p2)
  
  sig <- data.frame(anova(lm.2)$"Pr(>F)"<=0.05)
  nSig <- rbind(nSig,sum(sig[1:20,]))
}

### Part c - Determine the number of significant explanatory variables at the 5% level
sigVar <- sum(nSig) # Number of significant explanatory vars at the 5% level = 96

### Part d - Determine and report the proportion of significant variables in the 100 simulations
totVar <- 20*100 # Total number of explanatory vars tested for significance = 2000
sigVar/totVar # Proportion of significant vars in the 100 simulations = 0.048

# The proportion of significant vars in the 100 simulations is 0.048 (96 out of 2000).
# Our expected theoretical value is 0.05. Given that we are testing at the 5% level, we
# would expect that 5% should be significant by chance.
