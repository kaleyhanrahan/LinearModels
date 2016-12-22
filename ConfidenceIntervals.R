############################
### Confidence Intervals ###
############################
library(dplyr)

# Set seed for replicability when generating random numbers
set.seed(7)

### Parameters to play around with
# Set number of iterations for loop (the higher it is, the more clear the effect)
nRep <- 100
# Set sample size for each regression
nSample <- 100
# Set confidence interval percentage (convention is usually 0.95, occasionally 0.99)
confPercent <- 0.95
# Set true beta
beta<-5

# Initialize dataframe to hold p-values of each variable for each regression
yesTot <- c()

for (i in 1:nRep){
  df <- data.frame(matrix(NA, nrow = nSample, ncol=1))
  colnames(df) <- "x"
  # Create predictor variable with random data
  df$x<- sample(1:(nSample*10), nSample, replace=T)
  # Create response variable 'y' from a given underlying relationship 
  df$y <- 10 + beta*df$x + rnorm(nSample, 10, 10)
  # Run regression on dataframe
  lm <- lm(y~., data=df)
  lowerBound <- confint(lm, level=confPercent)[2,1]
  upperBound <- confint(lm, level=confPercent)[2,2]
  # Check if confidence interval bounds the true Beta
  yes <- ifelse(beta >= lowerBound & beta <= upperBound, 1, 0)
  yesTot <- append(yesTot, yes)
}

# Count the number of confidence intervals that contain the true beta (based on set level of confidence)
count <- sum(yesTot)
# Find the PROPORTION of confidence intervals that contain the true beta (based on set level of confidence)
prop <- count/nRep

message("Proportion of Convidene Intervals containing true Beta: ", prop, "% (n = ", count, ")")

##### Discussion of Results #####
