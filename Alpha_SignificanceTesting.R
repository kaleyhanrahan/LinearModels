##################################################
### Understanding Alpha - Significance Testing ###
##################################################
library(dplyr)

# Set seed for replicability when generating random numbers
set.seed(7)

### Parameters to play around with
# Set number of iterations for loop (the higher it is, the more clear the effect)
nRep <- 1000
# Set sample size for each regression
nSample <- 100
# Set alpha level (convention is usually 0.05, occasionally 0.01)
alpha <- 0.05
# Set number of predictors
nPredictors <- 4

# Initialize dataframe to hold p-values of each variable for each regression
pValuesTot <- c()

for (i in 1:nRep){
  df <- data.frame(matrix(NA, nrow = nSample, ncol=nPredictors))
  
  # Create predictor variables with random data
  for (i in 1:nPredictors){
    df[[paste('X', i, sep='')]] <- sample(1:(nSample*10), nSample, replace=T)
  }  
  
  # Create response variable 'y' from a normal distribution: mean = 10, variance =25
  df$y <- rnorm(nSample, 10, 5)
  # Run regression on dataframe
  lm <- lm(y~., data=df)
  # Pull the p-values for each predictor
  pValues <- anova(lm)$"Pr(>F)"[1:nPredictors]
  pValuesTot <- rbind(pValuesTot, pValues)
}

# Count the number of variables with p-values found to be significant (based on set p-values)
count <- sum(pValuesTot<=alpha)
# Find the PROPORTION of variables with p-values found to be significant (based on set p-values)
prop <- count/(nRep*nPredictors)

message("Proportion of variables found to be significant: ", prop, "% (n = ", count, ")")

#### As you can see, the proportion of 
