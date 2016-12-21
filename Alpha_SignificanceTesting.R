##################################################
### Understanding Alpha - Significance Testing ###
##################################################

numPredictors <- 4
nRep <- 1000
dfSigTot <- c()

for (i in 1:nRep){
  df <- data.frame(matrix(NA, nrow = 100, ncol=numPredictors))
  df$X1 <- seq(1, 200, 2)
  df$X2 <- sample(1:2000, 100, replace=T)
  df$X3 <- sample(1:100, 100, replace=F)
  df$X4 <- sample(1:100, 100, replace=T)
  df$y <- rnorm(100, 10, 5)
  
  hist(df$y, breaks=15)
  
  lm <- lm(y~., data=df)
  
  x1Sig <- anova(lm)$"Pr(>F)"[1]
  x2Sig <- anova(lm)$"Pr(>F)"[2]
  x3Sig <- anova(lm)$"Pr(>F)"[3]
  x4Sig <- anova(lm)$"Pr(>F)"[4]
  
  dfSig <- data.frame(x1Sig, x2Sig, x3Sig, x4Sig)
  
  dfSigTot <- rbind(dfSigTot, dfSig)
}

count <- c(sapply(dfSigTot, function(x) sum(x<=0.05)))
prop <- count/nRep

countTot <- sum(count)
propTot <- countTot / (nRep*numPredictors)
