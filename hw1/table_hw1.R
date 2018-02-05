nVals = seq(100, 500, by=100)
distTypes = c( "t1", "t5", "gaussian")

#create labels
c1 <- 0
c2 <- 0
i <- 1
for (n in nVals) {
  c1[[i]] <- toString(n)
  c1[[i+1]] <- ''
  c2[[i]] <- "PrimeAvg"
  c2[[i+1]] <- "SampAvg"
  i <- i+2
}

#get values
x <- matrix(NA, nrow=10, ncol=3)
i = 1
j = 1
for (type in distTypes) {
  for (n in nVals) {
    oFile = paste(type, "_n", n, ".txt", sep="")
    myVals <- read.table(oFile)
    x[[i]] = myVals[2][[1]][1]
    x[[i+1]] = myVals[2][[1]][2]
    i <- i+2
  }
  j <- j+1
}

#print table
df <- data.frame(c1, c2, x)
colnames(df) <- c("n", "Method", "t1", "t5", "Gaussian")

library(knitr)
kable(df, "markdown")