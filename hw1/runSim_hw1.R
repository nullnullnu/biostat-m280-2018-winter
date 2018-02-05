## parsing command arguments
args <- commandArgs(TRUE)
#options
seed <- eval(parse(text=args[[1]]))
n <- eval(parse(text=args[[2]]))
dist <- args[[3]]
rep <- eval(parse(text=args[[4]]))

set.seed(seed)

## check if a given integer is prime
isPrime = function(n) {
  if (n <= 3) {
    return (TRUE)
  }
  if (any((n %% 2:floor(sqrt(n))) == 0)) {
    return (FALSE)
  }
  return (TRUE)
}

## estimate mean only using observation with prime indices
estMeanPrimes = function (x) {
  n = length(x)
  ind = sapply(1:n, isPrime)
  return (mean(x[ind]))
}

classicalEstimator<-list(rep)
primesEstimator<-list(rep)

for (i in 1:rep ){
  # simulate data
  x = switch(dist,
             gaussian = rnorm(n),
             t1 = rt(n, 1),
             t5 = rt(n, 5))
  # estimate mean
  classicalEstimator[[i]] <- mean(x)[1]
  primesEstimator[[i]] <- estMeanPrimes(x)[1]
}

#calculate MSEs
trueMean <-0

classicalMSE <- 0
for (i in 1:rep) {
   classicalMSE <- classicalMSE + (classicalEstimator[[i]] - trueMean)^2
}
classicalMSE <- classicalMSE / rep

primesMSE <- 0
for (i in 1:rep) {
  primesMSE <- primesMSE + (primesEstimator[[i]] - trueMean)^2
}
primesMSE <- primesMSE / rep

#outpus values
print(primesMSE)
print(classicalMSE)
