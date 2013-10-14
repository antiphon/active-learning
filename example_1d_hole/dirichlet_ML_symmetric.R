## ML estimates for Dirichlet with single alpha
## using Newton-Raphson gradient descent.

DirSymML <- function(data, a0=NULL, eps=1e-4, prior=c(1,0), maxiter=5e3, verb=FALSE) {
  ## data should be a N x K matrix
  ## N sample size
  ## K dimension of vector.
  N <- nrow(data)
  K <- ncol(data)
  ## each rowsum should be 1.
  ## Be aware that each datarow should have 
  ## positive values, as log(data) will be computed.
  ##
  ## Initialize using method of moments:
  if(is.null(a0)){
    a0 <- mean(DirMaxMoments(data))
  }
  ## bits needed for ML:
  ##
  S <- sum(log(data))
  ## gradient function:
  g <- function(a) N * (K*digamma(K*a) - K*digamma(a) ) + S + (prior[1]-1)/a - prior[2]
  ## second gradient
  q <- function(a) N * (K^2*trigamma(K*a) - K*trigamma(a)) - (prior[1]-1)/a^2 
  
  ## loop: 
  dif <- 2*eps
  i<-0
  while( dif > eps & (nax<-i<maxiter)) {
    g0 <- g(a0)
    q0 <- q(a0)
    a1 <- a0 - g0/q0
    dif <- sum(abs(a0-a1))
    a0 <- a1
    if(verb) cat("\r",i<-i+1, " (dif=", dif,")      ", sep="")
  }
  if(verb) cat("\n", ifelse(nax, paste(i, " steps."), "Didn't converge."), "\n", sep="")
  a1
}


### method-of-moments estimator for alpha
DirMaxMoments <- function(data){
  Ek <- colMeans(data)
  j <- sample(1:ncol(data), 1)
  Ej2 <- mean(data[,j]^2)
  Ek * (Ek[j]-Ej2)/(Ej2-Ek[j]^2)
}

