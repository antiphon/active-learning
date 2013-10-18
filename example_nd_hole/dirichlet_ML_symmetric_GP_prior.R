## ML estimates for Dirichlet with single alpha
## using Newton-Raphson gradient descent.

## covariance function: the good old matern
source("matern.R")

DirGPSymML <- function(X, h, a0=0.1, eps=1e-4, 
                     prior=list(range=0.1, mu=0, kappa=1, s2=1, nugget=0),
                     maxiter=5e2, 
                     verb=FALSE) {
  ## data should be a N x K matrix
  ## N sample size
  ## K dimension of vector.
  N <- nrow(h)
  K <- ncol(h)
  ##
  S <- prior$s2 * matern(as.matrix(dist(X)), 
                         kappa=prior$kappa, 
                         range=prior$range)
  diag(S) <- prior$nugget + prior$s2
  Si <- solve(S)
  ## each rowsum of h should be 1.
  ## Be aware that each datarow should have 
  ## positive values, as log(data) will be computed.
  ##
  a0 <- rep(a0, N)[1:N]
  
  ## gradient function:
  summa <- c(apply(h, 1, function(hi) sum(log(hi))))
  gg <- function(a){
    a <- c(a)
    -0.5* (1/a) * c( c(t(log(a)-prior$mu)%*%Si) + (log(a)-prior$mu)*diag(Si) )
  }
  g <- function(a) 
    c( K*digamma(K*a) - K*digamma(a) + summa - 1/a + gg(a) )
  
  ## second gradient
  q <- function(a) 
    c( K^2*trigamma(K*a) - K*trigamma(a) + 1/a^2 )
  
  NN <- function(a) 
    c( 2*diag(Si) - t(log(a)-prior$mu)%*%Si - (log(a)-prior$mu)*diag(Si) )
  
  H <- function(a)
    diag(q(a), N) - 0.5*( (1/a)%*%t(1/a)*Si + diag( c(NN(a)/(a^2)), N) )
  
  ## loop: 
  dif <- 2*eps
  i<-0
  while( dif > eps & (nax<-i<maxiter)) {
    g0 <- g(a0)
    Hi <- solve(H(a0))
    a1 <- c(a0 - Hi%*%g0)
    dif <- sum(abs(a0-a1))
    a0 <- pmax(a1, 0.001)
    
    if(verb) cat("\r",i<-i+1, " (dif=", dif,")      ", sep="")
  }
  if(verb) cat("\n", ifelse(nax, paste(i, " steps."), "Didn't converge."), "\n", sep="")
  list(est=a1, Si=Si)
}



