#' simulate the alpha field
#' 
library(rfhc)
source("matern.R")

simulate_alpha2D <- function(X, ncol=50, mu=0, inflate=0.1, ...){
  xl <- range(X[,1]); xl <- xl+c(-1,1)*diff(xl)*inflate
  yl <- range(X[,2]); yl <- yl+c(-1,1)*diff(yl)*inflate
  ncol <- floor(ncol*diff(xl)/diff(yl))
  alpha <- simulateGMRF(nrow=ncol, ncol=ncol, xlim=xl, ylim=yl, ...)
  alpha$v <- exp(mu+alpha$v)
  alpha
}

simulate_alpha1D <- function(xmin=0, xmax=1, resolution=50,
                             inflate=0.1,
                             prior=list(mu=-1, kappa=1, nugget=0, s2=0.1, range=0.1),
                             seed,
                             ...) {
  if(!missing(seed)) set.seed(seed)
  xl <- c(xmin, xmax)
  xl <- xl+c(-1,1)*diff(xl)*inflate
  xseq <- seq(xl[1], xl[2], length=resolution)
  S <- prior$s2 * matern(as.matrix(dist(xseq)), kappa=prior$kappa, range=prior$range)
  diag(S) <- prior$nugget +  prior$s2
  z<-rnorm(length(xseq))
  gaus <- t(chol(S))%*%z + prior$mu
  ## 2. exp
  list(x=xseq, alpha=exp(gaus))
}
