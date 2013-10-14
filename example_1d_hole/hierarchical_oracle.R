#' simulate h-vectors from the hierarchical model
#' 
#' stick to 2d?
library(rfhc)
source("simulate_alpha.R")
require(MCMCpack)
#'

# simulate cautious oracle
rcautious_oracle2D <- function(X, y, alpha) {
  if(missing(alpha)){
    alpha <- simulate_alpha2D(X)
  }
  function(x, ytrue) {
    x<-rbind(x)
    ytrue <- as.integer(ytrue)
    # locate the point
    z<-rfhc_xy2grid(x[,1], x[,2], alpha$xcol, alpha$yrow)
    # extract alpha
    a<-alpha[z]
    # draw from Dirichlet
    am <- t(apply(cbind(a), 1, rep, nlevels(y))  )
    h <- rbind(t(apply(am, 1, function(as) rdirichlet(1,as ) ) ))
    for(i in 1:nrow(h)){
      j <- which.max(h[i,])
      ht <- h[i, ytrue[i] ]
      h[i, ytrue[i]] <- h[i,j]
      h[i,j] <- ht
    }
    h
  }
}


# simulate cautious oracle
rcautious_oracle1D <- function(X, y, alpha) {
  if(missing(alpha)){
    alpha <- simulate_alpha1D()
  }
  function(x, ytrue) {
    x<-cbind(x)
    ytrue <- as.integer(ytrue)
    # locate the point
    z <- apply(x, 1, function(xi) sum(xi >= alpha$x))
    # extract alpha
    a<-alpha$alpha[z]
    # draw from Dirichlet
    am <- t(apply(cbind(a), 1, rep, nlevels(y))  )
    h <- rbind(t(apply(am, 1, function(avec) rdirichlet(1, avec ) ) ))
    for(i in 1:nrow(h)){
      j <- which.max(h[i,])
      ht <- h[i, ytrue[i] ]
      h[i, ytrue[i]] <- h[i,j]
      h[i,j] <- ht
    }
    list(h=h, a=a)
  }
}

entropy <- function(h) {
  h<-cbind(h)
  apply(h , 1, function(p)  -sum(p*log(p)) )
}

if(exists("TEST_SIM")) {
  ## Generate some data
  set.seed(1234)
  X <- data.frame( rbind(cbind(x=rnorm(50, -1, .6), y=rnorm(50, 0, 1) ),
                         cbind(x=rnorm(50, 0, 1), y=rnorm(50, -1, .3) ),
                         cbind(x=rnorm(50, 3, .6), y=rnorm(50, 0, 1) )))
  y <- factor(rep(1:3, each=50), labels=c("left", "bottom", "right"))
    
  if(!exists("alpha")) alpha <- simulate_alpha(X, range=3, tau=1, mu=0, ncol=50)
  
  z<-rfhc_xy2grid(X[,1], X[,2], alpha$xcol, alpha$yrow)
  v<-alpha[z]
  oracle <- rcautious_oracle2D(X, y, alpha)
  
  h <- oracle(X, y)
  
  # estimate
  source("estimate_alpha.R")
  prior <- list(range=1, kappa=1, s2=1000, nugget=0)
  a_hat <- estimate_alpha(h, X, prior=prior, verb=3)
  
  # what we got:
  par(mfrow=c(2,2))
  ## indep
  qqplot(v, a_hat$indep, main="Q-Q", log="xy", xlab="log alpha", ylab="log alpha_hat")
  abline(a=0, b=1)
  plot(alpha, col=gray.colors(50))
  points(X, cex=a_hat$indep, col=y) 
  ## GP
  qqplot(v, a_hat$gp, main="Q-Q", log="xy", xlab="log alpha", ylab="log alpha_hat")
  abline(a=0, b=1)
  
  plot(alpha, col=gray.colors(50))
  points(X, cex=a_hat$gp, col=y) 
  
}



