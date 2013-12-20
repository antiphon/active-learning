#' 1d

source("R/estimate_alpha.R")
source("R/parameters_default.R")
# generate data
n <- 100
x <- seq(-5, 5, length=n)
u <- -3*cos(x)  
b <- -2
eps <- rnorm(n, 0, 0.1)
a <- exp( b + u + eps)
avec <- matrix(rep(a, each=6), byrow=T, ncol=6)
library(MCMCpack)
h <- t( apply(avec, 1, rdirichlet, n=1) )

#plot(x,a)
pars <- parameters_default()
pars$hyper_prior$mu0 <- 4
pars$hyper$range<-2
pars$hyper_prior$tau0 <- .1
est <- labeller_estimate_alpha(cbind(x), h, pars )

par(mfrow=c(2,1))
plot(x,u, main="u")
lines(x, est$spatial$mean)

plot(x, a, main="a", ylim=c(0,10))
lines(x, exp(est$intercept$mean+est$spatial$mean)  )
