matern<-function(x, kappa=1, range=1) 
  besselK(x/range, kappa)*(x/range)^kappa/(2^(kappa-1)*gamma(kappa))
