#'
parameters_default <- function() {
  list(
  hyper_prior = list(tau0=.01,  # in case we estimate, set these priors
                     mu0=0,
                     tau_u_a=.01,
                     tau_u_b=.01,
                     tau_nugget_a=.01,
                     tau_nugget_b=.01,
                     range_a=.1,
                     range_b=.1),
  hyper=list(range=1, kappa=1, s2=2, nugget=2), # in case we dont estimate
  estimate_hyper=FALSE,
  start=c(1,1,1),
  lower=c(1e-1, 1e-1, .1),
  upper=c(1e1, 1e1, 10),
  verb=FALSE,
  covf = kexp,
  gaussian_max = 200,
  gaussian_min = -200,
  minimum_data=5
  )
}

kmatern <- function(x, hyper=list(kappa = 1, range = 1)) {
  kappa <- hyper$kappa
  range <- sqrt(8 * kappa) / hyper$range 
  besselK(x*range, kappa)*(x*range)^kappa/(2^(kappa-1)*gamma(kappa))
}

kgaussian <- function(x, hyper=list(range=1)) {
  range <- hyper$range/2.146 # this way range=1 -> rho=0.1
  exp(-(x/range)^2/2)
}

kexp <- function(x, hyper=list(range=1)) {
  range <- hyper$range/2.3026
  exp(-x/range)
}
