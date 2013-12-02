#'
parameters_default <- function() {
  list(
  hyper_prior = list(tau0=.01,  # in case we estimate
                     tau_a=.01,
                     tau_b=.01,
                     range_a=.1,
                     range_b=.1),
  hyper=list(range=1, kappa=1, s2=2, nugget=2), # in case we dont estimate
  estimate_hyper=FALSE,
  start=c(1,1,1),
  lower=c(1e-1, 1e-1, .1),
  upper=c(1e1, 1e1, 10),
  verb=FALSE,
  gaussian_max = 200,
  gaussian_min = -200,
  minimum_data=5
  )
}
