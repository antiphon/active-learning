#'
parameters_default <- function() {
  list(
  hyper_prior = list(tau0=1e-4,  # in case we estimate
                     tau_a=1e-5,
                     tau_b=1e-5,
                     range_a=1,
                     range_b=1),
  hyper=list(range=1, kappa=1, s2=2, nugget=2), # in case we dont estimate
  estimate_hyper=FALSE,
  start=c(1,1,1),
  lower=c(1e-4, 1e-4, .05),
  upper=c(1e4, 1e4, 10),
  verb=TRUE,
  minimum_data=5
  )
}
