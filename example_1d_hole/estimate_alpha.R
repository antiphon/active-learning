#' estimate alpha
#' just do it with smoothing for now.
source("dirichlet_ML_symmetric.R")
source("dirichlet_ML_symmetric_GP_prior.R")
#
estimate_alpha <- function(h, X, ...) {
  ## THIS IS WRONG AS NO PROXIMITY SYNERGY USED
  a_hat <- apply(h, 1, function(hi) DirSymML(rbind(hi), prior=c(1,1), a0=0.1) ) 
  
  ## This should be better
  a_hat_GP <- DirGPSymML(X, rbind(h), ...)
  
  #
  list(gp=a_hat_GP, indep=a_hat)
  #a_hat_GP
}

