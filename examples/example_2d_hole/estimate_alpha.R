#' estimate alpha
#' just do it with smoothing for now.
source("dirichlet_ML_symmetric_GP_prior.R")
#
estimate_alpha <- function(h, X, prior, ...) {
  a_hat_GP <- DirGPSymML(X, rbind(h), prior=prior, ...)  
  #
  list(gp=a_hat_GP$est, Si=a_hat_GP$Si, prior=prior)
}

