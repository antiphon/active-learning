#'@import mvtnorm

classifier_informativeness <- function(classifier, X) {
  ## Compute the class probabilities for each item/row in X
  ps <- classifier_log_probabilities(classifier, X)
  # the I for classifier is the entropy
  If <- function(z) -sum(exp(z)*z)
  apply(ps, 1, If)
}


classifier_log_probabilities <- function(classifier, X) {
  ps <- NULL
  xz <- rbind(X)
  d <- ncol(xz)
  for(i in 1:length(classifier$classes)){
    S <- classifier$theta_S[[i]]
    logp <- log(classifier$theta_c[i]) + dmvnorm(xz, classifier$theta_m[,i], S, log=) 
    ps <- cbind(ps, logp)
  }
  # these are unscaled, need to rescale
  ps <- t(apply(ps, 1, function(logp)
                          logp-log(sum( exp(logp) ) ) 
                )
          )
  colnames(ps) <- colnames(classifier$theta_m)
  ps
}