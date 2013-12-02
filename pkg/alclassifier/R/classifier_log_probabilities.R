classifier_log_probabilities <- function(classifier, X) {
  ps <- NULL
  xz <- rbind(X)
  d <- ncol(xz)
  for(i in 1:length(classifier$classes)){
    S <- classifier$theta_S[[i]]
    logp <- dmvnorm(xz, classifier$theta_m[,i], S, log=TRUE) #+log(classifier$theta_c[i])
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
