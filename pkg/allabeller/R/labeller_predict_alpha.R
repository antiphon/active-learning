#'

labeller_predict_alpha <- function(labeller, X) {
  # @TODO doit faster
  if(labeller$estimated) {
  xnew <- as.matrix(cbind(X))
  hyper <- labeller$parameters$hyper
  ## We do this by predicting u and correcting for the mean.
  # compute K(x*, x). 
  Nd <- nrow(labeller$data_X)
  Nn <- nrow(xnew)
  datastar <- rbind(as.matrix(labeller$data_X), xnew)
  D <- as.matrix(dist(datastar))
  K <- matern(D, kappa=hyper$kappa, range=hyper$range)
  K[is.na(K)] <- 1 # @TODO: failsafe for replicated observations
  K <- hyper$s2*K  
  new <- (Nd+1):ncol(D)
  old <- 1:Nd
  #
  # predict the spatial component
  KQ <- K[new,old]%*%(solve(K[old, old] + diag(hyper$s2, Nd)))
  upred <- c(KQ %*% (labeller$spatial$mean) )
  s2pred <- hyper$s2-diag(KQ%*%K[old,new])
  # adjust for the intercept
  pred <- exp(c(upred) + c(labeller$intercept$mean) + 0.5*s2pred)
  } 
  else pred <- 1
  pred
}