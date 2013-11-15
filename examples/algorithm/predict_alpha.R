#' predict alpha to given locations using krigin

predictor_alpha <- function(est, data) {
  Nd <- nrow(data)
  function(xnew) {
    xnew <- rbind(xnew)
    # TODO doit correctly, just for testing now
    p <- est$prior
    Nn <- nrow(xnew)
    xnew<-as.matrix(xnew)
    datastar <- rbind(as.matrix(data), xnew)
    D <- as.matrix(dist(datastar))
    D <-D[(Nd+1):nrow(D), 1:Nd]
    if(is.null(dim(D))) D <- cbind(D)
    K <- p$s2*matern(D, kappa=p$kappa, range=p$range)
    K[is.na(K)] <- 0 ## TODO failsafe
    lv <- K%*%est$Si%*%(log(est$gp)-p$m)
    e <- c(exp(lv + p$m))
    e
  }
}

predictor_alpha_inla <- function(est, data) {
  Nd <- nrow(data)
  function(xnew) {
    xnew <- as.matrix(cbind(xnew))
    # TODO doit correctly, just for testing now
    hyper <- est$hyper
    Nn <- nrow(xnew)
    ## We do this by predicting u and correcting for the mean.
    # compute K(x*, x). 
    datastar <- rbind(as.matrix(data), xnew)
    D <- as.matrix(dist(datastar))
    #D <- D[(Nd+1):ncol(D), 1:Nd]
    #if(is.null(dim(D))) D <- cbind(D)
    K <- matern(D, kappa=hyper$kappa, range=hyper$range)
    #diag(K) <- 1
    K[is.na(K)] <- 1 ## TODO failsafe for replicated observations
    K <- hyper$s2*K  
    new <- (Nd+1):ncol(D)
    old <- 1:Nd
    #
    # predict the spatial component
    KQ <- K[new,old]%*%(solve(K[old, old] + diag(hyper$s2, Nd)))
    upred <- c(KQ %*% (est$u$m) )
    s2pred <- hyper$s2-diag(KQ%*%K[old,new])
    # adjust for the intercept
    e <- exp(c(upred) + c(est$intercept) + 0.5*s2pred)
    e
    #list(m=e, s2=s2pred)
  }
}

predictor_variance <- function(fit, data) {
  Nd <- nrow(data) 
  K <- length(fit$fixed$m)
  function(xnew) {
    xnew <- cbind(xnew)
    hyper <- est$hyper
    
  }
}



