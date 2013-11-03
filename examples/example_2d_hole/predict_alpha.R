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
    D <-D[(Nd+1):ncol(D), 1:Nd]
    if(is.null(dim(D))) D <- cbind(D)
    K <- p$s2*matern(D,
                kappa=p$kappa, range=p$range)
    K[is.na(K)] <- 0 ## TODO failsafe
    #
    #cat("[K=", dim(K), ", Si=",dim(est$Si),"] ")
    lv <- K%*%est$Si%*%(log(est$gp)-p$m)
    e <- c(exp(lv + p$m))
    ## cap?
    e #pmin(e, 100)
  }
}