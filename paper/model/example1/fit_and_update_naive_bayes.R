#' Classifier for clean data
fitM0 <- function(X, y, prior_sd=4) {
  #' naive bayes classifier: parameters are the class averages 
  theta_m <- sapply( split(X, y), function(x) apply(x, 2, mean))
  theta_sd <- sapply( split(X, y), function(x) apply(x, 2, sd))
  #' add prior sd, rought estimate
  theta_sd <- theta_sd + matrix(rep(prior_sd, prod(dim(theta_sd))), 
                                ncol=ncol(theta_sd))/nrow(X)^2
  nc <- table(y)/nrow(X)

  list(theta_m=theta_m, theta_sd=theta_sd, theta_c=nc, n=table(y), classes=levels(y))
}


update_fitM <- function(fit, data1x, data1h) {
  #' TODO: reallly crude approximation, haven't derived formulas yet!
  X<-rbind(data1x)
  H<-rbind(data1h)
  m <- fit$theta_m
  s <- fit$theta_sd
  nc <- fit$theta_c
  n <- fit$n
  for(i in 1:nrow(X)) {
    x <- X[i,]
    h <- H[i,]
    for(sp in 1:ncol(m)) {
      for(ro in 1:nrow(m)) {
        mnew <- as.numeric((m[ro, sp]*n[sp] + h[sp]*x[ro])/(n[sp]+h[sp]) )
        ssold <- n[sp] * (s[ro, sp]^2 + m[ro,sp]^2)
        snew <- sqrt( (ssold+h[sp]*x[ro]^2 )/(n[sp]+h[sp]) - mnew^2 )
        m[ro, sp] <- mnew
        s[ro, sp] <- snew
      }
    }
    nc <- nc + h
    nc <- nc/sum(nc)
    n <- n+h
  }
  list(theta_m=m, theta_sd=s, theta_c=nc, n=n, classes=fit$classes)
}
