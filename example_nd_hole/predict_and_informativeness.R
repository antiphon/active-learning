#' max-prob classification
classify <- function(X, fit) {
  p<-probM(X, fit)
  fit$classes[apply(p, 1, which.max)]
}

#' The class probabilities for x as predicted by the fit
require(mvtnorm)
probM <- function(x, fit) {
  ps <- NULL
  xz <- rbind(x)
  d <- ncol(xz)
  for(i in 1:length(fit$classes)){
    p<-NULL
    S <- fit$theta_S[[i]]
    for(j in 1:nrow(xz)){
      p[j] <- log(fit$theta_c[i]) + dmvnorm(xz[j,], fit$theta_m[,i], S, log=T) 
    }
    ps <- cbind(ps, p)
  }
  # scale
  ps<<-ps
  ps <- t(apply(ps, 1, function(p) p-log(sum(exp(p))) ))
  colnames(ps) <- colnames(fit$theta_m)
  ps
}

#' all sorts of informativeness measures
If <- function(X, fit, ps, type="entropy") {
  if(missing(ps)) ps <- probM(X, fit)
  
  g<- switch(type, 
             entropy=function(z) -sum(exp(z)*z),
             least_sure=function(z) 1-max(z),
             delta=function(z) {v<-sort(z, decreasing=T); 1-(v[1]-v[2])},
             random=function(z){p<-rep(0, nrow(X));p[sample(1:nrow(X), 1)] <-1 ;p}
             )

  v <- apply(ps, 1, g)
  v
}


