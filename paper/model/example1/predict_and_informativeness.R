#' max-prob classification
classify <- function(X, fit) {
  p<-probM(X, fit)
  fit$classes[apply(p, 1, which.max)]
}

#' The class probabilities for x as predicted by the fit
probM <- function(x, fit) {
  ps <- NULL
  xz <- rbind(x)
  cat2("fit")
  for(j in 1:nrow(xz)) {
    p<-NULL
    for(i in 1:length(CLASSES)){
      p[i] <- log(fit$theta_c[i]) + sum( sapply(1:ncol(xz), function(k)
        dnorm(xz[j,k], fit$theta_m[k,i], fit$theta_sd[k,i], log=T) )  )
    }
    ps <- rbind(ps, log(exp(p)/sum(exp(p))) )
  }
  colnames(ps) <- colnames(fit$theta_m)
  cat2("\n")
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


