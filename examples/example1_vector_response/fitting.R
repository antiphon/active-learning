#' fit the classifier for vector value data
#' Classifier for clean data
fitM0 <- function(data0, prior_sd=1) {
  # 
  y <- data0[,1]
  X <- data0[,-1]
  #' naive bayes classifier: parameters are the class averages 
  theta_m <- sapply( split(X, y), function(x) apply(x, 2, mean))
  theta_S <- lapply( split(X, y), function(x) cov(x) + diag(prior_sd/nrow(x), ncol(x)))
       
  nc <- table(y)/nrow(X)
  
  list(theta_m=theta_m, theta_S=theta_S,
       theta_m0=theta_m, theta_S0=theta_S,
       n0 = c(table(y)),
       theta_c=nc, n=c(table(y)), 
       classes=levels(y), 
       data0=cbind(y, X),
       data_h=NULL,
       data_X=NULL,
       alpha=function(xnew) rep(1, nrow(rbind(xnew))),
       prior=NULL
       )
}
#############
update_fitM <- function(fit, data1x, data1h) {
  data1x<-rbind(data1x)
  data1h<-rbind(data1h)
  
  fit$data_h <- rbind(fit$data_h, data1h)
  x <- fit$data_X <- rbind(fit$data_X, data1x)
  m <- fit$theta_m
  S <- fit$theta_S
  # update per class
  for(i in 1:ncol(m)){
    h <- fit$data_h[,i]
    if(sum(h)){
      ni <- sum(h)
      wm <- cov.wt(x, h)
      S[[i]] <- pmax(pmin(wm$cov, 100), -100)
      S[[i]][is.na(S[[i]])] <- 0
      m[,i] <- wm$center
    }
  }
  # balance between the datasets
  nh <- colSums(fit$data_h)
  l1 <- c(fit$n0 + nh)
  l0 <- fit$n0/l1
  m <- t(t(m)*(1-l0)) + t(t(fit$theta_m0)*l0)                         
  for(i in 1:ncol(m)) S[[i]] <- S[[i]]*(1-l0[i]) + fit$theta_S0[[i]]*l0[i]
  n <- table(fit$data0$y) + colSums(fit$data_h)
  nc <- n/sum(n)
    ## done, update result object
  fit$theta_m <- m
  fit$theta_S <- S
  fit$theta_c <- nc
  fit$n <- n
  fit
}



