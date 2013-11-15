#' fit the classifier and the alpha 
source("estimate_alpha.R")
source("predict_alpha.R")

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
update_fitM <- function(fit, data1x, data1h, ...) {
  data1x<-rbind(data1x)
  data1h<-rbind(data1h)
  
  fit$data_h <- rbind(fit$data_h, data1h)
  x <- fit$data_X <- rbind(fit$data_X, data1x)
  m <- fit$theta_m
  S <- fit$theta_S
  # update per class
  for(i in 1:ncol(m)){
    h <- fit$data_h[,i]  
    ni <- sum(h)
    if(ni){
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
  
  ## then we go and update the alpha
  ## TODO update formula here!
  if(fit$est_alpha & fit$collect_n_before_est_alpha <= nrow(fit$data_h)){
    if(fit$use_inla){
      fit$a_est <- estimate_alpha_inla(fit$data_h, fit$data_X, prior=fit$prior, ...)
      fit$alpha <- predictor_alpha_inla(fit$a_est, fit$data_X)
    }
    else{
      fit$a_est <- estimate_alpha(fit$data_h, fit$data_X, prior=fit$prior)
      fit$alpha <- predictor_alpha(fit$a_est, fit$data_X)
    }
  }
  else fit$alpha <- function(xy) rep(1, nrow(rbind(xy)))
  ## done, update result object
  fit$theta_m <- m
  fit$theta_S <- S
  fit$theta_c <- nc
  fit$n <- n
  fit
}



update_fitM_old <- function(fit, data1x, data1h) {
  #' NOT WORKING covariance estimation!!!
  X<-rbind(data1x)
  H<-rbind(data1h)
  m <- fit$theta_m
  s <- fit$theta_sd
  rho <- fit$theta_rho
  nc <- fit$theta_c
  n <- fit$n
  for(i in 1:nrow(X)) {
    x <- X[i,]
    h <- H[i,]
    for(sp in 1:ncol(m)) {
      cov_old <- rho[sp]*prod(s[,sp])
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
  fit$data_h <- rbind(fit$data_h, data1h)
  fit$data_X <- rbind(fit$data_X, data1x)
  ## then we go and update the alpha
  ## TODO update formula here!
  if(fit$est_alpha){
    fit$a_est <- estimate_alpha(fit$data_h, fit$data_X, prior=fit$prior)
    fit$alpha <- predictor_alpha(fit$a_est, fit$data_X)
  }
  else fit$alpha <- function(xy) rep(1, nrow(rbind(xy)))
  ## done, update result object
  fit$theta_m <- m
  fit$theta_sd <- s
  fit$theta_c <- nc
  fit$n <- n
  fit
}


