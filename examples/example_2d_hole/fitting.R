#' fit the classifier and the alpha 
source("estimate_alpha.R")
source("predict_alpha.R")

#' Classifier for clean data
fitM0 <- function(data0, prior_sd=5) {
  # 
  y <- data0[,1]
  X <- data0[,-1]
  #' naive bayes classifier: parameters are the class averages 
  theta_m <- sapply( split(X, y), function(x) apply(x, 2, mean))
  theta_sd <- sapply( split(X, y), function(x) apply(x, 2, sd))
  theta_rho <- sapply( split(X, y), function(x) cor(x)[1,2])
  #' add prior sd, rought estimate
  theta_sd <- theta_sd + matrix(rep(prior_sd, prod(dim(theta_sd))), 
                                ncol=ncol(theta_sd))/nrow(X)^2
  nc <- table(y)/nrow(X)
  
  list(theta_m=theta_m, theta_sd=theta_sd, theta_rho=theta_rho,
       theta_m0=theta_m, theta_sd0=theta_sd, theta_rho0=theta_rho,
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
  #' NOT WORKING covariance estimation!!!
  data1x<-rbind(data1x)
  data1h<-rbind(data1h)
  m <- fit$theta_m
  s <- fit$theta_sd
  rho <- fit$theta_rho
  
  fit$data_h <- rbind(fit$data_h, data1h)
  x <- fit$data_X <- rbind(fit$data_X, data1x)
  for(i in 1:ncol(m)){
    h <- fit$data_h[,i]  
    ni <- sum(h)
    m[,i] <- colSums(x*h)/ni
    s[,i] <- sqrt(colSums(h * t((t(x)-m[,i])^2))/ni)
    rho[i] <- sum(h * (x[,1]-m[1,i])*(x[,2]-m[2,i]))/(ni*prod(s[,i]))
  }
  rho[is.na(rho)]<-0
  nh <- colSums(fit$data_h)
  l1 <- c(fit$n0 + nh)
  m <- t(t(m)*nh/l1) + t(t(fit$theta_m0)*(fit$n0/l1))
  s <- t(t(s)*nh/l1) + t(t(fit$theta_sd0)*(fit$n0/l1))
  rho <- (nh*rho/l1) + fit$theta_rho0*(fit$n0/l1)
  n <- table(fit$data0$y) + colSums(fit$data_h)
  nc <- n/sum(n)
  
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
  fit$theta_rho <- rho
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
