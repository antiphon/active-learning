#'

classifier_update <- function(classifier, xnew, hnew ) {
  
  data1x<-rbind(xnew)
  data1h<-rbind(hnew)
  
  classifier$data_h <- rbind(classifier$data_h, data1h)
  x <- classifier$data_X <- rbind(classifier$data_X, data1x)
  m <- classifier$theta_m
  S <- classifier$theta_S
  # update per class
  for(i in 1:ncol(m)){
    h <- classifier$data_h[,i]  
    ni <- sum(h)
    if(ni){
      wm <- cov.wt(x, h)
      S[[i]] <- pmax(pmin(wm$cov, 100), -100)
      S[[i]][is.na(S[[i]])] <- 0
      m[,i] <- wm$center
    }
  }
  # balance between the datasets
  nh <- colSums(classifier$data_h)
  l1 <- c(classifier$n0 + nh)
  l0 <- classifier$n0/l1
  m <- t(t(m)*(1-l0)) + t(t(classifier$theta_m0)*l0)                         
  for(i in 1:ncol(m)) S[[i]] <- S[[i]]*(1-l0[i]) + classifier$theta_S0[[i]]*l0[i]
  n <- table(classifier$data0$y) + colSums(classifier$data_h)
  nc <- n/sum(n)
  # add updated values
  classifier$theta_m <- m
  classifier$theta_S <- S
  classifier$theta_c <- nc
  classifier$n <- n
  classifier  
}