#'

classifier_update <- function(classifier, xnew, hnew, digits=4) {
  
  data1x<-rbind(xnew)
  data1h<-rbind(hnew)
  
  h <- classifier$data_h <- rbind(classifier$data_h, data1h)
  x <- classifier$data_X <- rbind(classifier$data_X, data1x)
  #x <- rbind(classifier$data0[,-1], classifier$data_X)
  #h <- rbind(classifier$data0_h, classifier$data_h)
  m <- classifier$theta_m
  S <- classifier$theta_S
  # update per class
  for(i in 1:ncol(m)){
    hi <- round( h[,i]  , digits)
    ni <- sum(hi)
    if(ni){
      wm <- cov.wt(x, hi)
      Si <- pmax(pmin(wm$cov, 1000), -1000) 
      Si[is.na(Si)] <- 0
      #if(sum(is.na(S[[i]]))) warning("Some S values are NA.")
      S[[i]] <- Si
      m[,i] <- wm$center
    }
  }
  # balance between the initial and new datasets
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
  # done updating the classifier
  classifier  
}