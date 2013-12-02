#' the labellers

library(MCMCpack)

create_shot_noise_labeller <- function(Xpool, ypool, Xknown, yknown, kernel) {
  classes <- levels(ypool)
  nclasses <- length(classes)
  Xknown <- rbind(Xknown)
  nknown <- nrow(Xknown)
  
  function(xnew) {
    xnew <- rbind(xnew)
    nnew <- nrow(xnew)
    labels <- ypool[apply(xnew,  1, function(z) 
      which(apply(Xpool, 1, function(v) all(v==z))   )[1])]
    dists <- rbind( as.matrix(dist(rbind(Xknown, xnew)))[1:nknown, nknown+1:nnew] )
    w <- rbind(apply(dists, 2, kernel))
    mass <- pmin(1, colSums(w)/kernel(0))
    alfas <- 0.001 - pmin(log(mass), 30)
    
    alfavecs <- t(sapply(alfas, rep, nclasses))
    h <- t( apply(alfavecs, 1, rdirichlet, n=1) )
    for(i in 1:nrow(h)) {
      j <- which(labels[i]==classes)
      k <- which.max(h[i,])
      h0 <- h[i,j]
      h[i,j] <- max(h[i,])
      h[i,k] <- h0
    }
    h
  }
}