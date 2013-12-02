library(MCMCpack)
####
def_co <- c(0.8,-0.9)
def_ro <- 1
##

create_shot_noise_labeller <- function(Xpool, ypool, Xknown, kernel, flatten=FALSE) {
  classes <- levels(ypool)
  nclasses <- length(classes)
  nknown <- nrow(Xknown)
  function(xnew) {
    xnew <- rbind(xnew)
    nnew <- nrow(xnew)
    labels <- ypool[apply(xnew,  1, function(z) 
      which(apply(Xpool, 1, function(v) all(v==z))   )[1])]
    
    dists <- rbind( as.matrix(dist(rbind(Xknown, xnew)))[1:nknown, nknown+1:nnew] )
    w <- cbind(apply(dists, 2, kernel))
    mass <- pmin(1, apply(w, 2, max)/kernel(0))
    alfas <- 0.0001 - log(mass + 1e-300)
    alfavecs <- t(sapply(alfas, rep, nclasses))
    h <- t( apply(alfavecs, 1, rdirichlet, n=1) )
    for(i in 1:nrow(h)) {
      if(is.na(h[i,1])) h[i,]<-c(1, rep(0, nclasses-1))
      j <- which(labels[i]==classes)
      k <- which.max(h[i,])
      h0 <- h[i,j]
      h[i,j] <- max(h[i,])
      h[i,k] <- h0
      if(flatten) h[i,-j] <- (1-max(h[i,]))/(nclasses-1)
    }
    list(h=h, alfas=alfas)
  }
}

kernel_make <- function(type="gaussian", par=1){
  switch(type,
         "gaussian"=function(x) dnorm(x, 0, par),
         "epa" = function(x) 3/(4*par) * (1-x^2/par^2)* (abs(x)<par),
         "uniform"=function(x) 1*(abs(x)<par)/(2*par))
}
