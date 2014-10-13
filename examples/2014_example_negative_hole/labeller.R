#' Create a labeller that has a hole in his knowledge
library(MCMCpack)

create_labeller_neg_hole <- function(Xpool, ypool, center, range, flatten=0) {
  classes <- levels(ypool)
  nclasses <- length(classes)
  #' this will be returned
  function(xnew, samp=TRUE) {
    xnew <- rbind(xnew)
    nnew <- nrow(xnew)
    labels <- ypool[apply(xnew,  1, function(z) which(apply(Xpool, 1, function(v) all(v==z))   )[1])]  
    know <- apply(xnew, 1, function(xy) sum((xy-center)^2) > range^2  )
    
    alfas <- 30 - 29.99*know
    if(!samp) return(list(alfas=alfas))
    alfavecs <- t(sapply(alfas, rep, nclasses))
    h <- t( apply(alfavecs, 1, rdirichlet, n=1) )
    for(i in 1:nrow(h)) {
      if(is.na(h[i,1])) h[i,]<-c(1, rep(0, nclasses-1))
      j <- which(labels[i]==classes)
      k <- which.max(h[i,])
      h0 <- h[i,j]
      h[i,j] <- max(h[i,])
      h[i,k] <- h0
      if(flatten>0){
        o <- order(h[i,], decreasing=TRUE)
        h[i, o[-(1:flatten)]] <- (1-sum(h[i,o[1:flatten]]))/(nclasses-flatten)
      }
    }
    list(h=h, alfas=alfas)
  }
}