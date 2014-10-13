#' oracle labeller

#' Create a labeller that knows everything
library(MCMCpack)

create_oracle <- function(Xpool, ypool) {
  classes <- levels(ypool)
  nclasses <- length(classes)
  #' this will be returned
  function(xnew, samp=TRUE) {
    xnew <- rbind(xnew)
    nnew <- nrow(xnew)
    labels <- ypool[apply(xnew,  1, function(z) which(apply(Xpool, 1, function(v) all(v==z))   )[1])]  
    
    
    alfas <- rep(0, nnew)
    if(!samp) return(list(alfas=alfas))
    h <- matrix(0, ncol=nclasses, nrow=nnew)
    labi <- match(labels, classes)
    h[cbind(1:nnew, labi)] <- 1
    list(h=h, alfas=alfas)
  }
}