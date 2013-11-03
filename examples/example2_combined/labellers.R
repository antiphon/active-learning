#' the labellers
require(MCMCpack)

create_oracle_with_hole <- function(data, center, radius, v_in=0.1, v_out=3) {
  classes <- levels(data[,1])
  nclasses <- length(classes)
  
  function(x) {
    labels <- data[,1][apply(x,  1, function(z) 
      which(apply(data[,-1], 1, function(v) all(v==z))   )[1])]
    is_out <- colSums((t(rbind(x))-center)^2) > radius^2
    a <- c(v_in, v_out)[is_out+1]
    
    h<-matrix(1/(nclasses), nrow=nrow(x), ncol=nclasses)
    for(i in which(!is_out)){
      h[i,]<-0
      h[i, which(classes==labels[i])] <- 1
    }
    list(h=h, a=a)
  }
}
