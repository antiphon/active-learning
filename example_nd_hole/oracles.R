#' the labellers
require(MCMCpack)
create_oracle_with_alpha <- function(alpha, data) {
  ## Assume labels are the first column/variable in the data frame.
  classes <- levels(data[,1])
  nclasses <- length(classes)
  function(dataUnlabeled) {
    x<-rbind(dataUnlabeled)
    ## true labels. 
    labels <- data[,1][apply(x,  1, function(z) 
      which(apply(data[,-1], 1, function(v) all(v==z))   )[1])]

    ## get the alpha values
    a<-alpha(x)
    # draw from Dirichlet
    am <- t(apply(cbind(a), 1, rep, nclasses)  )
    h <- rbind(t(apply(am, 1, function(as) rdirichlet(1, as ) ) ))
    for(i in 1:nrow(h)){
      j <- which.max(h[i,])
      ht <- h[i, labels[i] ]
      h[i, labels[i]] <- h[i,j]
      h[i,j] <- ht
    }
    list(h=h, a=a)
  }
}

create_oracle_with_unknown_label <- function(data, class) {
  ## Assume labels are the first column/variable in the data frame.
  classes <- levels(data[,1])
  nclasses <- length(classes)
  function(dataUnlabeled) {
    x<-rbind(dataUnlabeled)
    ## true labels. 
    labels <- data[,1][apply(x,  1, function(z) 
      which(apply(data[,-1], 1, function(v) all(v==z))   )[1])]
    
    ## set the alpha values according to knowledge
    a <- rep(0.01, length(labels))
    a[labels==class] <- 20
    # draw from Dirichlet
    am <- t(apply(cbind(a), 1, rep, nclasses)  )
    h <- rbind(t(apply(am, 1, function(as) rdirichlet(1, as ) ) ))
    for(i in 1:nrow(h)){
      j <- which.max(h[i,])
      ht <- h[i, labels[i] ]
      h[i, labels[i]] <- h[i,j]
      h[i,j] <- ht
    }
    list(h=h, a=a)
  }
}
