#' the labellers
require(MCMCpack)

create_oracle_with_known_label <- function(data, class) {
  ## Assume labels are the first column/variable in the data frame.
  classes <- levels(data[,1])
  nclasses <- length(classes)
  nc <- which(classes==class)
  function(dataUnlabeled) {
    x<-rbind(dataUnlabeled)
    ## true labels. 
    labels <- data[,1][apply(x,  1, function(z) 
      which(apply(data[,-1], 1, function(v) all(v==z))   )[1])]
    knows <- labels==class
    h<-matrix(0.5, ncol=nclasses, nrow=length(labels))
    h[,nc] <- 0
    h[knows,] <- 0
    h[knows,nc] <- 1
    list(h=h)
  }
}

create_oracle_with_known_label_realize <- function(data, class) {
  ## Assume labels are the first column/variable in the data frame.
  classes <- levels(data[,1])
  nclasses <- length(classes)
  nc <- which(classes==class)
  nnc <- which(classes!=class)
  function(dataUnlabeled) {
    x<-rbind(dataUnlabeled)
    ## true labels. 
    labels <- data[,1][apply(x,  1, function(z) 
      which(apply(data[,-1], 1, function(v) all(v==z))   )[1])]
    knows <- labels==class
    h<-matrix(0, ncol=nclasses, nrow=length(labels))
    h[knows, nc] <- 1
    h[cbind(which(!knows), sample(nnc, sum(!knows), replace=TRUE))] <- 1
    list(h=h)
  }
}