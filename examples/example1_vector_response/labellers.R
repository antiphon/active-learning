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
    h<-matrix(1/(nclasses-1), ncol=nclasses, nrow=length(labels))
    h[,nc] <- 0
    h[knows,] <- 0
    h[knows,nc] <- 1
    list(h=h)
  }
}

create_oracle_all_knowing <- function(data) {
  ## Assume labels are the first column/variable in the data frame.
  classes <- levels(data[,1])
  nclasses <- length(classes)
  function(dataUnlabeled) {
    x<-rbind(dataUnlabeled)
    ## true labels. 
    labels <- data[,1][apply(x,  1, function(z) 
      which(apply(data[,-1], 1, function(v) all(v==z))   )[1])]
    knows <- which(labels%in%classes)
    h<-matrix(0, ncol=nclasses, nrow=length(labels))
    h[cbind(knows, match(labels, classes)) ] <- 1
    list(h=h)
  }
}


create_oracle_with_known_label_realize <- function(data, class) {
  ## Assume labels are the first column/variable in the data frame.
  classes <- levels(data[,1])
  nclasses <- length(classes)
  nc <- which(classes==class)
  nnc <- which(classes!=class)
  if(length(nc)!=1) stop("wrong class.")
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

####
def_co <- c(1.7,-1)
def_ro <- 1.4
##

create_oracle_hole <- function(data, center=def_co, radius=def_ro, v_in=0.01, v_out=30) {
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


create_oracle_hole_realize <- function(data, center=def_co, radius=def_ro, v_in=0.01, v_out=30) {
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
      ii <- which(classes==labels[i])
      h[i, ii] <- 1
    }
    o  <- apply(h, 1, function(z) sample(1:nclasses, 1, prob=z))
    h <- h*0
    h[cbind(1:length(o), o)] <- 1
    list(h=h, a=a)
  }
}


create_shot_noise_labeller <- function(Xpool, ypool, Xknown, kernel) {
  classes <- levels(ypool)
  nclasses <- length(classes)
  nknown <- nrow(Xknown)
  function(xnew) {
    xnew <- rbind(xnew)
    nnew <- nrow(xnew)
    labels <- ypool[apply(xnew,  1, function(z) 
      which(apply(Xpool, 1, function(v) all(v==z))   )[1])]
    
    dists <- rbind( as.matrix(dist(rbind(Xknown, xnew)))[1:nknown, nknown+1:nnew] )
    w <- rbind(apply(dists, 2, kernel))
    mass <- pmin(1, colSums(w)/kernel(0))
    alfas <- 0.01 - pmin(log(mass+0.0001), 30)
    
    alfavecs <- t(sapply(alfas, rep, nclasses))
    h <- t( apply(alfavecs, 1, rdirichlet, n=1) )
    for(i in 1:nrow(h)) {
      j <- which(labels[i]==classes)
      k <- which.max(h[i,])
      h0 <- h[i,j]
      h[i,j] <- max(h[i,])
      h[i,k] <- h0
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


