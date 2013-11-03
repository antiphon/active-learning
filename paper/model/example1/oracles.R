#' various oracles
#' 


#' the cautious oracle: know only one of the classes
cautious_oracle_know_one <- function(x, 
                                     oracle=FALSE, # know all?
                                     realize=FALSE # sample one with the probabilities?
) {
  #' we know only the third class
  x<-rbind(x)
  labels <- data0$y[apply(x,  1, function(z) 
    which(apply(data0$X, 1, function(v) all(v==z))   )[1])]
  h <- NULL
  cat2("oracle")
  uv <- rep(1, length(CLASSES))/(length(CLASSES)-1)
  for(label in labels) {
    if(!oracle) h <- rbind(h, 
                           if(label==known_label) 1*(label==CLASSES) 
                           else (1-(label==CLASSES))*uv
    )
    else h <- rbind(h, 1*(label==CLASSES))
  }
  if(realize) h<-t(apply(h, 1, function(p) {
    v<-p*0;i<-sample(1:length(p), 1, prob=p+1e-5); v[i]<-1;v
  } ))
  cat2("\n")
  h
}

#' the cautious oracle: known inside a geometric area
co_circlec <- c(0.7,-1)
co_radius <- 1.5
cautious_oracle_limited <- function(x, 
                                    oracle=FALSE, # know all?
                                    realize=FALSE # sample one with the probabilities?
) {
  # know everything inside a sphere
  x<-rbind(x)
  ## true labels
  labels <- data0$y[apply(x,  1, function(z) 
    which(apply(data0$X, 1, function(v) all(v==z))   )[1])]
  h <- NULL
  cat2("oracle")
  uv <- rep(1, length(CLASSES))/(length(CLASSES))
  for(i in 1:nrow(x)) {
    tr <- 1*(labels[i]==CLASSES)
    if(oracle) h <- rbind(h, tr)
    else {
      h<-rbind(h, if( sum((x[i,]-co_circlec)^2) > co_radius^2) uv else tr)
    }
  }
  if(realize) h<-t(apply(h, 1, function(p) {
    v<-p*0;i<-sample(1:length(p), 1, prob=p+1e-5); v[i]<-1;v
  } ))
  cat2("\n")
  h
}
