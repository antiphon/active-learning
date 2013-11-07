
create_labeller_with_known_genres <- function(data, known_genres) {
  known_genres <- sort(known_genres)
  genres <- levels(data$genre)
  ngenres <- length(genres)
  nc <- which(genres %in% known_genres)
  function(dataUnlabeled) {
    x<-rbind(dataUnlabeled)
    ## true labels. 
    labels <- data$genre[apply(x,  1, function(z) 
      which(apply(subset(data, select = -c(genre)), 1, function(v) all(v==z))   )[1])]
    knows <- labels %in% known_genres
    h<-matrix(1/(ngenres-length(known_genres)), ncol=ngenres, nrow=length(labels))
    h[,nc] <- 0
    h[knows,] <- 0
    h[cbind(which(knows),nc[match(labels[knows], known_genres)])] <- 1
    list(h=h)
  }
}

