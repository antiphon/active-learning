### Data: ############################################################
load_data <- function(){
  songs <- readRDS("data/dortmund-features.Rds")
  
  traini <- sample(1:nrow(songs), nrow(songs)*0.7)
  testi <- setdiff(1:nrow(songs), traini)
  
  feats <- subset(songs, select=-c(id, genre))
  
  feats <- feats[, which(apply(feats, 2, function(x) diff(range(x)) != 0 ))]
  
  X <- as.data.frame(scale(feats))
  
  songs_train <- data.frame(genre = songs$genre[traini],
                                 X=X[traini, ])
  ##  test set
  songs_test <- data.frame(genre = songs$genre[testi],
                         X=X[testi, ])
    
  list(train=songs_train, test=songs_test, X=X, genre=songs$genre)
}