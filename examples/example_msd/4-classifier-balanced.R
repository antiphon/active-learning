
library("seriation")
library("randomForest")



### Data: ############################################################

songs_meta <- readRDS("data/msd-metadata.Rds")
songs_feat_pca <- readRDS("data/msd-features-pca.Rds")
songs_feat_full <- readRDS("data/msd-features-full.Rds")


### Balanced classification problem of decades:

sample_songs <- function() {
  dec_oi <- tail(levels(songs_meta$decade), 6)
  dec_oi

  min_songs <- min(table(songs_meta$decade)[dec_oi])
  min_songs

  songs_oi <- split(1:nrow(songs_meta), songs_meta$decade)
  songs_oi <- songs_oi[dec_oi]
  songs_oi <- unlist(lapply(songs_oi, sample, min_songs))

  songs_oi
}


set.seed(1234)
songs_oi <- sample_songs()

table(songs_meta$decade[songs_oi])



### Classifier on PCA features: ######################################

songs_pca <- cbind(decade = songs_meta[songs_oi, "decade"],
                   songs_feat_pca[songs_oi, 1:10])
songs_pca$decade <- songs_pca$decade[, drop = TRUE]


### First check:
d <- dist(songs[, -1])
o <- seriate(d)
pimage(d, o)
#-> Ouch :-(


### Classifier:
c1 <- randomForest(decade ~ ., data = songs_pca)
c1



### Classifier on orig features: #####################################

songs_full <- cbind(decade = songs_meta[songs_oi, "decade"],
                    songs_feat_full[songs_oi, ])
songs_full$decade <- songs_full$decade[, drop = TRUE]


### Classifier:
c2 <- randomForest(decade ~ ., data = songs_full)
c2


