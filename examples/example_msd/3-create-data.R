
library("RSQLite")
library("rhdf5")  # via BioConductor
library("classInt")

ADD_FILES <- "extdata/MillionSongSubset/AdditionalFiles/"
H5_FILES <- "extdata/MillionSongSubset/data"



### Song selection: ##################################################

db_meta <- dbConnect(dbDriver("SQLite"),
                     file.path(ADD_FILES, "subset_track_metadata.db"))
songs <- dbReadTable(db_meta, "songs")
dbDisconnect(db_meta)


songsOK <- subset(songs, year > 0)
songsOK$decade <- 
  ordered(sprintf("%s0s", substr(as.character(songsOK$year), 1, 3)))

str(songsOK)



### Basic song features: #############################################

### http://labrosa.ee.columbia.edu/millionsong/pages/example-track-description

read_song_features <- function(file) {
  s <- h5read(file, "analysis")
  
  l <- as.list(as.data.frame(t(s$segments_pitches)))
  names(l) <- sprintf("segments_pitches%s", 1:length(l))
  s <- c(s, l)

  l <- as.list(as.data.frame(t(s$segments_timbre)))
  names(l) <- sprintf("segments_timbre%s", 1:length(l))
  s <- c(s, l)

  s$songs <- NULL
  s$segments_pitches <- NULL
  s$segments_timbre <- NULL
  s$sections_confidence <- NULL
  s[grepl("_start", names(s))] <- NULL
  
  s
}


bin_feature <- function(f, k) {
  y <- round(seq(1, length(f), length.out = k+1))  
  idx <- cut(1:length(f), y, include.lowest = TRUE)
  unname(tapply(f, idx, mean))
}


compress_song_features <- function(s, k = 10) {
  y <- try(unlist(lapply(s, bin_feature, k)))
  if ( class(y) == "try-error" ) {
    y <- NULL
  }
  y
}


paths <- substr(songsOK$track_id, 3, 5)
paths <- lapply(strsplit(paths, ""), paste, collapse = "/")
paths <- sprintf("%s/%s/%s.h5", H5_FILES, paths, songsOK$track_id)

songs <- lapply(paths, read_song_features)
songs_compr <- lapply(songs, compress_song_features)

bad_songs <- which(sapply(songs_compr, is.null))

songs_compr <- songs_compr[-bad_songs]
songs_compr <- do.call(rbind, songs_compr)
songs_compr <- as.data.frame(songs_compr)

songsOK <- songsOK[-bad_songs]


### Export data:

saveRDS(songsOK, file = "data/msd-metadata.Rds")
saveRDS(songs_compr, file = "data/msd-features-full.Rds")



### PCA: #############################################################

songs_pca <- prcomp(songs_compr, scale = TRUE)
songs_pca <- as.data.frame(songs_pca$x)


### Export data:

saveRDS(songs_pca, file = "data/msd-features-pca.Rds")


### Check the PCs:

library("seriation")
d <- dist(songs_pca[, 1:10])
pimage(d)

