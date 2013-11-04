
library("RSQLite")
library("rhdf5")  # via BioConductor

ADD_FILES <- "extdata/MillionSongSubset/AdditionalFiles/"
H5_FILES <- "extdata/MillionSongSubset/data"



### Make song selection: #############################################

db_meta <- dbConnect(dbDriver("SQLite"),
                     file.path(ADD_FILES, "subset_track_metadata.db"))
songs <- dbReadTable(db_meta, "songs")
dbDisconnect(db_meta)


songsOK <- subset(songs, year > 0)
songsOK$decade <- 
  ordered(sprintf("%s0s", substr(as.character(songsOK$year), 1, 3)))

str(songsOK)



### Read song features: ##############################################

read_feat <- function(file) {
  l <- h5read(file, "analysis")
  l$songs <- NULL
  l
}


paths <- substr(songsOK$track_id, 3, 5)
paths <- lapply(strsplit(paths, ""), paste, collapse = "/")
paths <- sprintf("%s/%s/%s.h5", H5_FILES, paths, songsOK$track_id)


