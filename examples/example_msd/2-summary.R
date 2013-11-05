

library("RSQLite")


### Data: ############################################################

ADD_FILES <- "extdata/MillionSongSubset/AdditionalFiles/"


db_meta <- dbConnect(dbDriver("SQLite"),
                     file.path(ADD_FILES, "subset_track_metadata.db"))
songs <- dbReadTable(db_meta, "songs")
dbDisconnect(db_meta)


db_term <- dbConnect(dbDriver("SQLite"),
                     file.path(ADD_FILES, "subset_artist_term.db"))
mbtags <- dbReadTable(db_term, "artist_mbtag")
dbDisconnect(db_term)


str(songs)
str(mbtags)



### Metadata: ########################################################

head(sort(table(mbtags$mbtag), decreasing = TRUE), 20)

# Tags/Genres only for artists, not for songs


## Year/Decade:

sort(table(songs$year))
sort(table(songs$year > 0))

songsOK <- subset(songs, year > 0)

sort(table(songsOK$year))

songsOK$decade <- 
  ordered(sprintf("%s0s", substr(as.character(songsOK$year), 1, 3)))
          
table(songsOK$decade)


