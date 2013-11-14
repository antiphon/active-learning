
### Download "Music Audio Benchmark Data Set": #######################

src <- "http://www-ai.cs.uni-dortmund.de/AUDIO/"
dest <- "dortmund/"

system(sprintf("wget -r -nd -l1 -P %s %s", src, dest), wait = TRUE)



### Create musicpool: ################################################

dir.create("musicpool")
dir.create("sessions")

zip_files <- c("alternative", "blues", "electronic", "folkcountry",
               "funksoulrnb", "pop", "raphiphop", "rock")  # jazz
zip_files <- sprintf("%s%s.zip", dest, zip_files)

lapply(zip_files, unzip, exdir = "musicpool")



### Create pool description: #########################################

dat <- read.table(unz("dortmund/Music_features_17_10_2005.zip", "music_data_17_10_05.exa"),
                  sep = " ", header = FALSE, stringsAsFactors = FALSE)

dat$V34 <- dat$V38 <- dat$V39 <- dat$V42 <- dat$V43 <- dat$V46 <- dat$V47 <- dat$V52 <- NULL

dat$V51 <- factor(dat$V51)

names(dat) <- c("id", sprintf("feature%s", 1:(ncol(dat) - 2)), "genre")


### Remove "jazz" because files are missing:

dat <- subset(dat, genre != "jazz")
dat$genre <- droplevels(dat$genre)


### Map ids to files:

files <- list.files("musicpool/", pattern = ".mp3")
w <- lapply(dat$id, agrep, files, value = TRUE)

stopifnot(all(!sapply(w, is.null)))

w <- unlist(w)

stopifnot(length(w) != nrow(w))

dat$file <- w


### Save musicpool description:

saveRDS(dat, file = "musicpool/description.Rds")


