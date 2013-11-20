
### Download "Music Audio Benchmark Data Set": #######################

src <- "http://www-ai.cs.uni-dortmund.de/AUDIO/"
dest <- "dortmund/"

system(sprintf("wget -r -nd -l1 -P %s %s", src, dest), wait = TRUE)



### Create musicpool: ################################################

dir.create("musicpool")
dir.create("musicpool/mp3s")
dir.create("sessions")

zip_files <- c("alternative", "blues", "electronic", "folkcountry",
               "funksoulrnb", "jazz", "pop", "raphiphop", "rock")
zip_files <- sprintf("%s%s.zip", dest, zip_files)

lapply(zip_files, unzip, exdir = "musicpool/mp3s")



### Create pool description: #########################################

dat <- read.table(unz("dortmund/Music_features_17_10_2005.zip", "music_data_17_10_05.exa"),
                  sep = " ", header = FALSE, stringsAsFactors = FALSE)

dat$V34 <- dat$V38 <- dat$V39 <- dat$V42 <- dat$V43 <- dat$V46 <- dat$V47 <- dat$V52 <- NULL

dat$V51 <- factor(dat$V51)

names(dat) <- c("id", sprintf("feature%s", 1:(ncol(dat) - 2)), "genre")


### Map ids to files:

files <- list.files("musicpool/mp3s", pattern = ".mp3")
w <- lapply(dat$id, agrep, files, value = TRUE)

stopifnot(all(!sapply(w, is.null)))

w <- unlist(w)

stopifnot(length(w) != nrow(w))

dat$file <- w


### Save musicpool description:

saveRDS(dat, file = "musicpool/description.Rds")


### Scale the features
dat_scaled <- dat
dat_scaled[,-c(1, 44, 45)] <- scale(dat[,-c(1, 44, 45)])

saveRDS(dat_scaled, file = "musicpool/description_scaled.Rds")

### Choose an initial set of items for the classifier's 'seed'
data0 <- dat_scaled[,-c(1,45)]
y0 <- data0[,43]
X0 <- data0[,-43]

# take a balanced sample
set.seed(1234)
idx <- c(sapply(split(1:nrow(X0), y0), sample, 3))
y<-y0[idx]
X<-X0[idx,]
saveRDS(list(y=y, X=X, idx=idx), file = "initial_data_for_classifier.Rds")



