
dat <- read.table("extdata/Music_features_17_10_2005/music_data_17_10_05.exa",
                  sep = " ", header = FALSE, stringsAsFactors = FALSE)

dat$V38 <- dat$V39 <- dat$V42 <- dat$V43 <- dat$V46 <- dat$V47 <- dat$V52 <- NULL

dat$V51 <- factor(dat$V51)

names(dat) <- c("id", sprintf("feature%s", 1:(ncol(dat)-2)), "genre")


saveRDS(dat, file = "data/dortmund-features.Rds")


# meta <- readLines("extdata/Music_features_17_10_2005/music_data_17_10_05.att")
# w <- grepl("name", meta)
# n <- sub(".*name.*= \"(.*)\".*", "\\1", meta[w])
# 
# length(n)
# 
# colnames(dat) <- n
# 
# dim(dat)
# table(dat$label)
# 
# which(is.na(dat))

