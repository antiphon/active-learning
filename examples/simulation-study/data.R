#' 
#' 

data_all <- readRDS("../../experiment/musicpool/description_scaled.Rds")

data <- data_all[,-c(1,45)]
y <- data$genre
X <- data[,-43]

# test set
test_idx <- unlist( lapply(split(1:nrow(X), y), sample, 20 ) )
Xtest <- X[test_idx,]
ytest <- y[test_idx]
data_test <- data.frame(y=ytest, Xtest)

# pool
Xpool <- X[pool_idx <- setdiff(1:nrow(X), test_idx), ]
ypool <- y[pool_idx]
data_pool <- data.frame(y=ypool, Xpool)

# initial data
init_idx <- unlist( lapply(split(1:nrow(Xpool), ypool), sample, 5 ) )
Xinit <- Xpool[init_idx, ]
yinit <- ypool[init_idx]
data_init <- data.frame(y=yinit, Xinit)
