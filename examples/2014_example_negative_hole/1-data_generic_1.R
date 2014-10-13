#' the dataset we want to classify

## Generate some data
set.seed(1234569)

X <- data.frame( rbind(cbind(x=rnorm(50, -1, .16), y=rnorm(50, 0, 1) ),
                       cbind(x=rnorm(50, 0, 1), y=rnorm(50, -1, .3) ),
                       cbind(x=rnorm(50, 3, .6), y=rnorm(50, 0, 1) ),
                       cbind(x=rnorm(50, 1.5, 1), y=rnorm(50, 1.5, .3) )))
y <- factor(rep(1:4, each=50), labels=c("left", "bottom", "right", "top"))

data <- cbind(y=y, X=X)

#' clean training data, 2 per cluster
l0<- c(1, 2, 51, 52, 101, 102, 153, 152)
LX <- X[l0, ]
Ly <- y[l0]
data0 <- cbind(label=Ly, LX)

#' piece out a test set
test_i <- sample(setdiff(1:nrow(data), l0), 30)
testX <- X[test_i,]
testy <- y[test_i]
testdata <- data[test_i, ]
l1 <- c(l0, test_i)
#' Pool
dataUnlabeled <- X[-l1, ]
true_labels <- y[-l1]

