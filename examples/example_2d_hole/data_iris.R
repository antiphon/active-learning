#' the data for this example

## Generate some data
data(iris)

#' drop to 2d
X <- data.frame(cmdscale(dist(iris[,1:4])))
y <- iris[,5]

data <- cbind(y=y, X=X)

#' clean training data
l0<- c(1, 2, 51, 52, 101, 102)
LX <- X[l0, ]
Ly <- y[l0]
data0 <- cbind(label=Ly, LX)

#' test
t1 <- sample(setdiff(1:nrow(X), l0), 30)
test_data <- cbind(label=y[t1], X[t1,])
l0 <- c(l0, t1)
#' rest
dataUnlabeled <- X[-l0, ]
true_labels <- y[-l0]
