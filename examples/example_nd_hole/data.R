#' the data for this example

## Iris
data(iris)
## drop the duplicate
iris[-102,]

X <- data.frame(iris[,-5])
## normalize the data
X <- data.frame(scale(X))
y <- iris$Species

data <- cbind(y=y, X=X)

#' clean training data
l0<- c(1, 2, 51, 52, 101, 102)
LX <- X[l0, ]
Ly <- y[l0]
data0 <- cbind(label=Ly, LX)

#' test data
set.seed(1234)
t1 <- sample(setdiff(1:nrow(X), l0), 20)
test_X <- X[t1,]
test_y <- y[t1]
test_data <- cbind(test_y, test_X)
#' rest
dataUnlabeled <- X[-c(l0,t1), ]
true_labels <- y[-c(l0,t1)]
data_train <- cbind(label=true_labels, dataUnlabeled)
