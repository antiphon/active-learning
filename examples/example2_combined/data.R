#' the data for this example

## Generate some data
set.seed(123456)

X <- data.frame( rbind(cbind(x=rnorm(50, -1, .6), y=rnorm(50, 0, 1) ),
                       cbind(x=rnorm(50, 0, 1), y=rnorm(50, -1, .3) ),
                       cbind(x=rnorm(50, 3, .6), y=rnorm(50, -1, 1) )))#,
                       #cbind(x=rnorm(50, 0, 1), y=rnorm(50, 1, .3) )))
y <- factor(rep(1:3, each=50), labels=c("left", "middle", "right"))

data <- cbind(y=y, X=X)

#' clean training data
l0<- c(1, 2, 51, 52, 101, 102)#, 151, 152)
LX <- X[l0, ]
Ly <- y[l0]
data0 <- cbind(label=Ly, LX)

#' shuffle
set.seed(12345)
o <- sample(1:nrow(X))
data <- data[o,]
X<-X[o,]
y<-y[o]

#' test data
t1 <- sample(setdiff( 1:nrow(X), l0), 40)
test_data <- data[t1,]
l0 <- c(l0, t1) 
#' rest
dataUnlabeled <- X[-l0, ]
true_labels <- y[-l0]

data1 <- cbind(y=true_labels, X=dataUnlabeled)