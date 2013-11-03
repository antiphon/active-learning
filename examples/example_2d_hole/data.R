#' the data for this example

## Generate some data
set.seed(123456)

X <- data.frame( rbind(cbind(x=rnorm(50, -1, .6), y=rnorm(50, 0, 1) ),
                       cbind(x=rnorm(50, 0, 1), y=rnorm(50, -1, .3) ),
                       cbind(x=rnorm(50, 3, .6), y=rnorm(50, 0, 1) ),
                       cbind(x=rnorm(50, 0, 1), y=rnorm(50, 1, .3) )))
y <- factor(rep(1:4, each=50), labels=c("left", "bottom", "right", "top"))

data <- cbind(y=y, X=X)

#' clean training data
l0<- c(1, 2, 51, 52, 101, 102, 151, 152)
LX <- X[l0, ]
Ly <- y[l0]
data0 <- cbind(label=Ly, LX)

#' rest
dataUnlabeled <- X[-l0, ]
true_labels <- y[-l0]

co_circlec <- c(0.7,-1)
co_radius <- 1.5
