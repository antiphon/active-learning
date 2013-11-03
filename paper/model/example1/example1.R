#' initial data
# data(iris)
# l0<- c(1, 2, 51, 52, 101, 102)
# LX <- iris[l0,-(yi<-which(names(iris)=="Species"))]
# Ly <- iris[l0, yi]
# #' rest
# U <- data[-l0,1:4]
# true_labels <- iris$Species[-l0]
# CLASSES <- levels(iris$Species)

## Generate some data
set.seed(12345)

X <- data.frame( rbind(cbind(x=rnorm(50, -1, .6), y=rnorm(50, 0, 1) ),
            cbind(x=rnorm(50, 0, 1), y=rnorm(50, -1, .3) ),
            cbind(x=rnorm(50, 3, .6), y=rnorm(50, 0, 1) )))
y <- factor(rep(1:3, each=50), labels=c("left", "bottom", "right"))

#' clean training data
l0<- c(1, 2, 51, 52, 101, 102)
LX <- X[l0, ]
Ly <- y[l0]

#' rest
U <- X[-l0, ]
true_labels <- y[-l0]
CLASSES <- levels(y)
known_label <- "bottom"
data0 <- list(X=X, y=y)

DEBUG <- FALSE
cat2 <- if(DEBUG) cat else function(x) NULL

#
source("fit_and_update_naive_bayes.R")
source("predict_and_informativeness.R")
source("oracles.R")
cautious_oracle <- cautious_oracle_limited
#

## now we test:
res <- active_learning(LX, Ly, U, true_labels, nstep=nsteps <- 30)
res_oracle <- active_learning(LX, Ly, U, true_labels, nstep=nsteps, oracle=TRUE)
res_e <- active_learning(LX, Ly, U, true_labels, nstep=nsteps, realize=TRUE)




## plot learning rate
par(mfrow=c(2,2))
ts.plot(res$error_hist, ylim=c(0,1), xlab="iteration", 
        ylab="classification rate", col=3)
lines(res_oracle$error_hist, col=2)
lines(res_e$error_hist, col=3, lty=2)

# full data classification rate
f0 <- fitM0(data0$X, data0$y)
abline(h=mrate <- mean(classify(data0$X, f0)==data0$y))
legend("bottom", c("oracle", "caut. oracle", "caut. oracle, realize"), lty=c(1,1,2), col=c(2:3,3), cex=0.7)

# plot sampling
source("plot_functions.R")
plot1(res_oracle, U, main="Oracle")
plot1(res, U, main="Restricted oracle")
plot1(res_e, U, main="Resticted oracle, realize probabilities")




