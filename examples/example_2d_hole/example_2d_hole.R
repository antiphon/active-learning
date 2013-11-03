#' Two D example of estimating the user uncertainty alpha(x)

source("data.R")

#' Generate the uncertainty area for the labeller
source("alpha.R")
co_circle <- c(1, -1)
co_radius <- 1.3
alpha <- sim_hole_alpha(co_circle, co_radius, v_in=0.01, v_out=50)

## Check alpha
# xy <- expand.grid(xsteps<-seq(-4,6, length=50), ysteps<-seq(-3,3, length=30))
# v<-matrix(alpha(xy), ncol=30)
# image(v, x=xsteps, y=ysteps, asp=1)
# points(X, col=y)
## works.

#' Setup the oracle
source("oracles.R")

oracle1 <- create_oracle_with_alpha(alpha, data)

#' Now we see what happends
source("active_learning_loop.R")
eprior <- list(range=0.6, mu=0, kappa=1, s2=10, nugget=.5)
#' with user model entropy
res_just_user <- active_learning(data0, dataUnlabeled, oracle=oracle1, true_labels=true_labels,
                       alpha_prior=eprior, nsteps=NN<-20, combineI=function(a,b) -b)
#' without user model
res_just_machine <- active_learning(data0, dataUnlabeled, oracle=oracle1, true_labels=true_labels,
                       alpha_prior=eprior, nsteps=NN, est_alpha=F, combineI=function(a,b) a)
res_both <- active_learning(data0, dataUnlabeled, oracle=oracle1, true_labels=true_labels,
                        alpha_prior=eprior, nsteps=NN, 
                            combineI=function(a,b) sqrt(a*b)   )



#' Diagnose:
source("plot_functions.R")
par(mfrow=c(2,2), cex=0.7)
plot1(res_just_user); title("just user")
plot1(res_just_machine); title("just machine")
plot1(res_both); title("both")
ts.plot(res_just_user$error_hist, ylim=c(0,1))
lines(res_just_machine$error_hist, col=2)
lines(res_both$error_hist, col=3)
legend("bottom", c("just user", "just machine", "both"), col=1:3, lty=1, ncol=3, cex=.7)

#' add full data training error
library(e1071)
tm <- naiveBayes(y~., data=data)
te <- mean(predict(tm, data)==data[,1])
abline(h=te, col="gray60")

