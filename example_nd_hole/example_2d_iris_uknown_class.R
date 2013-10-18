#' Two D example of estimating the user uncertainty alpha(x)
#
#' NOT GOOD BEFORE CORRELATION ADDED TO  CLASSIFIER MODEL (-> slanted clusters)

source("data_iris.R")

#' Setup the oracle
source("oracles.R")

oracle1 <- create_oracle_with_unknown_label(data, class="virginica")

#' Now we see what happends
source("active_learning_loop.R")
eprior <- list(range=0.3, mu=-1, kappa=1, s2=10, nugget=2)
#' with user model entropy
res_just_user <- active_learning(data0, dataUnlabeled, oracle=oracle1, true_labels=true_labels,
                       alpha_prior=eprior, nsteps=NN<-25, combineI=function(a,b) -b)
#' without user model
res_just_machine <- active_learning(data0, dataUnlabeled, oracle=oracle1, true_labels=true_labels,
                       alpha_prior=eprior, nsteps=NN, est_alpha=F, combineI=function(a,b) a)

res_both <- active_learning(data0, dataUnlabeled, oracle=oracle1, true_labels=true_labels,
                        alpha_prior=eprior, nsteps=NN, 
                            combineI=function(a,b) rowMeans(cbind(rank(a),rank(-b))), 
                            )

#' Diagnose:
source("plot_functions.R")
par(mfrow=c(2,2), cex=0.7)
plot1(res_just_user, F); title("just user")
plot1(res_just_machine, F); title("just machine")
plot1(res_both, F); title("both")
ts.plot(res_just_user$error_hist, ylim=c(0,1))
lines(res_just_machine$error_hist, col=2)
lines(res_both$error_hist, col=3)
legend("bottom", c("just user", "just machine", "both"), col=1:3, lty=1, ncol=3, cex=.7)

#' add full data training error
library(e1071)
tm <- naiveBayes(y~., data=data)
te <- mean(predict(tm, data)==data[,1])
abline(h=te, col="gray60")

