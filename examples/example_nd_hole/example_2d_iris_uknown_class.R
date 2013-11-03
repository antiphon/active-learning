#' 4D example of estimating the user uncertainty alpha(x)

source("data.R")

#' Setup the oracle
source("oracles.R")

oracle1 <- create_oracle_with_known_label(data, class="versicolor")

#' Now we see what happends
wd<-getwd()
setwd("../algorithm/")
source("active_learning_loop.R")
setwd(wd)

#eprior <- list(range=0.8, mu=-1, kappa=1, s2=10, nugget=2)
eprior <- list(range=0.1, mu=2, kappa=1, s2=10, nugget=2)

# wrapper
wrap <- function(...)
  active_learning(data0, dataUnlabeled, oracle=oracle1, test_data=test_data, 
                  true_labels=true_labels,
                  alpha_prior=eprior, nsteps=NN<-25, ...)

#' with user model entropy
res_just_user <- wrap(combineI=function(a,b) b)
#' without user model
res_just_machine <- wrap(est_alpha=F, combineI=function(a,b) a)
#' both
res_both <- wrap(combineI=function(a,b) sqrt(a*b))

#' Diagnose:
par(mfrow=c(1,1))
ts.plot(res_just_user$test_error_hist, ylim=c(0,1))
lines(res_just_machine$test_error_hist, col=2)
lines(res_both$test_error_hist, col=3)
legend("bottomright", c("just user", "just machine", "both"), col=1:3, lty=1, bty="n", cex=.7)
# 
# #' add full data training error
library(e1071)
tm <- naiveBayes(y~., data=data)
te <- mean(predict(tm, data)==data[,1])
abline(h=te, col="gray60")
# 
#source("plot_functions.R")
#
grid_plot(res_both, data_train)