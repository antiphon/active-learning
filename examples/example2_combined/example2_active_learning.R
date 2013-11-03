#' Example 2: Active learning with uncertain labeller.
#' 

#' load data
source("data.R")

#' Create the  labeller:
source("labellers.R")
co <- c(1.7,-1)
ro <- 1.4
labeller <- create_oracle_with_hole(data, co, ro, v_in=0.001, v_out=30)

#' Now let's see...
NN <- 25
setwd("../algorithm/")
source("active_learning_loop.R")
setwd("../example2_combined/")

#' hyperprior for alpha 
eprior <- list(range=0.3, mu=2, kappa=1, s2=10, nugget=2)

#' Without the user
res_just_user <- active_learning(data0, dataUnlabeled, oracle=labeller, 
                                    true_labels=true_labels, est_alpha=TRUE,
                                    alpha_prior=eprior, nsteps=NN, 
                                    combineI=function(a,b) b,
                                    test_data=test_data)

res_just_machine <- active_learning(data0, dataUnlabeled, oracle=labeller, 
                                    true_labels=true_labels, est_alpha=F,
                                    alpha_prior=eprior, nsteps=NN, 
                                    combineI=function(a,b) a,
                                    test_data=test_data)

res_both <- active_learning(data0, dataUnlabeled, oracle=labeller, 
                                    true_labels=true_labels,
                                    alpha_prior=eprior, nsteps=NN, 
                                    combineI=function(a,b) sqrt(a*b),
                                    test_data=test_data)
############################################
#' PLOT
source("plot.R")
circle <- function() NULL#symbols(co[1], co[2], circles=ro, add=T, inches=F, lty=2)
#
pdf(file="3_fields_and_tsplot.pdf", width=5, height=4)
par(mfrow=c(2,2),mar=c(4,2,2,2), cex=.5)
plotd(res_just_user, data1, 1,2, reso<-50); circle(); title("Just labeller inform.")
plotd(res_just_machine, data1, 1,2,reso, col="gray90"); circle(); title("Just classifier inform.")
plotd(res_both, data1, 1,2,reso); circle(); title("Both")

## Test error rate
lc <- c("goldenrod", "maroon1", "dodgerblue")
ts.plot(res_just_user$test_error_hist, ylim=c(0,1), lty=3, col=lc[3], xlab="Iteration", main="Classification rate")
lines(res_just_machine$test_error_hist, lty=2, col=lc[2])
lines(res_both$test_error_hist, lty=1, col=lc[1])
legend("bottomright", c("just labeller", "just classifier", "both"), col=lc[3:1], lty=3:1, bty="n", cex=.8)

#' add full data training error
library(e1071)
tm <- naiveBayes(y~., data=data1)
te <- mean(predict(tm, newdata=test_data[,-1])==test_data[,1])
abline(h=te, col="gray60")

dev.off()




