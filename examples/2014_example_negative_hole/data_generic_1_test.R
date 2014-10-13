#' test if the data_generic_1 is good for our purposes, i.e.
#' it has potential to show improvement on learning:
source("data_generic_1.R")
library(alclassifier)
source("oracle.R")
o <- create_oracle(X, y)

c1 <- classifier_initial(LX, Ly)

#' add all data
h <- o(X)$h
c2 <- classifier_update(c1, X, h)
classifier_plot(c1, data)

#'
#' The optimal learning curve as given by the oracle:
#' 
source("../algorithm/active_learning_loop_v3.R")
v <- active_learning(data0, dataUnlabeled, o, est_alpha = F, true_labels, nsteps = 150, combineI = function(a,b) a, test_data = testdata)

par(mfrow=c(2,1))
plot(v$test_error_hist, type="l")
classifier_plot(v$classifier, data, asked=v$asked)

#' do we see marked increase in classification fitness?