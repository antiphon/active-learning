#' Example 1: Estimate the labeller's uncertainty.
#' 

#' load data
#source("data.R")
source("../example2_combined/data.R")
#' load labellers:
source("labellers.R")
source("../example2_combined/labellers.R")
#oracle <- create_oracle_with_known_label(data, "bottom")
co <- c(1.7,-1)
ro <- 1.4
oracle <- create_oracle_with_hole(data, co, ro, 0.001, 30)
#' the fitting of NB with gaussian components:
setwd("../algorithm/")
source("active_learning_loop.R")
setwd("../example1_vector_response/")

#' Estimate the labeller
NN <- 10
  
## fit with the labeller

eprior <- list(range=0.5, mu=3, kappa=1, s2=10, nugget=2)

set.seed(123)
res1 <- active_learning(data0, dataUnlabeled, oracle=oracle, true_labels, 
                            nsteps=NN, I='first', alpha_prior=eprior, est_alpha=T,
                            test_data=test_data)

res2 <- active_learning(data0, dataUnlabeled, oracle=oracle, true_labels, 
                        nsteps=NN, I='first', alpha_prior=eprior, est_alpha=T,
                        test_data=test_data, result=res1)

## Plot the field
source("plot.R")
pdf(file="estimated_alpha.pdf", width=9, height=3.5)
par(mfrow=c(1,2), mar=c(1,1,1,1), oma=c(1,1,1,1))
plot1(res1, data1, reso=50); title("after 10 queries")
symbols(co[1], co[2], circles=ro, add=T, lty=2, inches=F)
plot1(res2, data1, reso=50); title("after 20 queries")
symbols(co[1], co[2], circles=ro, add=T, lty=2, inches=F)
dev.off()
