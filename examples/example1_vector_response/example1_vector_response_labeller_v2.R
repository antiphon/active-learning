#' Example 1: Estimate the labeller's uncertainty.
#' 
#' v2: use the packages
#' load data
source("../example2_combined/data.R")

#' load labellers:
source("../algorithm/shot_noise_labeller.R")
##
Xknown <- rbind(c(x1=def_co[1], x2=def_co[2]))#dataUnlabeled[41,]
sigma <- def_ro
#
oracle <- create_shot_noise_labeller(dataUnlabeled, true_labels,
                                     Xknown,
                                     kernel=kernel_make("uniform", sigma))

#' the fitting of NB with gaussian components:
source("../algorithm/active_learning_loop_v2.R")

#' how many steps
NN <- 10
  
#' set up
lpars <- parameters_default()
lpars$hyper <-  list(range=1.5, kappa=1, s2=.1, nugget=.01) 
lpars$verb <- FALSE
lpars$minimum_data <- 1

#' lets go
set.seed(12345)
res1 <- active_learning(data0, dataUnlabeled, oracle=oracle, true_labels, 
                        nsteps=NN, 
                        test_data=test_data,
                        combineI=function(a,b)runif(1:length(a)),
                        labeller_parameters=lpars)
# run 2*NN more

res2 <- active_learning(data0, dataUnlabeled, oracle=oracle, true_labels, 
                        nsteps=NN, 
                        test_data=test_data, 
                        combineI=function(a,b)runif(1:length(a)),
                        result=res1)

## Plot the field
source("plot_v2.R")
ro <- sigma
co <- Xknown

zl <- c(-.8,.3)
reso <- 50
pdf(file="estimated_alpha_v2.pdf", width=9, height=3.5)
par(mfrow=c(1,2), mar=c(1,1,1,0), oma=c(1,1,1,1))
plot1(res1, data1, reso=reso, zlim=zl); title(paste0("after ", NN," queries"))
symbols(co[1], co[2], circles=ro, add=T, lty=2, inches=F)
plot1(res2, data1, reso=reso, zlim=zl); title(paste0("after ", 2*NN," queries"))
symbols(co[1], co[2], circles=ro, add=T, lty=2, inches=F)
dev.off()
