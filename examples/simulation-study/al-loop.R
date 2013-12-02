#'


library(allabeller)
library(alclassifier)

source("data.R")



## designing labeller
# set.seed(1234)
# D <- as.matrix(dist(Xpool))
# nn<-apply(D, 1, function(d) sum(d<4))
# 
# nneighs<-apply(D, 1, function(d) (d<4))
# 
# neighs <- sapply(1:nrow(nneighs) , function(z) which(nneighs[z,]))
# 
# knownclasses <- levels(ypool)[c(2,5,9)]
# 
# nclass_in_neighs <- list()
# for(i in 1:length(knownclasses))
# nclass_in_neighs[[i]] <- sapply(neighs[ypool==knownclasses[i]], function(l) length(table(c(ypool[l]))))
# names(nclass_in_neighs)<- knownclass
# 
# iknown<-NULL
# for(i in 1:length(knownclasses))
# iknown[i] <- sample(which(ypool==knownclasses[i])[nclass_in_neighs[[i]]==2], 1)
set.seed(1234)
iknown <- sample(1:nrow(Xpool), 6)#c(19, 538, 1529)

Xknown <- Xpool[iknown,]

  
source("../algorithm/shot_noise_labeller.R")
labeller <- create_shot_noise_labeller(Xpool, ypool, Xknown, 
                                       kernel=kernel_make("gaussian", 5), flatten=TRUE)
## labeller initialized.
#
source("../algorithm/active_learning_loop_v2.R")

# priors
lpars <- parameters_default()
lpars$upper[3] <- 50
NN <- 50
set.seed(1234)
just_machine <- active_learning(data_init, Xpool, labeller, ypool, nsteps=NN, 
                                combineI=function(m,l) m, 
                                est_alpha=F, test_data=data_test, 
                                #begin_with=init_idx,
                                debug=T, labeller_parameters=lpars)


# estimate hypers
# lpars2 <- lpars
# lpars2$estimate_hyper <- TRUE
# lpars2$verb <- TRUE
# lpars2$hyper_prior$range_a <- 10
# lpars2$hyper_prior$range_b <- 1
# just_machine$est_alpha <- TRUE
# just_machine$labeller$parameters <- lpars2
# just_machine$labeller$data_h <- just_machine$classifier$data_h
# just_machine$labeller$data_X <- just_machine$classifier$data_X
# 
# temp <- active_learning(data_init, Xpool, labeller, ypool, nsteps=1, 
#                    combineI=function(m,l) m, 
#                    est_alpha=T, test_data=data_test, 
#                    result=just_machine,
#                    debug=T, labeller_parameters=lpars2)
# 
# 
#

lpars3 <- lpars
lpars3$hyper <- list(range=10, kappa=1, s2=0.1, nugget=.3) #temp$labeller$parameters$hyper
#lpars3$hyper_prior$tau0 <- 1e9
# no try with the labeller estimate
set.seed(1234)
with_labeller <- active_learning(data_init, Xpool, labeller, ypool, nsteps=NN, 
                                  combineI=function(m,l) l, 
                                  est_alpha=T, test_data=data_test, 
                                  #begin_with=init_idx,
                                  debug=T, labeller_parameters=lpars3)

M <- NN
plot(0:M, just_machine$test_error_hist[0:M+1], type="l", ylim=c(0.2,.4))
lines(0:M, with_labeller$test_error_hist[0:M+1], col=2)

#
library(e1071)
if(!exists("MEAN")) MEAN<-mean(predict(naiveBayes(y~. , data=data_pool), newdata=data_test[,-1])==data_test[,1])
abline(h=MEAN)




