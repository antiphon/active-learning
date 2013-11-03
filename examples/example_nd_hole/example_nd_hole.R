#' Iris: 5D example of estimating the user

source("data.R")

#' Setup the oracle
source("oracles.R")
oracle1 <- create_oracle_with_unknown_label(data, class="virginica")

#' Now we see what happends
source("active_learning_loop.R")
eprior <- list(range=0.4, mu=0, kappa=1, s2=10, nugget=.5)
NN <- 20

res_just_user <- active_learning(data0, dataUnlabeled, 
                                    true_labels=true_labels,
                                    oracle=oracle1, 
                                    alpha_prior=eprior, 
                                    est_alpha=T,
                                    combineI=function(a,b) -b,
                                    test_data=test_data,
                                    nsteps=NN)

res_just_machine <- active_learning(data0, dataUnlabeled, 
                            true_labels=true_labels,
                            oracle=oracle1, 
                            alpha_prior=eprior, 
                            est_alpha=F,
                            combineI=function(a,b) a,
                            test_data=test_data,
                            nsteps=NN)



res_both <- active_learning(data0, dataUnlabeled, 
                            true_labels=true_labels,
                            oracle=oracle1, 
                            alpha_prior=eprior, 
                            combineI=function(a,b)a-b,
                            test_data=test_data,
                            nsteps=NN)


## 
source("plot_functions.R")
grid_plot(res_just_user, data)
grid_plot(res_just_machine, data)
grid_plot(res_both, data)

# train error
ts.plot(res_just_user$training_error_hist, ylim=c(0,1), main="training rate")
lines(res_just_machine$training_error_hist, col=2)
lines(res_both$training_error_hist, col=3)

library(e1071)
tm <- naiveBayes(label~., data=data_train)
te <- mean(predict(tm, data_train)==data_train[,1])
abline(h=te, col="gray60")

## Test error
ts.plot(res_just_user$test_error_hist, ylim=c(0,1), main="test rate")
lines(res_just_machine$test_error_hist, col=2)
lines(res_both$test_error_hist, col=3)
tm <- naiveBayes(label~., data=data_train)
te <- mean(predict(tm, test_data)==test_data[,1])
abline(h=te, col="gray60")



