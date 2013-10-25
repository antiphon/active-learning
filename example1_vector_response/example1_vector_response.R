#' Example 1: Why use vector valued responses.
#' 
#' The idea: If the user is forced to sample the vector, 
#' the learning of the classifier will be slower.

#' load data
source("data.R")

#' load labellers:
source("labellers.R")

oracle_forced <- create_oracle_with_known_label_realize(data, "bottom")
oracle_free <- create_oracle_with_known_label(data, "bottom")

#' the fitting of NB with gaussian components:
source("fitting.R")

#' Then see what happends to the model when we increase the sample by random sampling
source("active_learning_loop.R")

#' First, with the forced-to-choose oracle
NN <- 30
set.seed(1234)
list_free <- list()
list_forced <-list()
for(j in 1:20){
  # add randomness to the sampling
  o <- sample(1:nrow(dataUnlabeled))
  dataUnlabeled<-dataUnlabeled[o,]
  true_labels <- true_labels[o]
  
  ## fit with the labeller
  res_free <- active_learning(data0, dataUnlabeled, oracle=oracle_free, true_labels, 
                              nsteps=NN, I='first', alpha_prior=NULL, est_alpha=F,
                              test_data=test_data)
  list_free[[j]] <- res_free
  ## then the randomness from the forced labeller's side
  reslist <- list()
  for(i in 1:10) {
    res_forced <- active_learning(data0, dataUnlabeled, oracle=oracle_forced, true_labels, 
                                  nsteps=NN, I='first', alpha_prior=NULL, est_alpha=F,
                                  test_data=test_data)
    reslist[[i]] <- res_forced
  }
  
  list_forced[[j]] <- reslist
  cat(paste0("** ", j, " **\n" ))
}

#sav
save(file="20x10x30.rda", list_forced, list_free)

############################################
#' PLOT


free_testerror <- apply(te<-sapply(list_free, function(z) z$test_error_hist), 1, 
                          quantile, prob=c(0.05, 0.5, 0.95))
v <- free_testerror[2,]
tf <-lapply(list_forced, function(z) sapply(z, getElement, 'test_error_hist'))
tf <- (do.call(cbind, lapply(tf, rowMeans)))
forced_testerror <- apply(tf, 1, quantile, prob=c(0.05, 0.5, 0.95))

#
pdf(file="choose1_vs_give_vector.pdf", width=5, height=4)
ts.plot(v, ylim=c(0,1), col=2, xlab="sample size", ylab="test set classification rate", xlim=c(3,25), lty=2)
polygon(c(1:NN, NN:1), c(free_testerror[1,], rev(free_testerror[3,])), 
        border=rgb(1,.9,.9), col=rgb(1,.9,.9, 0.5))

polygon(c(1:NN, NN:1), c(forced_testerror[1,], rev(forced_testerror[3,])), 
          border=rgb(.9, .9, 1), col=rgb(.9,.9, 1, 0.75))
lines(forced_testerror[2,], col="blue")
lines(v, col=2, lty=2)

legend("bottomright", bty="n", c("Forced to choose", "Return a vector"), col=c("blue",2), lty=1:2)
dev.off()

## 

# source("plot_functions.R")
# plotd(res_forced, data, col="white"); title("Forced to choose.")
# plotd(res_free, data, col="white"); title("Free to say \"I don't know\".")

