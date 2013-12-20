#' Example 1: Why use vector valued responses.
#' 
#' The idea: If the user is forced to sample the vector, 
#' the learning of the classifier will be slower.
#' v2: use packages
#' load data
#' 
#' No user estimation in this one.
#' 
source("../example2_combined/data.R")

#' load labellers:
source("labellers.R")


oracle_forced <- create_oracle_hole_realize(data) #with_known_label_realize(data, nc<-"middle")
oracle_free <- create_oracle_hole(data) #with_known_label(data, nc)
oracle <- create_oracle_all_knowing(data)
#' Then see what happends to the model when we increase the sample by random sampling
source("../algorithm/active_learning_loop_v2.R")


## The fitting function.
AL <- function(d, oracle)
  active_learning(d$data0, d$dataUnlabeled, oracle=oracle, d$true_labels, 
                nsteps=NN,  est_alpha=F,
                combineI=function(m,l) sample(m),
                test_data=d$test_data, debug=F)
#' Run. Upto NN samplings per session, repeat each session J times,  J*I times for forced.
NN <- 25
list_best <- list()
list_free <- list()
list_forced <-list()
J <- 50
I <- 20
for(j in 1:J){
  ## add randomness by sampling the initial data:
  set.seed(1234+j)
  init_idx <- unlist(lapply(split(1:nrow(X),y), sample, 2))
  d <- split_data(init_idx)
  # mess things up by feeding the classifier randomized labels
  #d$data0[,1] <- sample(d$data0[,1])
  #
  ## fit with oracle
  res_best <- AL(d, oracle)
  list_best[[j]] <- res_best
  ## fit with the labeller
  res_free <- AL(d, oracle_free)
  list_free[[j]] <- res_free
  ## then the randomness from the forced labeller's side
  reslist <- list()
  for(i in 1:I) {
    res_forced <- AL(d, oracle_forced)
    reslist[[i]] <- res_forced
  }
  list_forced[[j]] <- reslist
  ##
  cat(paste0("** ", j, " **\n" ))
}

#sav
# save(file="20x10x30_v2.rda", list_forced, list_free)

############################################
#' PLOT
opt <- sapply(list_best, function(z) z$test_error_hist)
opt_testerror <- apply(opt, 1, quantile, prob=probs <- c(.05, .5, .95))
v <- opt_testerror[2,] <- rowMeans(opt)


te <- sapply(list_free, function(z) z$test_error_hist)
free_testerror <- apply(te, 1, quantile, prob=probs)
free_testerror[2,] <- rowMeans(te)

tf <-lapply(list_forced, function(z) sapply(z, getElement, 'test_error_hist'))
tf <- do.call(cbind, lapply(tf, rowMeans)) #
forced_testerror <- apply(tf, 1, quantile, prob=probs)
forced_testerror[2,] <- rowMeans(tf)

#
M <- 0#v #free_testerror[2,]
pdf(file="choose1_vs_give_vector_v2.pdf", width=5, height=4)
par(mfrow=c(1,1))
plot(0:NN, free_testerror[2,]-M, ylim=c(0,1), col=2, xlab="sample size", 
     ylab="precision", xlim=c(0, NN), lty=2, type="l")#b", pch=1+18*(z>0.5))
# polygon(c(0:NN, NN:0), c(free_testerror[1,]-M, rev(free_testerror[3,]-M)), 
#         border=rgb(1,.9,.9), col=rgb(1,.9,.9, 0.5))
polygon(c(0:NN, NN:0), c(forced_testerror[1,]-M, rev(forced_testerror[3,]-M)), 
        border=rgb(.9, .9, 1), col=rgb(.9,.9, 1, 0.75))

lines(0:NN, free_testerror[2,]-M, lty=2, col=2)
lines(0:NN, forced_testerror[2,]-M, col="blue")
lines(0:NN, opt_testerror[2,]-M, col="gray20", lty=3, lwd=2)

legend("bottomright", bty="n", 
       c("Forced to choose", "Return a vector", "Oracle"), col=c("blue",2, "gray20"), lty=1:3, lwd=2)
dev.off()

## 
# classifier_plot(res_best$classifier, d$data1)
# symbols(co[1],co[2], circles=ro, inches=F, add=T, lty=2)


# source("plot_functions_v2.R")
# par(mfrow=c(2,1))
# plotd(list_best[[J]], d$data1, zlim=c(-10,0))
# plotd(list_forced[[J]][[1]], d$data1)


