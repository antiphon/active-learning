#' Demonstration one: Hole 
source("oracle.R")
source("labeller.R")
source("1-data_generic_1.R")

#' This knows everything:
oracle <- create_oracle(dataUnlabeled, true_labels)
#' This has a hole where he doesn't know:
idiot <- create_labeller_neg_hole(dataUnlabeled, true_labels, center = c(-1,-1), range=1.5)

#'
#' Check the zone-of-no-knowledge:
plot(X, asp=1, col=y)
symbols(-1, -1, circles=1.5, add=T, inches=F, bg="black")

#'
#'
#' Load the algorithms:
source("../algorithm/active_learning_loop_v3.R")

#' Set parameters, especially the hyper-prior for smoothness of the user estimate:
pars <- parameters_default()
pars$hyper$range <- .8

#' Function for running a labeller:
nsteps <- 25
run <- function(...)
  active_learning(data0, dataUnlabeled, true_labels=true_labels, nsteps = nsteps, test_data = testdata, labeller_parameter=pars, ...)
#'
#' Run for different labellers:
#'
#' oracle, optimal learning curve.
o <- run(oracle=oracle, est_alpha=F)
#' just machine
m <- run(oracle=idiot, est_alpha=FALSE, combineI=function(m,l) m)
#' just labeller
l <- run(oracle=idiot, est_alpha=TRUE, combineI=function(m,l) l, train_user_with=la <- sample(nrow(dataUnlabeled), 5))
#' both
ml <- run(oracle=idiot, est_alpha=TRUE, combineI=function(m,l) m*l, train_user_with=la)
#'
#' train a user-estimate with all data: Best case scenario of learning the user
#' just for checking if the algorithm really learns anything.
up <- labeller_initial(parameters_default())
h <- idiot(dataUnlabeled)$h
up <- labeller_update(up, dataUnlabeled, h)
#'
#'
#' Save: 
results <- list(m=m, l=l, o=o, ml=ml, up=up)

saveRDS(results, "demo_results.rds")
