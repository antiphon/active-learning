#' Two D example of estimating the user uncertainty alpha(x)
#
#' NOT GOOD BEFORE CORRELATION ADDED TO  CLASSIFIER MODEL (-> slanted clusters)

source("data_iris.R")

#' Setup the oracle
source("oracles.R")

oracle1 <- create_oracle_with_unknown_label(data, class="virginica")

#' Now we see what happends
source("active_learning_loop.R")
eprior <- list(range=0.3, mu=-1, kappa=1, s2=10, nugget=2)


## movie function
source("plot_functions.R")

movie <- function(..., pref="temp") {
  result <- NULL
  nsteps<-20
  for(i in 1:nsteps) {
    if(is.null(result))result <- active_learning(..., nsteps=1)
    else result <- active_learning(..., nsteps=1, result=result)
    # store as a png
    png(paste0("movies/", pref, "_",i, ".png"))
    plot1(result, F); title(paste0(pref, ": ", i))
    dev.off()
  }
  result
}
#' with user model entropy
# res_just_user <- movie(data0=data0, dataUnlabeled=dataUnlabeled, oracle=oracle1, 
#                        true_labels=true_labels,
#                        alpha_prior=eprior, combineI=function(a,b) -b, pref="just_user")
# #' without user model
# res_just_machine <- movie(data0=data0, dataUnlabeled=dataUnlabeled, oracle=oracle1, 
#                           true_labels=true_labels,
#                           alpha_prior=eprior, est_alpha=F, combineI=function(a,b) a,
#                           pref="just_machine")

res_both <- movie(data0=data0, dataUnlabeled=dataUnlabeled, oracle=oracle1, 
                  true_labels=true_labels,
                   alpha_prior=eprior,
                  combineI=function(a,b) rowMeans(cbind(order(-a),order(b))), 
                  pref="both")

#' Diagnose:
