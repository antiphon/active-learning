#' Active learning function
source("fitting.R")
source("predict_and_informativeness.R")


active_learning <- function(data0, # the initial learning sample for the classifier
                            dataUnlabeled, # unlabeled set for querying
                            oracle, # the labeller function f(x)
                            true_labels, # true labels of unlabeld set, for error
                            nsteps=50, # how many iterations
                            I="entropy", 
                            result=NULL, # previous result object
                            alpha_prior, # priors for GP
                            combineI=function(a,b) a-b, # how to combine the I's
                            est_alpha=TRUE, collect_n_before_est_alpha=0,
                            test_data,
                            use_inla=FALSE,
                            debug=FALSE,
                            ...) {
  cat2 <- if(debug) cat else function(...)NULL
  if(is.null(result)) {
    result <- list( 
      fit = fitM0(data0),
      asked = NULL,
      Im_hist = NULL, # classifier
      Io_hist = NULL, # oracle
      I_hist = NULL,  # criteria
      training_error_hist = NULL,
      test_error_hist = NULL,
      h_hist = NULL,
      use_inla=use_inla
    )
    result$fit$use_inla <- use_inla
    result$fit$est_alpha <- est_alpha
    result$fit$collect_n_before_est_alpha <- collect_n_before_est_alpha
    result$asked <- NULL
  }
  result$fit$prior <- alpha_prior
  
  K <- length(result$fit$classes)
  
  # oracle entropy formula
  # TODO better here
  Iof <- function(newx, fit) {
    # use plug-in estimate
    a <- c(fit$alpha(newx))
    (K-1)/(K^2*(K*a+1))
  }
  # TODO counter resampling
  notasked <- setdiff(1:nrow(dataUnlabeled), result$asked)
  # loop
  for(i in 1:nsteps) {
    cat2("If ")
    result$Im_hist <- rbind(result$Im_hist, Im <- If(dataUnlabeled, result$fit, type=I))
    cat2("Iof ")
    result$Io_hist <- rbind(result$Io_hist, Io <- Iof(dataUnlabeled, result$fit))
    result$I_hist <- rbind(result$I_hist , Is <- combineI(Im, Io))
    result$asked <- c(result$asked, ask <- notasked[which.max(Is[notasked])])
    notasked <- setdiff(notasked, ask)
    cat2("Asking:", ask, " ", sep="")
    xnew <- dataUnlabeled[ask, ]
    cat2("oracle ")
    result$h_hist <- rbind(result$h_hist, hnew <- oracle(xnew)$h )
    cat2("updateFit ")
    result$fit <- update_fitM(result$fit, xnew,  hnew, ...)
    ## training error
    result$training_error_hist <- c(result$training_error_hist, 
                           mean( 1*(true_labels==classify(dataUnlabeled, result$fit))  ))
    ## test error
    if(!missing(test_data)) 
      result$test_error_hist <- c(result$test_error_hist, 
                                      mean( 1*(test_data[,1]==classify(test_data[,-1], result$fit))  ))
    
    RES <<- result
    cat2("", i, "/", nsteps, "        \n")
  }
  cat2("\n")
  result
}



