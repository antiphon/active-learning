#' Active learning function
#' v2: use packages

library(allabeller)
library(alclassifier)

active_learning <- function(data0, # the initial learning sample for the classifier
                            dataUnlabeled, # unlabeled set for querying
                            oracle, # the labeller function f(x)
                            true_labels, # true labels of unlabeld set, for error
                            nsteps=50, # how many iterations
                            result=NULL, # previous result object
                            combineI=function(m,l) m*l, # how to combine the I's
                            est_alpha=TRUE, 
                            test_data,
                            debug=TRUE,
                            I="entropy",
                            begin_with=NA,
                            labeller_parameters=parameters_default(),
                            ...) {
  cat2 <- if(debug) cat else function(...)NULL
  if(is.null(result)) {
    result <- list( 
      classifier = classifier_initial(data0[,-1], data0[,1]),
      labeller = labeller_initial(labeller_parameters),
      asked = NULL,
      Im_hist = NULL, # classifier
      Io_hist = NULL, # labeller
      I_hist = NULL,  # used criteria
      training_error_hist = NULL,
      test_error_hist = NULL,
      h_hist = NULL,
      est_alpha = est_alpha
    )
    if(!missing(test_data)) {
      pred_test <- classifier_classify(result$classifier, test_data[,-1])
      result$test_error_hist <- c(result$test_error_hist, 
                                  mean( 1*(test_data[,1]==pred_test) ) )
    }
  }
  result$classifier$Itype <- I
  K <- length(result$classifier$classes)
  result$labeller$K <- K
  # @TODO : encounter resampling
  notasked <- setdiff(1:nrow(dataUnlabeled), result$asked)
  # loop
  for(i in 1:nsteps) {
    cat2("If ")
    result$Im_hist <- rbind(result$Im_hist, 
                            Im <- classifier_informativeness(result$classifier, dataUnlabeled))
    cat2("Iof ")
    result$Io_hist <- rbind(result$Io_hist, 
                            Io <- labeller_informativeness(result$labeller, dataUnlabeled))
    # combine
    result$I_hist <- rbind(result$I_hist , 
                            Is <- combineI(Im, Io))
    # choose the point to ask
    
    result$asked <- c(result$asked, 
                      ask <- if(is.na(begin_with[i])) notasked[which.max(Is[notasked])] else begin_with[i])
    notasked <- setdiff(notasked, ask)
    cat2("Asking:", ask, " ", sep="")
    xnew <- dataUnlabeled[ask, ]
    cat2("query ")
    result$h_hist <- rbind(result$h_hist, 
                           hnew <- oracle(xnew)$h )
    cat2("updateClassifier ")
    result$classifier <- classifier_update(result$classifier, xnew,  hnew)
    if(result$est_alpha){
      cat2("updateLabeller ")
      result$labeller <- labeller_update(result$labeller, xnew,  hnew)
    }
    ## training error
    pred_train <- classifier_classify(result$classifier, dataUnlabeled[result$asked,])
    score <- mean( 1*(true_labels[result$asked]==pred_train)  )
    result$training_error_hist <- c(result$training_error_hist, 
                                    score)
    ## test error
    if(!missing(test_data)) {
      pred_test <- classifier_classify(result$classifier, test_data[,-1])
      score <- mean( 1*(test_data[,1]==pred_test) )
      #score <- fscore(test_data[,1], pred_test)
      result$test_error_hist <- c(result$test_error_hist, 
                            score )
    }
    
    RES <<- result
    cat2("", i, "/", nsteps, "        \n")
  }
  cat2("\n")
  result
}


fscore <- function(true, pred){
  if(!all(levels(true)==levels(pred)))stop("levels dont match.")
  
  A <- table(pred, true)
  evaluate_binary <- function(A) {
    # see http://en.wikipedia.org/wiki/Sensitivity_and_specificity#Worked_example
    pos_pred_value <- if(sum(A[2,])) A[2,2]/sum(A[2,]) else 0 # same as precision
    sensitivity <- A[2,2]/sum(A[,2]) # same as recall
    accuracy <- (A[1,1]+A[2,2])/sum(A)
    false_pos_rate <- A[2,1]/(A[2,1]+A[1,1])
    Fscore <- 2 * (pos_pred_value*sensitivity)/(pos_pred_value + sensitivity)
    if(is.na(Fscore))  Fscore <- 0 # we decided this.
    # phi score aka Matthews correlation coefficient
    denom <- sqrt( prod(c(colSums(A),rowSums(A))) )
    phi_score <- if( denom ) (A[2,2]*A[1,1] - A[1,2]*A[2,1])/denom else 0
    # keep all
    result <- data.frame(Fscore=Fscore, phi_score=phi_score, accuracy=accuracy,
                         false_pos_rate=false_pos_rate,
                         sensitivity=sensitivity, pos_pred_value=pos_pred_value)
  }
  nlevels <- nlevels(true)
  levs_truth <- levels(true)
  result <- NULL
  Bave <- diag(0, 2)
  for(l in 1:nlevels){
    # level l confusion matrix
    tp <- A[l,l]
    tn <- sum(A[-l,-l])
    fp <- sum(A[l,-l])
    fn <- sum(A[-l,l])
    B <- matrix(c(tn, fp, fn, tp), ncol=2)
    Bave <- Bave + B ## for average matrix
    result <- rbind(result, evaluate_binary(B) ) # for average values
  }
  ## average over the classes. Using average confusion matrix...
  ave_result <- evaluate_binary(Bave / nlevels)
  ## ...and value averages
  result <- rbind(colMeans(result), ave_result, result)
  ##
  rownames(result) <- c("score_mean", "confusion_mean", levs_truth)
  ## return only the score_mean
  result[1,1]
}

