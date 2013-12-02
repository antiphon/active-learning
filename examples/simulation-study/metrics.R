#' metrics
metrics <- function(pred, truth) {
  levs_pred <- levels(pred) 
  levs_truth <- levels(truth)
  if(!all(levs_pred == levs_truth)) stop("Level mismatch in column ", v)
  #
  nlevels <- length(levs_pred)
  ## confusion i.e. contingency matrix
  A <- table(pred, truth)
  ## The table format for binary classification is then:
  #  TN, FN
  #  FP, TP
  
  ## Evaluate: 
  ## If only binary classification...
  if(nlevels == 2) {
    result <- evaluate_binary(A)
  }
  ## or multi-label. We average over one-vs-others binary classifications
  else { 
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
  }
  # done.
  result
}

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

