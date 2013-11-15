#' 

active_learning_step <- function(num_question, question, answer, models, sound_pool) {
  X <- as.matrix(sound_pool[, -c(1, 44, 45)])
  X <- scale(X)
  
  if(!is.null(question$idx)) {
    cat("AL[")
    # the new data
    K <- length(models$classifier$classes)
    hnew <- rep(1/K, K)
    i <- match(question$genres, models$classifier$classes)
    hnew[i] <- as.numeric(answer/sum(answer))
    
    xnew <- X[question$idx, ]
    
    # update the classifier
    cat("c ")
    models$classifier <- classifier_update( models$classifier, xnew, hnew )
    
    # update the labeller model
    cat("l ")
    
    if(num_question%%10==0) models$labeller$parameters$estimate_hyper <- TRUE
    
    models$labeller <- labeller_update( models$labeller, xnew, hnew )
    
    models$labeller$parameters$estimate_hyper <- FALSE
    
  }  
  # compute informativeness
  asked <- (models$classifier$asked <- c(models$classifier$asked, question$idx) )
  unasked <- setdiff(1:nrow(sound_pool), asked)
  
  cat("Ic ")
  Ic <- classifier_informativeness(models$classifier, X[unasked,])
  cat("Il ")
  Il <- labeller_informativeness(models$labeller, X[unasked,])
  Info <- Ic * Il
  new_question_idx <- unasked[which.max(Info)]
  cat("]\n")
  
  list(new_question_idx = new_question_idx, 
       models=models)
}