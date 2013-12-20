#'
labeller_update <- function(labeller, X, h) {
  X<-rbind(X)
  h<-rbind(h)
  h <- h +  0.005
  h <- h/sum(h)
  X <- rbind(labeller$data_X, X)
  h <- rbind(labeller$data_h, h)
  
  #' @TODO: We should really have an update instead of full estimation.
  
  if(labeller$parameters$minimum_data <= nrow(h) ){
    new_labeller <- labeller_estimate_alpha(X, h, labeller$parameters)
  }
  else{ 
    labeller$data_X <- X
    labeller$data_h <- h
    labeller$K <- ncol(h)
    new_labeller <- labeller
  }
  new_labeller
}
