#'

labeller_update <- function(labeller, X, h) {
  
  X<-rbind(X)
  h<-rbind(h)
  h <- h + runif(ncol(h), 0, 0.001)
  h <- h/sum(h)
  X <- rbind(labeller$data_X, X)
  h <- rbind(labeller$data_h, h)
  #' @TODO: We should really have an update instead of always re-estimating.
  
  if(labeller$parameters$minimum_data <= nrow(h) ){
    new_labeller <- labeller_estimate_alpha(X, h, labeller$parameters)
    cat("+")
  }
  else{ 
    labeller$data_X <- X
    labeller$data_h <- h
    cat("-")
    labeller$K <- ncol(h)
    new_labeller <- labeller
  }
  new_labeller
}
