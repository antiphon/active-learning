#'

labeller_initial <- function(parameters) {
  labeller <- list(parameters=parameters, estimated=FALSE, K=0)
  if(parameters$verb) cat("Labeller initialized.\n")
  labeller
}