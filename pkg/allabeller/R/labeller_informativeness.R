#'

labeller_informativeness <- function(labeller, X) {
  if(!labeller$estimated) return(rep(1, nrow(X)))
  # estimate alfas
  alfa <- labeller_predict_alpha(labeller, X)  
  # plugin variance
  K <- labeller$K
  (K-1)/(K^2*(K*alfa+1))
}
