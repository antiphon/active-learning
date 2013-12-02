#'@import mvtnorm

classifier_informativeness <- function(classifier, X) {
  ## Compute the class probabilities for each item/row in X
  ps <- classifier_log_probabilities(classifier, X)
  # the I for classifier is the entropy
  If <- switch(classifier$Itype, 
               "entropy" = function(z) -sum(exp(z)*z),
               "max" = function(z) 1 - max(z),
               "margin" = function(z) 1 - abs(diff(sort(z, decreasing=TRUE)[1:2]))
  )
  apply(ps, 1, If)
}
