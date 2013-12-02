#'
classifier_classify <- function(classifier, X) {
  lps <- classifier_log_probabilities(classifier, X)
  y <- classifier$classes[ apply(lps, 1, which.max) ]
  factor(y, levels=classifier$classes)
}
