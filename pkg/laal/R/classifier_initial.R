#'

classifier_initial <- function(X, y, idx, prior_sd=2) {
  #' this will compute the classifier
  #' with a sample from the pool.
  # 
#   data0 <- sound_pool[,-c(1,45)]
#   y0 <- data0[,43]
#   X0 <- data0[,-43]
#   
#   # take a balanced sample
#   set.seed(1234)
#   idx <- c(sapply(split(1:nrow(X0), y0), sample, 3))
#   y<-y0[idx]
#   X<-X0[idx,]
  # the initial data
  #' naive bayes classifier: parameters are the class averages 
  theta_m <- sapply( split(X, y), function(x) apply(x, 2, mean))
  theta_S <- lapply( split(X, y), function(x) cov(x) + diag(prior_sd/nrow(x), ncol(x)))
  
  nc <- table(y)/nrow(X)
  
  cat("Classifier initialized.\n")
  
  list(theta_m=theta_m, theta_S=theta_S,
       theta_m0=theta_m, theta_S0=theta_S,
       n0 = c(table(y)),
       data0 = data.frame(y=y, X),
       theta_c=nc, n=c(table(y)), 
       classes=levels(y),
       asked=idx,
       data_h=NULL,
       data_X=NULL
  )
}