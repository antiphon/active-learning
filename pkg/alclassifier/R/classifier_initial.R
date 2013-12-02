#'

classifier_initial <- function(X, y, idx=NULL, prior_sd=2, verb=FALSE) {
  #' this will compute the classifier
  #' with a sample from the pool.
  #
  if(!is.factor(y)) stop("y should be a factor vector. Make sure levels are correct.")
  # the initial data
  #' naive bayes classifier: parameters are the class averages 
  theta_m <- sapply( split(X, y), function(x) apply(x, 2, mean))
  theta_S <- lapply( split(X, y), function(x) cov(x) + diag(prior_sd/nrow(x), ncol(x)))
  
  nc <- table(y)/nrow(X)
  
  # make a h version
  data0_h <- matrix(0, ncol=nlevels(y), nrow=length(y))
  data0_h[cbind(1:nrow(data0_h), match(y, levels(y))) ] <- 1
  
  if(verb) cat("Classifier initialized.\n")
  
  list(theta_m=theta_m, theta_S=theta_S,
       theta_m0=theta_m, theta_S0=theta_S,
       n0 = c(table(y)),
       prior_sd=prior_sd,
       data0 = data.frame(y=y, X),
       data0_h = data0_h,
       theta_c=nc, n=c(table(y)), 
       classes=levels(y),
       asked=idx,
       data_h=NULL,
       data_X=NULL,
       Itype="entropy"
  )
}