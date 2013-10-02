#' Use the iris data
data(iris)
#' initial data
l0<- c(1, 2, 51, 52, 101, 102)
L0 <- iris[l0,]
#' rest
U <- iris[-l0,1:4]
true_labels <- iris$Species[-l0]
CLASSES <- levels(iris$Species)

DEBUG <- FALSE
cat2 <- if(DEBUG) cat else function(x) NULL

#' Classifier for clean data
fitM0 <- function(data) {
  #' naive bayes classifier: parameters are the class averages 
  theta_m <- sapply( split(data[,1:4], data[,5]), function(x) apply(x, 2, mean))
  theta_sd <- sapply( split(data[,1:4], data[,5]), function(x) apply(x, 2, sd))
  #' add prior sd, rought estimate
  theta_sd <- theta_sd + matrix(rep(100, 12), ncol=3)/nrow(data)^2
  nc <- table(data[,5])/nrow(data)
  list(theta_m=theta_m, theta_sd=theta_sd, theta_c=nc, n=table(data[,5]))
}


## update the fit with cautious oracle data
update_fitM <- function(fit, data1x, data1h) {
  #' TODO: reallly crude approximation, haven't derived formulas yet!
  X<-rbind(data1x)
  H<-rbind(data1h)
  m <- fit$theta_m
  s <- fit$theta_sd
  nc <- fit$theta_c
  n <- fit$n
  for(i in 1:nrow(X)) {
    x <- X[i,]
    h <- H[i,]
    for(sp in 1:3) {
      for(ro in 1:4) {
        mnew <- as.numeric((m[ro, sp]*n[sp] + h[sp]*x[ro])/(n[sp]+h[sp]) )
        ssold <- n[sp] * (s[ro, sp]^2 + m[ro,sp]^2)
        snew <- sqrt( (ssold+h[sp]*x[ro]^2 )/(n[sp]+h[sp]) - mnew^2 )
        m[ro, sp] <- mnew
        s[ro, sp] <- snew
      }
    }
    #nc <- nc + h
    #nc <- nc/sum(nc)
    n <- n+h
  }
  list(theta_m=m, theta_sd=s, theta_c=nc, n=n)
}

## return the probabilities p(c|x, data), log
probM <- function(x, fit) {
  ps <- NULL
  xz <- rbind(x)
  cat2("fit")
  for(j in 1:nrow(xz)) {
    p<-NULL
    for(i in 1:3){
      p[i] <- log(fit$theta_c[i]) + sum( sapply(1:4, function(k)
        dnorm(xz[j,k], fit$theta_m[k,i], fit$theta_sd[k,i], log=T) )  )
    }
    ps <- rbind(ps, p)
  }
  colnames(ps) <- colnames(fit$theta_m)
  cat2("\n")
  ps
}

entropy <- function(X, fit0, fit1) {
  ps <- probM(X, fit0, fit1)
  apply(ps, 1, function(z) -sum(exp(z)*z) )
}


#' the cautious oracle
cautious_oracle <- function(x, oracle=TRUE) {
  #' we know only the third class
  x<-rbind(x)
  labels <- iris$Species[apply(x,  1, function(z) 
    which(apply(iris[,-5], 1, function(v) all(v==z))   )[1])]
  h <- NULL
  cat2("oracle")
  for(label in labels) {
    if(!oracle) h <- rbind(h, if(label=="virginica") c(0,0,1) else c(0.5,0.5,0))
    else h <- rbind(h, 1*(label==CLASSES))
  }
  cat2("\n")
  h
}

classify <- function(X, fit) {
  p<-probM(X, fit)
  CLASSES[apply(p, 1, which.max)]
}


#' Active learning
active_learning <- function(L0, U, true_labels, nsteps=50) {
  fit <- fitM0(L0)  
  asked <- NULL
  I_hist <- NULL
  error_hist <- NULL
  for(i in 1:nsteps) {
    I_hist <- rbind(I_hist, Is <- entropy(U, fit))
    asked <- c(asked, ask <- which.max(Is))
    xnew <- U[ask, ]
    hnew <- cautious_oracle(xnew)
    fit <- update_fitM(fit, xnew,  hnew)
    error_hist <- c(error_hist, mean( 1*(true_labels==classify(U, fit))  ))
    print(i)
  }
  list(fit=fit, I_hist=I_hist, asked=asked, error_hist=error_hist)
}

## now we test:
res <- active_learning(L0, U, true_labels, nstep=nsteps <- 10)
print(table(res$asked))
## plot the data using 
xy <- cmdscale(dist(U))
size <- .1+table(factor(res$asked, levels=1:nrow(U)))/sqrt(nsteps)
plot(xy, col = iris$Species, cex=size)

