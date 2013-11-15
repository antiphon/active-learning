# Estimate the latent gaussian variables for the Dirichlet observation data.
# Assume symmetric Dirichlet.

source("matern.R")

DirGPSymINLA <- function(X, 
                         h, 
                         Z=NULL,
                         hyper=list(range=1, kappa=1, 
                                    s2=.5, nugget=1),
                         maxdiff=3,
                         hyper_prior = list(tau0=1e-4, 
                                          tau_a=1e-5,
                                          tau_b=1e-5,
                                          range_a=.02,
                                          range_b=.02),
                         estimate_hyper=FALSE,
                         Xnew, 
                         start=c(1,1,1),
                         lower=c(1e-4, 1e-4, .1),
                         upper=c(1e4, 1e4, 100),
                         verb=FALSE) {
  #
  cat2 <- if(verb) cat else function(...) NULL
  #
  n <- nrow(h)
  K <- ncol(h)
  # for the intercept
  Z <- cbind(rep(1, n), Z)
  p <- ncol(Z)
  nx <- n+n+p
  # some common items
  D <- as.matrix(dist(X))
  #
  # 
  # where are what 
  q_eta <- 1:n
  q_beta <- n+1:p
  q_spat <- 1:n+n+p
  #
  S <- diag(0, nx)
  # 
  # fixed effects, constant priors. (NB: we actually use on the intercept)
  Sbeta0 <- diag(1/hyper_prior$tau0, p)
  S[q_beta, q_beta] <- Sbeta0
  #
  # linear predictor: this depends on theta so computed in Qtheta(). 
  # just the constant here.
  Seta0 <- Z%*%Sbeta0%*%t(Z)
  # cross between eta and beta
  S[q_eta, q_beta]  <- Z%*%Sbeta0 
  S[q_beta, q_eta] <- t(S[q_eta, q_beta])
  
  # This function gives the Q for given hyper priors theta
  Qtheta <- function(theta){
    Su0 <- matern(D, range=theta$range)
    diag(Su0) <- 1
    # spatial components spat
    S[q_spat, q_spat] <- theta$s2*Su0
    # linear predictors eta
    S[q_eta, q_eta] <- Seta0 + S[q_spat, q_spat] + diag(theta$nugget, n)
    # cross between eta and spat
    S[q_eta, q_spat] <- S[q_spat, q_eta] <- S[q_spat, q_spat]
    # the most expensive operation is this:
    solve(S + diag(1e-4, n+p+n))
  }
  
  # Functions related to the dirichlet distribution
  lysum <- rowSums(log(h)) 
  gg <- function(x)  
    exp(x)*lysum + K*exp(x)*digamma(K*exp(x)) - K*exp(x)*digamma(exp(x))
  ggg <- function(x) {
    x <- exp(x)
    x*lysum + K*x*digamma(K*x) + K^2*x^2*trigamma(K*x)-
      K*x*digamma(x) - K*x^2*trigamma(x)
  }
  
  # We use Gaussian approximation, not possible the best but definitely
  # the simplest reasonable.
  fill <- rep(0, n+p)
  optimize_for_theta <- function(theta){
    loop <- TRUE
    mu <- matrix(0, nrow=n+p+n)
    iter <- 0
    Q1 <- Qtheta(theta)
    while(loop) {
      mu0 <- mu
      bee <- c(gg(mu[q_eta]) - mu[q_eta]*ggg(mu[q_eta]) , fill)
      cee <- c(-ggg(mu[q_eta]) , fill)
      QN <- Q1+diag(cee)
      mu <- solve(QN, bee)
      loop <- (dd<-max(abs(mu-mu0))) > 1e-2
      iter <- iter + 1
    }
    list(m=mu, Q=QN, Qprior=Q1)
  }
  #
  ### Now to actual work.
  # Two scenarios: we estimate the hyper parameters, or we take them given.
  if(estimate_hyper) {
    # We will estimate the hyperpriors
    #  range
    #  s2
    #  nugget
    # These will be called the theta in what follows
    # the hyperprior kappa must be fixed.
    #
    # We need a bunch of functions first:
    #
    log_f_data <- function(x) sum( (exp(x)-1)*lysum + lgamma(K*exp(x)) - K*lgamma(exp(x)) )
    # priors
    prior_a <- c(hyper_prior$tau_a, hyper_prior$tau_a, hyper_prior$range_a)
    prior_b <- c(hyper_prior$tau_b, hyper_prior$tau_b, hyper_prior$range_b)
    log_f_priors <- function(theta) { 
      sum(  (prior_a-1)*log(theta) - prior_b*theta + prior_a*log(prior_b) - lgamma(prior_a)  )
    }
    ## x|theta, evaluated at x, Q is the prior Q(theta).
    log_f_x_given_theta <- function(mode, theta){
      0.5*(determinant(mode$Qprior)$mod -nx*log(2*pi) ) - 0.5*t(mode$m)%*%mode$Qprior%*%mode$m
    }
    # GA density at mode
    log_f_GA_x <- function(mode){
      -(p+n+n)*0.5*log(2*pi)+0.5*determinant(mode$Q)$modulus
    }
    # log_f(theta|y)  using GA for normalization with (x|y)
    log_f_theta <- function(theta){ 
      # damn... must make a list
      theta2 <- list(nugget=1/theta[1], s2=1/theta[2], range=theta[3])
      #
      mode<-optimize_for_theta(theta2)
      log_f_priors(theta) + log_f_x_given_theta(mode, theta2) + 
        log_f_data(mode$m[1:n]) - log_f_GA_x(mode)    
    }
    # negative log f_theta|y
    neg_log_f_theta <- function(theta) -log_f_theta(theta)
    #
    # The optimization step:
    cat2("optimizing")
    theta_star <- optim(fn=neg_log_f_theta, par=start, 
                        lower=lower, upper=upper,
                        method="L-BFGS-B", hessian=T)
    cat2(", mode found.[", paste(c("s2=","nugget=","range="), 
                                 theta_star$par^c(-1,-1,1)),"]\n")
    #
    # done for now. If curvature (variances) are needed, add them here.
    #
    hyper$nugget <- 1/theta_star$par[1]
    hyper$s2 <- 1/theta_star$par[2] 
    hyper$range <- theta_star$par[3] 
  }
  # the rest goes the same in both cases.
  #
  theta <- hyper
  mode <- optimize_for_theta(theta)
  
  # gather results
  S <- solve(mode$Q)
  eta <- list(m=mode$m[q_eta], S=S[q_eta, q_eta])
  alpha <- exp(eta$m+diag(eta$S))
  beta <- mode$m[q_beta]
  u <- list(m=mode$m[q_spat], S=S[q_spat, q_spat])
  hyper$mu <- hyper$m <- beta
  #
  cat2("estimation complete.\n")
  #
  list(gp=alpha, est=alpha, 
       linear=eta,
       Si=mode$Q[q_eta, q_eta], 
       u=u, 
       intercept=beta, hyper=hyper, prior=hyper)
}


if(exists("TI")){
  library(MCMCpack)
  set.seed(1235)
  # the users knowledge field
  m<-100
  loc <- seq(-5, 5, length=m)
  u <- cos(loc)
  mu <- 0.5
  nugget_s2 <- 0.02
  nugget <- rnorm(m, 0, sqrt(nugget_s2))
  a <- exp( mu + u + nugget)
  
  # then we choose some for asking
  n <- 50
  query <- sort( sample(1:m, n) )
  a_query <- a[query]
  x_query <- loc[query]
  
  # then the user replies
  K <- 4
  h <- t(sapply(a_query, function(a) rdirichlet(1, rep(a,K))))
  lsum <- rowSums(log(h))
  # then we estimate the alpha:
  if(!exists("hyper"))hyper <- DirGPSymINLA(x_query, h, estimate_hyper=TRUE, 
                                            verb=TRUE)$hyper
  
  fit <- DirGPSymINLA(x_query, h, hyper=hyper)
  #
  # summarize
  su <- data.frame(true=c(intercept=mu, nugget_s2=nugget_s2), 
                   est=c(fit$intercept, fit$hyper$nugget))
  print( su )
  
  # then we predict
  source("predict_alpha.R")
  fp <- predictor_lalpha(fit, cbind(x_query))
  la_pred <- fp(cbind(loc))
  a_pred <- exp(la_pred$m + la_pred$s2*0.5)
  ### diag plot
  par(mfrow=c(3,1))
  # the data
  plot(loc, a, "b", cex=.2, ylim=yl<-c(0,10))
  points(x_query, a_query, col="blue")
  # This is what we estimate alpha to be:
  plot(x_query, fit$est, col="red", ylim=yl)
  points(loc, a_pred, col=3, pch=19, cex=0.5)
  #polygon(c(loc, rev(loc)), c(fit$est), col=rgb(.8,.8,.8,.7), fg="white")
  # not horrible.
  # The spatially smooth component:
  plot(loc, u, type="b")
  points(x_query, fit$u$m, col=2, pch=19)  
}
