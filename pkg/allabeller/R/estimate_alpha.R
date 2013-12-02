#'

labeller_estimate_alpha <- function(X, h, parameters) {
  P <- parameters
  cat2 <- if(P$verb) cat else function(...) NULL
  h <- rbind(h)
  X <- rbind(X)
  n <- nrow(h)
  K <- ncol(h)
  # for the intercept
  Z <- cbind(rep(1, n))
  p <- 1#ncol(Z)
  nx <- n+n+p
  # some common items
  D <- as.matrix(dist(X))
  #
  # where are what 
  q_eta <- 1:n
  q_beta <- n+1:p
  q_spat <- 1:n+n+p
  #
  S <- diag(0, nx)
  # 
  # fixed effects, constant priors. (NB: we actually use on the intercept)
  Sbeta0 <- diag(1/P$hyper_prior$tau0, p)
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
    solve(S + diag(1e-4, nx))
  }
  
  # Functions related to the dirichlet distribution
  lysum <- rowSums(log(h)) 
  gg <- function(x)  {
    exp(x)*lysum + K*exp(x)*digamma(K*exp(x)) - K*exp(x)*digamma(exp(x))
  }
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
    converged <- TRUE
    while(loop) {
      mu0 <- mu
      mu <- pmin(pmax(P$gaussian_min, mu), P$gaussian_max)
      
      bee <- c(gg(mu[q_eta]) - mu[q_eta]*ggg(mu[q_eta]) , fill)
      cee <- c(-ggg(mu[q_eta]) , fill)
      QN <- Q1+diag(cee)
      resp <- try(mu <- solve(QN, bee), silent=TRUE)
      if("try-error"%in%resp) mu <- solve(QN+diag(1e-3, nx), bee)
      loop <- (dd<-max(abs(mu-mu0))) > 1e-2
      iter <- iter + 1
      if(is.na(loop)) converged<-loop<-FALSE
    }
    list(m=mu, Q=QN, Qprior=Q1, converged=converged)
  }
  #
  ### Now to actual work.
  hyper <- P$hyper
  # Two scenarios: we estimate the hyper parameters, or we take them given.
  if(P$estimate_hyper) {
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
    prior_a <- c(P$hyper_prior$tau_a, P$hyper_prior$tau_a, P$hyper_prior$range_a)
    prior_b <- c(P$hyper_prior$tau_b, P$hyper_prior$tau_b, P$hyper_prior$range_b)
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
    theta_star <- optim(fn=neg_log_f_theta, par=P$start, 
                        lower=P$lower, upper=P$upper,
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
  # @TODO: We only have the Gaussian approximation for now!
  #
  mode <- optimize_for_theta(theta)
  if(!mode$converged) warning("Theta mode estimation did not converge.")
  #
  #
  cat2("estimation complete.\n")
  # gather results
  beta <- mode$m[q_beta]
  umean <- mode$m[q_spat]
  parameters$hyper <- hyper
  #
  list(spatial=list(mean=umean), intercept=list(mean=beta), 
       parameters=parameters,
       data_X=X,
       data_h=h,
       K=K,
       estimated=TRUE)
}


matern <- function(x, kappa = 1, range = 1) {
  range <- sqrt(8 * kappa) / range
  besselK(x*range, kappa)*(x*range)^kappa/(2^(kappa-1)*gamma(kappa))
}


