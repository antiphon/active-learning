## we now get a label probability vector from the oracle
source("experts2.R")

user_model_entropy <- function(up) {
  apply(up, 1, function(x) -sum(x* log(x)))
}

update_user_count <- function(user, idx, label) {
  for ( i in seq(along = idx) )
    user[idx[i], label[i]] <- user[idx[i], label[i]] + 1
  user
}

update_user_prob <- function(user, alpha) {
  p <- t(t(user) + alpha)
  p / rowSums(p)
}

system2v <- function(data, burnin, expert, nsteps = 50, alpha = c(1, 1, 1)) {  
  n <- nrow(data) - length(burnin)
  obs_data <- data[burnin, ]
  
  m <- model_entropy(obs_data, data)
  
  uc <- matrix(0, nrow = nrow(data), ncol = nlevels(data$Species))
  colnames(uc) <- levels(data$Species)
  
  uc <- update_user_count(uc, burnin, obs_data$Species)
  up <- update_user_prob(uc, alpha)
  
  hist <- list()
  hist$entropy <- list()
  hist$classerror <- numeric()
  hist$userentropy <- list()
  hist$usercount <- list()
  
  for ( i in 1:nsteps ) {
    ue <- user_model_entropy(up)
    w <- which.max(sqrt(m$entropy * ue))
    #w <- which.max(m$entropy) ## = system 1
    l <- expert(data[w, 1:4])
    obs_data <- rbind(obs_data, cbind(data[w, 1:4], Species = l))
    m <- model_entropy(obs_data, data)
    
    uc <- update_user_count(uc, w, l)
    up <- update_user_prob(uc, alpha)
    
    hist$entropy[[i]] <- m$entropy
    hist$classerror <- c(hist$classerror, sum(m$class != data$Species) / nrow(data))
    hist$usercount[[i]] <- uc
    hist$userentropy[[i]] <- ue

    #cat(".")
  }
  
  hist
}

