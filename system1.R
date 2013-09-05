#' Active learning toy
#' 

source("experts.R")

system1 <- function(data, burnin, expert, nsteps) {  
  if(missing(nsteps))nsteps <- nrow(data) - length(burnin)
  obs_data <- data[burnin, ]
  
  m <- model_entropy(obs_data, data)
  
  hist <- list()
  hist$entropy <- list()
  hist$classerror <- numeric()
  
  for ( i in 1:nsteps ) {
    w <- which.max(m$entropy)
    l <- expert(data[w, 1:4])
    obs_data <- rbind(obs_data, cbind(data[w, 1:4], Species = l))
    m <- model_entropy(obs_data, data)
    
    hist$entropy[[i]] <- m$entropy
    hist$classerror <- c(hist$classerror, sum(m$class != data$Species) / nrow(data))
    #cat(".")
  }
  
  hist
}
