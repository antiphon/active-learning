#' Active learning toy
#' 

source("experts.R")

system1 <- function(data, burnin, expert, n) {  
  if(missing(n))n <- nrow(data) - length(burnin)
  obs_data <- data[burnin, ]
  
  m <- model_entropy(obs_data, data)
  
  hist <- list()
  hist$entropy <- list()
  hist$classerror <- numeric()
  
  for ( i in 1:n ) {
    w <- which.max(m$entropy)
    l <- expert(data[w, 1:4])
    obs_data <- rbind(obs_data, cbind(data[w, 1:4], Species = l))
    m <- model_entropy(obs_data, data)
    
    hist[[1]][[i]] <- m$entropy
    hist[[2]] <- c(hist[[2]], sum(m$class != data$Species) / nrow(data))
    #cat(".")
  }
  
  hist
}
