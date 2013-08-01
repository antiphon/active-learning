library("e1071")
data("iris")


oracle <- function(x) {
  w <- which(apply(iris[, 1:4], 1, function(y) all(x == y)))[1]
  iris$Species[w]
}

noisy_oracle <- function(x, prob = 0.9) {
  w <- oracle(x)
  if ( runif(1) > prob )
    w <- sample(setdiff(levels(iris$Species), w), 1)
  w
}

asshole <- function(x) {
  sample(levels(iris$Species), 1)
}

human <- function(x) {
  o <- oracle(x)
  if ( o == "versicolor")
    o <- NA
  o
}

model_entropy <- function(obs_data, data) {
  m <- naiveBayes(Species ~ ., data = obs_data)
  p <- predict(m, newdata = data, type = "raw")
  e <- apply(p, 1, function(x) -sum(x* log(x)))
  list(entropy = e, class = levels(data$Species)[apply(p, 1, which.max)]) 
}

burnin <- c(1, 2, 51, 52, 101, 102)
