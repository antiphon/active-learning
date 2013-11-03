## these experts return a vector of label probabilities

library("e1071")
data("iris")

## dummy vector
label_vector <- rep(0, 3)
names(label_vector) <- levels(iris$Species)

oracle <- function(x) {
  w <- which(apply(iris[, 1:4], 1, function(y) all(x == y)))[1]
  label <- iris$Species[w]
  label_vector[label] <- 1
  label_vector
}

noisy_oracle <- function(x, prob = 0.9) {
  vec <- oracle(x)
  vec[vec==0] <- (1-prob)/2
  vec[vec==1] <- prob
  vec
}

## tosses a coin
asshole <- function(x) {
  label_vector[sample(levels(iris$Species), 1)] <- 1
  label_vector
}

## knows only versicolor
human <- function(x) {
  o <- oracle(x)
  if ( names(o)[o==1] != "versicolor")
    {o <- 0*o+1/2; o['versicolor']<-0} ## knows only that not versicolor
  o
}

## note that obs_Data is probability vectors! 
model_entropy <- function(obs_data, data) {
  m <- naiveBayes(Species ~ ., data = obs_data)
  p <- predict(m, newdata = data, type = "raw")
  e <- apply(p, 1, function(x) -sum(x* log(x)))
  list(entropy = e, class = levels(data$Species)[apply(p, 1, which.max)]) 
}

burnin <- c(1, 2, 51, 52, 101, 102)


