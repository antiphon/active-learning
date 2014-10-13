#' test labeller

source("1-data_generic_1.R")

source("labeller.R")

lab1 <- create_labeller_neg_hole(X, y, center = c(-1,-1), range=1)

tests <- sample(nrow(X), 10)
resp <- lab1(X[tests,])

plot(X, cex=.1, asp=1)

points(X[tests,], pch=1+18*(resp$alfas<1))
symbols(-1,-1, circles=1, inches=F, add=T)


