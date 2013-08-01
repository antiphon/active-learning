
source("system1.R")


res <- system1(iris, burnin, oracle)

plot(res$classerror, type = "l", ylim = c(0, 0.5))

res2 <- system1(iris, burnin, asshole)
lines(res2$classerror, col = "red")

res3 <- system1(iris, burnin, noisy_oracle)
lines(res3$classerror, col = "green")

m0 <- naiveBayes(Species ~ ., data = iris)
abline(h = sum(predict(m0, newdata = iris) != iris$Species) / nrow(iris))
