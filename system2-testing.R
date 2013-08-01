
source("system2.R")


### Oracle:

s2_res <- system2(iris, burnin, oracle)

plot(s2_res$classerror, col = "blue", type = "l")

uc <- s2_res$usercount[[length(s2_res$usercount)]]
sum(uc)
plot(rowSums(uc), col = iris$Species)
plot(cmdscale(dist(iris[, 1:4])), col = iris$Species, cex = rowSums(uc) / 11)

uc <- s2_res$usercount[[15]]
plot(cmdscale(dist(iris[, 1:4])), col = iris$Species, cex = rowSums(uc))



### Asshole:

s2_res2 <- system2(iris, burnin, asshole)
lines(s2_res2$classerror, col = "red")


### Noisy oracle:

s2_res3 <- system2(iris, burnin, noisy_oracle)
lines(s2_res3$classerror, col = "green")

str(s2_res3, 1)
ue <- do.call(rbind, s2_res3$userentropy)

uc <- s2_res3$usercount[[length(s2_res3$usercount)]]

par(mfrow = c(1, 2))
plot(cmdscale(dist(iris[, 1:4])), col = iris$Species, cex = rowSums(uc) / 11)
plot(cmdscale(dist(iris[, 1:4])), col = iris$Species, cex = ue[200, ])
     

