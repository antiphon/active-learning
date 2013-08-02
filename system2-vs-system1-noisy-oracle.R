
source("system1.R")
source("system2.R")

n<-30
plot(0, type = "n", xlim = c(0, n), ylim = c(-0.5, 0.5))
abline(h = 0)

noisy_oracle60 <- function(x)noisy_oracle(x, prob=0.6)
set.seed(1234)
res<-list()

for ( i in 1:100 ) {
  burnin <- c(sample(1:50, 2), sample(51:100, 2), sample(101:150, 2))
  
  s1_res3 <- system1(iris, burnin, noisy_oracle60, n=n)  
  s2_res3 <- system2(iris, burnin, nsteps=n, noisy_oracle60)
  
  lines(dif<-s1_res3$classerror - s2_res3$classerror)
  res[[i]]<-dif
}


m <- do.call(rbind, res)
lines(colMeans(m), col=2, lwd=2)