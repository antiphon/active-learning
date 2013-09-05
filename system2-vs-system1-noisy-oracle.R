
source("system1.R")
source("system2.R")

n<-10
plot(0, type = "n", xlim = c(0, n), ylim = c(-0.5, 0.5))
abline(h = 0)

noisy_oracle60 <- function(x)noisy_oracle(x, prob=0.6)
res<-list()
seed <- 1234
set.seed(seed)


for ( i in 1:10 ) {
  burnin <- c(sample(1:50, 2), sample(51:100, 2), sample(101:150, 2))
  
  s1_res3 <- system1(iris, burnin, noisy_oracle60, nsteps=n)  
  
  s2_res3 <- system2(iris, burnin, noisy_oracle60, nsteps=n, alpha=c(1,1,1)*0.1)
  
  lines(dif<-s1_res3$classerror - s2_res3$classerror)
  res[[i]]<-dif
}


m <- do.call(rbind, res)
lines(colMeans(m), col=2, lwd=2)