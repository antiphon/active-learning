
source("system1.R")
source("system2.R")


plot(0, type = "n", xlim = c(0, 150), ylim = c(-0.2, 0.2))
abline(h = 0)

set.seed(1234)
for ( i in 1:100 ) {
  burnin <- c(sample(1:50, 2), sample(51:100, 2), sample(101:150, 2))
    
  s1_res3 <- system1(iris, burnin, noisy_oracle)  
  s2_res3 <- system2(iris, burnin, noisy_oracle)
  
  lines(s1_res3$classerror - s2_res3$classerror)
}

