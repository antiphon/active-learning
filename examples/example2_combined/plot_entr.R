#' explore the two I-functions
K <- 3

g <- function(a) K*log(gamma(a)) - log(gamma(K*a)) + K*(a-1)*(digamma(K*a) - digamma(a))

curve(g, from=0.1, to=4)
      
abline(v=1, col="gray70", lty=2)
# not good as not monotonous


# variance of a single dim
f <- function(a) (K-1)/(K^2*(K*a+1))

curve(f, from=0.1, to=5)
