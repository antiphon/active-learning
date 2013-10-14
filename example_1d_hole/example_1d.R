# 1d example of GP alpha estimation
par(mfrow=c(1,2))
## Generate the alpha field
source("simulate_alpha.R")
prior <- list(s2=4, mu=-3, nugget=1e-6, kappa=2, range=0.4)
alpha <- simulate_alpha1D(seed=12345,xmin=-.5, xmax=1.5, resolution=100, prior=prior)
## Generate some datapoints
K <- 4 # number of classes
N <- K*100 
X <- c(rnorm(N, seq(0,1, length=K), 1/(2*K)))
y <- factor(rep(1:K, N/K))

## sample a test set
N1 <- 50
i1 <- sample(1:N, N1, replace=F)
X1 <- X[i1]
y1 <- y[i1]

## now sample from Dirichlet racle with this alpha
source("hierarchical_oracle.R")
oracle <- rcautious_oracle1D(X, y, alpha)
got <- oracle(X1, y1)
h <- got$h
data1<-data.frame(x=X1, y=y1, a=got$a, h=got$h)[order(X1),]

## then we estimate
source("estimate_alpha.R")
prior2 <- list(mu=0, kappa=1, range=0.1, s2=1, nugget=1)
a_hat <- estimate_alpha(X1, h=h, prior=prior2, verb=T, eps=1e-9)

## then we plot
par(mfrow=c(2,2), cex=0.5)
## The underlying alpha field
with(alpha, plot(x, alpha, "l", ylim=c(0,2), main="Alpha, black=True, red=Est"))
## The alphas at the sample point
points(X1, got$a)
## The estimated alphas at the sample points
points(X1, a_hat$gp, col=2)
## Smoothed mean
lines(lowess(X1, a_hat$gp), col=2)
# the itempoints
rug(X)

# The sufficient statistic used in the estimation is the 
## Sum log h_k
## Plot this and see if it separates different alphas:
x<-got$a
y<-apply(got$h, 1, function(hi) sum(log(hi)))
plot(x, y, main="Sufficient statistic as a function of alpha")
lines(lowess(x,y), col=2)
## pretty vague unfortunately :(

## The maximum of the observation vectors at each x
hm <- apply(got$h, 1, max)
plot(X1, hm, main="Max. value of observation vectors")
plot(got$a, hm, main="Max. value of observation vectors as function of alpha")

## 

