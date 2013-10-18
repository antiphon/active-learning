#' test estimation of alpha in d-dims

source("data.R")
source("alpha.R")

set.seed(1234)

N1 <- 30
data_test <- data[sample(1:nrow(data),N1),]

# create the alpha
alpha1 <- sim_hole_alpha(centre=c(-1,-.9), 1.5, v_in=0.1,  v_out=5)

# plot_alpha2d(alpha1, asp=1, col=gray.colors(10))
# points(data[,-1], col=data[,1])
## seems ok

## Now, sample
source("oracles.R")
oracle<-create_oracle_with_alpha(alpha1, data)

testX<-data_test[,-1]
obs <- oracle(testX)

## check out the maximum of vectors wrt true alpha: h_max(alpha)
#plot(obs$a, apply(obs$h, 1, max))
# some overlap put we have hope.


## Now, try to estimate the alpha's using a GP prior and the Dirichlet-l'hood
source("estimate_alpha.R")
eprior <- list(range=0.3, mu=0, kappa=1, s2=2, nugget=0.5)
est <- estimate_alpha(obs$h, testX, prior=eprior, verb=T)

## Diagnose:
# spatial image of alpha
plot_alpha2d(alpha1, asp=1, col=gray.colors(10))
points(testX, cex=est$gp, col=2)

## seems to work :)

# Try the prediction
source("predict_alpha.R")
apred <- predictor_alpha(est, testX)

xy <- expand.grid(seq(-2,4, length=20), seq(-2,2, length=15))
ap <- apred(xy)

points(xy, col=3, cex=ap)

