
### The human should express her knowledge about the labels for the 
### corresponding observation as probabilities for the three different
### classes. This way, it is easy to express "I don't know" as 
### c(1, 1, 1) / 3.


### Protoyping: ######################################################

xy <- cmdscale(dist(iris[, -5]))
col <- palette()[as.integer(iris$Species)]
plot(xy, col = col)
##identify(xy)


### Cirlce of no knowledge:

w <- 69
th <- 1

d <- dist(iris[, -5])
d <- as.matrix(d)

dp <- d[w, ]

table(iris$Species[dp < th])

col <- palette()[as.integer(iris$Species)]
col[dp < th] <- "gray"
plot(xy, col = col)

##-> for these points, the human returns equal probability for all
##-> classes.



### Circle of some knowledge:

th2 <- th + 0.5

col <- palette()[as.integer(iris$Species)]
col[dp < th] <- "gray"
col[dp > th & dp < th2] <- "black"
plot(xy, col = col)

##-> for these points, the human returns equal probability for the two
##-> highest classes.


### Function (brutal hack):

human <- function(x, th = 1, th2 = th + 0.5) { 
  w <- which(apply(iris[, 1:4], 1, function(y) all(x == y)))[1]
  d <- as.matrix(dist(iris[, -5]))[69, ]
  
  ret <- c(0, 0, 0)
  
  if ( d[w] <= th) {
    ret <- c(1, 1, 1) / 3
  }
  if ( d[w] > th & d[w] <= th2 ) {
    ret <- c(0, 0.5, 0.5)
  }
  if ( d[w] > th2 ) {
    ret[as.integer(iris$Species[w])] <- 1
  }
  
  ret
}

k <- t(apply(iris[, -5], 1, human))
apply(k, 1, max)

col <- palette()[as.integer(iris$Species)]
plot(xy, col = col, cex = apply(k, 1, max))
