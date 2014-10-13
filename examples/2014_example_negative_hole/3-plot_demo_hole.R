#' plots
res <- readRDS("demo_results.rds")
source("1-data_generic_1.R")
library(alclassifier)
library(allabeller)



#' plot decision boundaries:
dbim <- function(cl, ...){
  grid <- expand.grid(xseq<-seq(-2, 5, length=50), yseq<-seq(-3,3, length=50))
  v<-classifier_classify(cl, grid)
  M <- image(xseq, yseq, z=matrix(as.numeric(v), ncol=50), col=1:4, ...)
  points(X, cex=.5)
}

# par(mfrow=c(2,2))
# dbim(res$o$classifier, main="Oracle", asp=1)
# dbim(res$m$classifier, main="Machine", asp=1)
# dbim(res$l$classifier, main="Idiot", asp=1)
# dbim(res$ml$classifier, main="Both", asp=1)

# maybe not.



#' Plot the user estimate
user <- function(l){
  grid <- expand.grid(xseq<-seq(-2, 5, length=50), yseq<-seq(-3,3, length=50))
  v <- labeller_predict_alpha(l, X = grid)
  list(xseq=xseq, yseq=yseq, v=v)
}
plot.user.im <- function(u, p=F, ci=F, ...){
  library(spatstat)
  M <- im(xcol=u$xseq, yrow=u$yseq, mat=t(matrix(as.numeric(u$v), ncol=50)))
  plot(M, xlab="", ylab="", ...)
  
  if(p) points(X, cex=.5)
  if(ci) symbols(-1,-1, circles=1.5, inches=F, add=T, col="blue")
}

plot.user <- function(u, p=F, ci=F, ...){
  image(u$xseq, u$yseq, z=matrix(as.numeric(u$v), ncol=50), xlab="", ylab="", ...)
  if(p) points(X, cex=.5)
  if(ci) symbols(-1,-1, circles=1.5, inches=F, add=T, col="blue")
}



l<-user(res$l$labeller)
m <- user(res$m$labeller)
ml<-user(res$ml$labeller)
#u <- user(res$up)
# 
# par(mfrow=c(3,1))
# plot.user(l, zlim=zl<-c(0,5), col=cols<-rev(rainbow(10)), asp=1, main="Just labeller, 50 queries")
# plot.user(ml, zlim=zl, col=cols, asp=1, main="Both machine and labeller, 50 queries")
# plot.user(u, zlim=zl, col=cols, asp=1, main="All data to learn labeller")


#' plot the classifier on top of labeller
plotcl<- function (res, lab, data=data, i = 1, j = 2, zl=c(0, 1), ..., cols = rev(gray.colors(200, start=0, end=1))) {
  plot.user(lab, col=cols, zlim=zl, p=F, asp=1, ..., xaxt="n", yaxt="n")
  dis <- apply(data[,-1], 1, function(xy) sum((xy-c(-1,-1))^2) < 1.5^2)
  pch_d <- c(1, 2)[1+1*dis]
  points(data[, -1][, c(i, j)], col = data[, 1], cex = 0.8, pch=pch_d)
  asked <- data[res$asked, ]
  dis <- apply(asked[,-1], 1, function(xy) sum((xy-c(-1,-1))^2) < 1.5^2)
  pch_a <- c(19, 17)[1+1*dis]
  points(asked[, -1][, c(i, j)], pch = pch_a, col = asked[, 1])
#   require(ellipse)
#   for (k in 1:length(res$classifier$classes)) lines(ellipse(res$classifier$theta_S[[k]][c(i, j), c(i, j)], 
#                                                         centre = res$classifier$theta_m[c(i, j), k]), col = k)
}





#' Ok let's plot the estimates and learning curves:

pdf(file="demo_hole_results.pdf", width=8.5, height=8)
par(mfrow=c(2,2), mar=c(1,1,3,1))
plotcl(res$l, l, data, main="Just labeller information", zl=c(0,5))
plotcl(res$m, m, data, cols="white", main="Just classifier information")
plotcl(res$ml, ml, data, zl=c(0,5), main="Combined labeller and classifier information")
par(mar=c(3,4,3,1))
plot(res$o$test_score$Fscore, type="l", main="Learning curves", ylab="F-score", ylim=c(0.5, 1), lwd=3)
lines(res$m$test_score$Fscore, col=2, lty=2, lwd=2)
lines(res$l$test_score$Fscore, col=3, lty=3, lwd=2)
lines(res$ml$test_score$Fscore, col=4, lty=4, lwd=2)
legend("topleft", lty=1:4, col=1:4, lwd=2, c("Oracle", "Classifier", "Labeller", "Both"))
dev.off()









#' One final check (not saved): is the user estimation consistent? 
limit <- user(res$up)
plot.user.im(limit, asp=1, col=rainbow(200), ci=T, zlim=c(0,100))
# seems to work.




