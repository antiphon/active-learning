#' plot functions for this example

## plot the sampled points
plot_add_oracle <- function() symbols(rbind(co_circle), circles=co_radius, inches=FALSE, add=TRUE, col=4, lty=2)

require(ellipse)
plot_add_contour <- function(res) {
  for(i in 1:length(res$fit$classes)) lines(ellipse(res$fit$theta_S[[i]], 
                                            centre=res$fit$theta_m[,i]), col=i)
}


plot1 <- function(res, add_oracle=TRUE, reso=80, cols=rev(gray.colors(100)), ...) {
  xy <- expand.grid(xc<-seq(-3,5, length=reso), yc<-seq(-3,3, length=floor(0.66*reso)))
  a_hat <- res$fit$alpha(xy)
  a_im <- matrix(a_hat, ncol=floor(0.66*reso))
  image(a_im, x=xc, y=yc, col=cols)
  pchs <- c(1,2,5)
  points(data[,-1], col=data[,1], cex=.6, pch=pchs[as.numeric(data[,1])])
  plot_add_contour(res)
  pchs_sel <- c(16, 17, 18)
  y <- as.numeric(true_labels[res$asked,])
  points(dataUnlabeled[res$asked,], col=y, cex=1.2, 
         pch=pchs_sel[y])
  if(add_oracle)plot_add_oracle()
}