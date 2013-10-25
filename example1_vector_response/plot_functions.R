#' plot functions for this example

## plot the sampled points

require(ellipse)
plot_add_contour <- function(res, i, j) {
  for(k in 1:length(res$fit$classes)) lines(ellipse(res$fit$theta_S[[k]][c(i,j), c(i,j)], 
                                            centre=res$fit$theta_m[c(i,j),k]), col=k)
}

plotd <- function(res, data, i=1, j=2, reso=30, cols=rev(gray.colors(100))) {
  ## i,j are the selected dimensions
  xr <- range(data[,i+1]) + c(-1, 1)
  yr <- range(data[,j+1]) + c(-1, 1)
  asp <- diff(yr)/diff(xr)
  # grid locations
  xy <- expand.grid(xc<-seq(xr[1], xr[2], length=reso), yc<-seq(yr[1], yr[2], length=floor(asp*reso)))
  ## training data in 2d
  xy_data <- data[res$asked, c(i,j)+1]
  ## make a predictor of alpha in this plane
  if(res$fit$est_alpha) {
    alpha <- predictor_alpha(res$fit$a_est, xy_data)
    a_hat <- alpha(xy)
  }else a_hat <- rep(1, nrow(xy))
  a_im <- matrix(a_hat, ncol=floor(asp*reso))
  xname <- colnames(data)[i+1]
  yname <- colnames(data)[j+1]
  image(a_im, x=xc, y=yc, col=cols, xlab=xname, ylab=yname, xaxt="n", yaxt="n")
  ## add datapoints
  points(data[,c(i,j)+1], col=data[,1], cex=.6, pch=19)
  ## add clusters
  plot_add_contour(res, i, j)
  ## add sampled points
  points(xy_data, col=1+length(res$fit$classes), cex=1.2, lwd=1)
}


grid_plot <- function(res, data){
  d <- ncol(data)-1
  par(mfrow=c(d,d), cex=0.8, mar=c(1,1,1,1))
  k<-1
  for(i in 1:d){
    for(j in 1:d){
      if(k==j){ 
        plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="n", ylab="", xaxt="n", yaxt="n")
        text(0.5, 0.5, colnames(data)[i+1])
      }
      else plotd(res, data, i, j)
    }
    k<-k+1
  }
  
}

