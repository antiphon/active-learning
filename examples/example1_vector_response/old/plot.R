require(lattice)

plot1 <- function(res, data, reso=30) {
  i<-1
  j<-2
  ## i,j are the selected dimensions
  xr <- range(data[,i+1]) + c(-1, 1)
  yr <- range(data[,j+1]) + c(-1, 1)
  asp <- diff(yr)/diff(xr)
  # grid locations
  xy <- expand.grid(xc<-seq(xr[1], xr[2], length=reso), yc<-seq(yr[1], yr[2], length=floor(asp*reso)))
  ## training data in 2d
  xy_data <- data[res$asked, c(i,j)+1]
  xy_y <- data[res$asked, 1]
  ## make a predictor of alpha in this plane
  if(res$fit$est_alpha) {
    alpha <- predictor_alpha(res$fit$a_est, xy_data)
    a_hat <- alpha(xy)
  }else a_hat <- rep(1, nrow(xy))
  a_im <- matrix(a_hat, ncol=floor(asp*reso))
  xname <- colnames(data)[i+1]
  yname <- colnames(data)[j+1]
  image(a_im, x=xc, y=yc, col=rev(gray.colors(100)), xlab="", ylab="", xaxt="n", yaxt="n")
#   p<-contourplot(c(a_im)~xy[,1]*xy[,2], region=T, col.regions=rev(gray.colors(1000)), cuts=10,
#               colorkey=list(at=seq(0,14,by=2)), xlab="" , ylab="", asp=1)
#   print(p)
#   ## add datapoints
  pchs <- c(1,2,5)
  #trellis.focus()
  points(data[,c(i,j)+1], col=data[,1], cex=.6, pch=pchs[as.numeric(data[,1])])
  ## add sampled points
  pchs_sel <- c(16, 17, 18)
  #trellis.focus()
  points(xy_data, col=xy_y, cex=1, pch=pchs_sel[xy_y])
}