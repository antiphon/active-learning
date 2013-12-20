require(lattice)

plot1 <- function(res, data, reso=30, zlim=NULL) {
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
  a_hat <- scale(log(labeller_predict_alpha(res$labeller, xy)), scale=F)
  
  a_im <- matrix(a_hat, ncol=floor(asp*reso))
  
  # log scale
  xname <- colnames(data)[i+1]
  yname <- colnames(data)[j+1]
  if(is.null(zlim))zlim <- range(a_im)
  print(zlim)
  image(a_im, x=xc, y=yc, col=rev(gray.colors(100)), xlab="", ylab="", xaxt="n", yaxt="n", zlim=zlim)
  ## add datapoints
  pchs <- c(1,2,5)
  points(data[,c(i,j)+1], col=data[,1], cex=.6, pch=pchs[as.numeric(data[,1])])
  ## add sampled points
  pchs_sel <- c(16, 17, 18)
  points(xy_data, col=xy_y, cex=1, pch=pchs_sel[xy_y])
}