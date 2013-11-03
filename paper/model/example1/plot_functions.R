#' plot functions for this example

## plot the sampled points
plot_add_oracle <- function() symbols(rbind(co_circlec), circles=co_radius, inches=FALSE, add=TRUE, col=4, lty=2)

require(ellipse)
plot_add_contour <- function(res) {
  for(i in 1:length(res$fit$classes)) lines(ellipse(0, scale=res$fit$theta_sd[,i], 
                                            centre=res$fit$theta_m[,i]), col=i)
}


plot1 <- function(res, xy, ...) {
  size <- exp(res$I_hist[nsteps,])/2#.1+table(factor(res$asked, levels=1:nrow(U)))
  n <- nrow(U)
  pc <- rep(1, n); pc[unique(res$asked)]<-19
  col <- as.numeric(true_labels)
  plot(xy, col = col, cex=size, pch=pc,...)
  plot_add_oracle()
  plot_add_contour(res)
}
