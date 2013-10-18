#' simulate and estimate alpha

## Generate a 'hole' alpha:
sim_hole_alpha <- function(centre, radius=1, v_in=0.1, v_out=3) {
  function(x) {
    is_out <- colSums((t(rbind(x))-centre)^2) > radius^2
    c(v_in, v_out)[is_out+1]
  }
}


############################################################
# 2D only: evaluate alpha on a grid

alpha_to_2d_matrix <- function(alpha, xlim=c(-3,5), ylim=c(-3,3), res=100, ...){
  resx <- res
  resy <- round(diff(ylim)/diff(xlim) * resx)
  xy <- expand.grid(x<-seq(xlim[1], xlim[2], length=resx), y<-seq(ylim[1], ylim[2], length=resy))
  v <- alpha(xy)
  list(v = t(matrix(v, nrow=resy, byrow=T)), x=x, y=y)
}

plot_alpha2d <- function(alpha, ...){
  im <- alpha_to_2d_matrix(alpha, ...)
  image(im$v, x=im$x, y=im$y, ...)
}