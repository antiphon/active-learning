
classifier_plot <- function(classifier, data, i=1, j=2, asked=NULL) {
  plot(data[,-1][,c(i,j)], col=data[,1], cex=0.8)
  asked <- data[asked, ]
  points(asked[,-1][,c(i,j)], pch=19, col=asked[,1])
  require(ellipse)
  for(k in 1:length(classifier$classes)) 
    lines(ellipse(classifier$theta_S[[k]][c(i,j), c(i,j)], 
                  centre=classifier$theta_m[c(i,j),k]), col=k)  
}