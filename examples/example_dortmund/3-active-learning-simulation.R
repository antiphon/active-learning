

### Data: ############################################################

source("load_data.R")

songs <- load_data()

str(songs, 1)

table(songs$train$genre)

init_idx <-
  unlist(lapply(split(1:length(songs$train$genre), songs$train$genre), sample, 5))

songs_init <- songs$train[init_idx, ]
songs_pool <- songs$train[-init_idx, ]

# library("tsne")
# p <- tsne(songs_init[, -1])
# plot(p, col = as.integer(songs_init[, 1]))


# library(seriation)
# d <- dist(songs_init[, -1])
# o <- seriate(d)
# pimage(d, o)



### Labellers: #######################################################

source("labellers.R")

l1 <- create_labeller_with_known_genres(songs_pool, c("folkcountry", "funksoulrnb"))

which(songs_pool$genre %in% c("rock", "blues"))

l1(songs_pool[1, -1])
l1(songs_pool[1:10, -1])


l2 <- create_labeller_with_known_genres(songs_pool, levels(songs_pool$genre))
l2(songs_pool[1:10, -1])



### Active learning loop: ############################################

wd <- setwd("../algorithm/")
source("active_learning_loop.R")
setwd(wd)

NN <- 100
eprior <- list(range=10, mu=2, kappa=1, s2=10, nugget=2)

# al_res <- active_learning(songs_init, songs_pool[, -1], oracle = l2, 
#                             true_labels = songs_pool$genre,
#                             alpha_prior = eprior, nsteps = NN, 
#                             combineI = function(a,b) sqrt(a*b),
#                             test_data=songs$test)

al_2genres <- active_learning(songs_init, songs_pool[, -1], oracle = l1, 
                          true_labels = songs_pool$genre,
                          alpha_prior = eprior, nsteps = NN, 
                          combineI = function(a,b) sqrt(a*b),
                          test_data=songs$test)

al_2genres_machine <- active_learning(songs_init, songs_pool[, -1], oracle = l1, est_alpha=FALSE,
                              true_labels = songs_pool$genre,
                              alpha_prior = eprior, nsteps = NN, 
                              combineI = function(a,b) a,
                              test_data=songs$test)

# al_machine <- active_learning(songs_init, songs_pool[, -1], oracle = l2, 
#                           true_labels = songs_pool$genre,
#                           alpha_prior = eprior, nsteps = NN, 
#                           combineI = function(a,b) a,
#                           test_data=songs$test)

# plot(al_res$test_error_hist, type = "b")
# lines(al_machine$test_error_hist, type = "b", col = "red")

plot(al_2genres$test_error_hist, type = "b", col = "red")
lines(al_2genres_machine$test_error_hist, type = "b", col = "blue")

table(songs_pool$genre[al_2genres$asked])
boxplot(al_2genres$Io_hist[50, ] ~ songs_pool$genre, las = 2)

al_2genres$Io_hist[50, ]
al_2genres$Im_hist[50, ]


############################################
#' PLOT
source("plot.R")
circle <- function() NULL#symbols(co[1], co[2], circles=ro, add=T, inches=F, lty=2)
#
pdf(file="3_fields_and_tsplot.pdf", width=5, height=4)
par(mfrow=c(2,2),mar=c(4,2,2,2), cex=.5)
plotd(res_just_user, data1, 1,2, reso<-50); circle(); title("Just labeller inform.")
plotd(res_just_machine, data1, 1,2,reso, col="gray90"); circle(); title("Just classifier inform.")
plotd(res_both, data1, 1,2,reso); circle(); title("Both")

## Test error rate
lc <- c("goldenrod", "maroon1", "dodgerblue")
ts.plot(res_just_user$test_error_hist, ylim=c(0,1), lty=3, col=lc[3], xlab="Iteration", main="Classification rate")
lines(res_just_machine$test_error_hist, lty=2, col=lc[2])
lines(res_both$test_error_hist, lty=1, col=lc[1])
legend("bottomright", c("just labeller", "just classifier", "both"), col=lc[3:1], lty=3:1, bty="n", cex=.8)

#' add full data training error
library(e1071)
tm <- naiveBayes(y~., data=data1)
te <- mean(predict(tm, newdata=test_data[,-1])==test_data[,1])
abline(h=te, col="gray60")

dev.off()




