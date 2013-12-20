#' testing

source("../example2_combined/data.R")

source("../algorithm/active_learning_loop_v2.R")

##
source("labellers.R")
oracle <- create_oracle_all_knowing(data)
## 
score0 <- function(cl0) {
  pred <- predict(cl0, newdata=test_data[,-1])
  mean(1*(pred==test_data[,1]))
}

score <- function(cl) {
  pred <- classifier_classify(cl, test_data[,-1])
  mean(1*(pred==test_data[,1]))
}

##
library(e1071)
#data0[,1] <- sample(data0[,1])
cl <- classifier_initial(data0[,-1], data0[,1], prior_sd=1)
dd<- data0
cl0 <- naiveBayes(label~., dd)
sh <- score(cl)
sh0 <- score0(cl0)

for(i in 1:5){
  classifier_plot(cl, data=data)
  inew <- sample(setdiff(1:nrow(dataUnlabeled), cl$asked), 1)
  xnew <- dataUnlabeled[inew,]
  dd <- rbind(dd, data1[inew,])
  cl0 <- naiveBayes(label~., dd)
  sh0 <- c(sh0 ,  score0( cl0 ) )
  hnew <- oracle(xnew)$h
  cl <- classifier_update(cl, xnew, hnew)
  sh <- c(sh ,  score( cl ) )
  cl$asked <- c(cl$asked, inew)
  
}

ts.plot(sh, ylim=c(0,1))
lines(sh0, col=2)

