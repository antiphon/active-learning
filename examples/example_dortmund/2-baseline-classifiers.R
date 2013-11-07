#' try my QDA classifier
wd<-getwd()
setwd("../algorithm/")
source("../algorithm/fitting.R")
source("../algorithm/predict_and_informativeness.R")
setwd(wd)
source("load_data.R")
songs <- load_data()
#

fit<-fitM0(data0=songs$train)
b<-classify(songs$test[,-1], fit)
mye <- mean(b!=songs$test[,1])
# 
# #
library(e1071)
sv<-svm(genre~., data=songs$train)
svme <- mean(predict(sv, newdata=songs$test[,-1]) != songs$test[,1])

library(randomForest)
rf <- randomForest(genre~., data=songs$train)
rfe <- mean(predict(rf, newdata=songs$test[,-1])!=songs$test[,1])
#
print(data.frame(mye, svme, rfe))

# Whee, it works!
