library(randomForest)
library(mlbench)
library(caret)
library(e1071)



data <- read.csv("2d.csv", header = TRUE)
data <- data[1:150, ]
data$class <- factor(data$class)

set.seed(2019)
sample_id <- sample(x = c(1:150), size = 150 * 0.8)

train_data <- data[sample_id, ]
test_data <- data[-sample_id, ]


#首次建模
rf_model <- randomForest(class~., data = train_data, ntree = 200, 
                         mtry=8, importance=TRUE, proximity=TRUE)
predict_ <- predict(rf_model,test_data)
accuracy = sum(predict_ == test_data[,'class'])/nrow(test_data)
importance <- rf_model$importance[,'MeanDecreaseAccuracy']

#importance[order(importance, decreasing = TRUE)][c(1, 800)]


#i <- 500
#i <- 1
#i <- 814


my_order <- order(importance, decreasing = TRUE)

#names(train_data[,c(my_order[-c((816-i):815)], 816)])



var_name <- function(i) {
  library(randomForest)
  select_data <- train_data[,c(my_order[-c((816-i):815)], 816)]
  sample_k_id <- sample(x = c(1:10), size = 120, replace = TRUE)
  k = 10
  k_rf <- function(k) {
    k_train_data <- select_data[!(sample_k_id==k), ]
    k_test_data <- select_data[(sample_k_id==k), ]
    rf_model_temp <- randomForest(class~., data = k_train_data, ntree = 200, 
                                  mtry=sqrt(round(ncol(k_train_data))), 
                                  importance=TRUE, proximity=TRUE)
    predict_temp <- predict(rf_model_temp,newdata = k_test_data)
    accuracy = sum(predict_temp == k_test_data[,'class'])/nrow(k_test_data)
    return(accuracy)
  }
  
  #k_rf(k = 4)
  return(mean(unlist(lapply(c(1:10), FUN = k_rf))))
  
}

#test
var_name(1)
var_name(814)


library(doParallel)
library(foreach)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

accuracy <- foreach(x=c(1,814)) %dopar% var_name(x)

stopCluster(cl)

accuracy <- unlist(accuracy)

library(ggplot2)
 
is_big <- rep("small", time = length(accuracy))
is_big[which.max(accuracy)] = "big"
data_accuracy <- data.frame(accuracy = accuracy, 
                            x = 1:length(accuracy),
                            is_big = factor(is_big))
ggplot(data = data_accuracy, aes(x = x, y = accuracy)) + 
  geom_point(aes(colour = is_big)) + geom_line()


i <- which.max(accuracy)
names(train_data[,c(my_order[-c((816-i):815)], 816)])#得到的变量就是可以达到最高
accuracy[i]
