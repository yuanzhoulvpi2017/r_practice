library(randomForest)
library(mlbench)
library(caret)
library(e1071)



data <- read.csv("2d.csv", header = TRUE)
data$class <- factor(data$class)


#
set.seed(2019)
sample_id <- sample(x = c(1:150), size = 150 * 0.8)

train_data <- data[sample_id, ]
test_data <- data[-sample_id, ]

#10 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(2019)
#Number randomely variable selected is mtry
mtry <- round(sqrt(ncol(train_data)))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(class~., 
                    data=train_data, 
                    method='rf', 
                    metric='Accuracy', 
                    tuneGrid=tunegrid, 
                    trControl=control)

rf_default$results
importance_ <- rf_default$finalModel$importance
dim(importance_) <- 815

sort_num <- order(importance_)

my_order <- order(importance_, decreasing = TRUE)



var_name_fun <- function(i) {
  library(caret)
  control <- trainControl(method='repeatedcv', 
                          number=10)#,repeats=3)
  set.seed(2019)
  tunegrid <- expand.grid(.mtry=29)
  #
  #select_data <- train_data[, names(train_data)[c(sort_num[1:i], 816)]]
  select_data <- train_data[,c(my_order[-c((816-i):815)], 816)]
  rf_default <- train(class~., 
                      data=select_data, 
                      method='rf', 
                      metric='Accuracy', 
                      tuneGrid=tunegrid, 
                      trControl=control)
  
  return(rf_default$results[1, 2])
}

#test
var_name_fun(i = 1)
var_name_fun(i = 814)

options(warn =-1)


# 启用parallel作为foreach并行计算的后端
library(doParallel)
library(foreach)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

#1:814
accuracy <- foreach(x=c(1,814)) %dopar% var_name_fun(x)

stopCluster(cl)

accuracy <- unlist(accuracy)

library(ggplot2)
data_accuracy <- data.frame(accuracy = accuracy, x = 1:length(accuracy))
ggplot(data = data_accuracy, aes(x = x, y = accuracy)) + geom_point() + geom_line()+
  ggtitle("accuracy") +
  theme(plot.title = element_text(hjust = 0.5))



i <- which.max(accuracy)
names(train_data[,c(my_order[-c((816-i):815)], 816)])#得到的变量就是可以达到最高
accuracy[i]


