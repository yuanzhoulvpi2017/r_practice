##PCA画图
# install.packages('devtools')
library(devtools)
install_github('fawda123/ggord')
library(ggord)
library(ggplot2)

x <- read.csv("2d.csv")
ord <- prcomp(x[,-ncol(x)])
p <- ggord(ord, x$class, poly = FALSE, polylntyp = x$class,arrow=0, vec_ext =0,txt=NULL,ellipse_pro=0.99)
p + scale_shape_manual('Groups', values = c(15, 16, 17, 18, 19))# + ylim(-2.5, 2.5) + ylab('PC2(6.06%)')# + xlab('D1')

##随机森林
#划分数据???
x_train <- x[1:103,]
x_test <- x[104:150,]
#准备工作
library(randomForest)
x_train$class = as.factor(x_train$class)
x_test$class = as.factor(x_test$class)
least_importance = c()
accuracy = c()
max_accuracy = 0
#首次建模
rf_model <- randomForest(class~., data = x_train, ntree = 200, mtry=8, importance=TRUE, proximity=TRUE)
x_predict <- predict(rf_model,x_test[,-ncol(x_test)])
accuracy[1] = sum(x_predict == x_test[,ncol(x_test)])/nrow(x_test)
importance <- rf_model$importance[,'MeanDecreaseAccuracy']
summary(rf_model)
my_import <- importance(rf_model)
importance[order(importance, decreasing = TRUE)]

if(accuracy[1] >= max_accuracy){
  max_accuracy = accuracy[1]
  best_var_group = least_importance   #记录最优删除变???
}
#循环建模(需运行???10分钟)
for(i in 2:(ncol(x)-1)){
  x_train_temp <- x_train[,-least_importance]
  x_test_temp <- x_test[,-least_importance]
  rf_model <- randomForest(class~., data = x_train_temp, ntree = 200, mtry=8, importance=TRUE, proximity=TRUE)
  x_predict <- predict(rf_model,x_test_temp[,-ncol(x_test_temp)])
  accuracy[i] = sum(x_predict == x_test_temp[,ncol(x_test_temp)])/nrow(x_test_temp)
  importance <- rf_model$importance[,'MeanDecreaseAccuracy']
  least_importance[i] <- which.min(importance)  #最不重要的一个变???
  if(accuracy[i] >= max_accuracy){
    max_accuracy = accuracy[i]
    best_var_group = least_importance   #记录最优删除变???
  }
}
#准确率与删除变量个数的关???
plot(accuracy,type = 'l',xlab = 'Number of deleted variables')  
#因测试集数量过少,各模型拟合准确的数量都在27-31之间,准确率只有几个选???,因此呈现这样的图???

#最高准确率的变量组合有很多???,最优变量组合为使用变量最???(即删除变量最???)的组???
best_var_group


