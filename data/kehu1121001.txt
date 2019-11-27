
#load data 
load_data <- read.csv("result1.csv")
load_data$class <- factor(load_data$class)


#splite data into train_data and test_data
sample_row <- sample(1:nrow(load_data), size = nrow(load_data) * 0.8)

train_data <- load_data[sample_row, ]
test_data <- load_data[-sample_row, ]



#
library(randomForest)
library(mlbench)
library(caret)
library(e1071)


#首次建模
rf_model <- randomForest(class~., data = train_data, ntree = 200, 
                         mtry=8, importance=TRUE, proximity=TRUE)
predict_ <- predict(rf_model,test_data)
accuracy = sum(predict_ == test_data[,'class'])/nrow(test_data)
importance <- rf_model$importance[,'MeanDecreaseAccuracy']


plot(rf_model)
#黑线是Out-of-Bag error rate，即用decision trees预测没有包括在bagging里样本的error
#。其他两条线为你DV的两个class。横坐标就是# of trees


import_df <- data.frame(var_name = names(train_data)[-62][order(importance, decreasing = TRUE)],
                        import_value = importance[order(importance, decreasing = TRUE)])
import_df #各个变量的重要性和对应的重要性的数值都给你搞出来了

#特征排序
#每个变量的重要性从大向小排列
library(ggplot2)
import_df$var_name <- factor(import_df$var_name, levels = names(train_data)[-62][order(importance, decreasing = TRUE)])
ggplot(data = import_df, aes(x = var_name, y = import_value)) + geom_bar(stat = 'identity') +
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(angle = -45))



#接下来依次删除重要性最小的变量一直删到只剩下最重要的变量为止
my_order <- order(importance, decreasing = TRUE)

#i <- 30
var_name <- function(i) {
  library(randomForest)
  select_data <- train_data[,c(my_order[-c((62-i):61)], 62)]
  k_n = 5     #这个是k-折如果为10，就是10折交叉验证，如果为5就五折交叉验证，
  #因为数据大，还有电脑性能，你自己选择。就是运行时间长短的问题
  sample_k_id <- sample(x = c(1:k_n), size = nrow(select_data), replace = TRUE)
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
  #k_rf()
  return(mean(unlist(lapply(c(1:k_n), FUN = k_rf))))
  
}

#test
var_name(1)
var_name(60)

#如果使用for，会非常慢，这里使用的是并行运算
library(doParallel)
library(foreach)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
accuracy <- foreach(x=c(1:60)) %dopar% var_name(x)
stopCluster(cl)
accuracy <- unlist(accuracy)

library(ggplot2)
is_big <- rep("small", time = length(accuracy))
is_big[which.max(accuracy)] = "big"
data_accuracy <- data.frame(accuracy = accuracy, 
                            x = 1:length(accuracy),
                            is_big = factor(is_big))
ggplot(data = data_accuracy, aes(x = x, y = accuracy)) + 
  geom_point(aes(colour = is_big)) + geom_line() +
  ggtitle("accuracy") +
  theme(plot.title = element_text(hjust = 0.5))


i <- which.max(accuracy)
names(train_data[,c(my_order[-c((62-i):61)], 62)])#得到的变量就是可以达到最高
accuracy[i]#这个就是最大准确率








#接下来使用pca降低维度，并且结合svm进行运算

library(factoextra)
library(FactoMineR)
library(corrplot)
res.pca <- PCA(train_data[, -62], graph = FALSE)
eig.val <- get_eigenvalue(res.pca)
fviz_eig(res.pca, addlabels = TRUE)
var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr=FALSE)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#如果不懂pca，可以参考这个网站
#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials


data_pca <- PCA(train_data[, -62], graph = FALSE)
fviz_pca_ind(data_pca,
             geom.ind = "point",
             col.ind = train_data$class,
             addEllipses = TRUE,
             legend.title = "class")



