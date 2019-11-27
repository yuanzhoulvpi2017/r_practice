
original_data <- read.csv("~/Rcode/pm10.csv", header = TRUE, stringsAsFactors = FALSE)
#original_data <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

original_data2 <- original_data[, -c(1, 2)]

which(!complete.cases(original_data2)) #返回空，说明没有缺失值

set.seed(2019)
select <- sample(1:nrow(original_data2), nrow(original_data2)*0.8)

hist(original_data2$PM, 150)
plot(density(original_data$PM))

#虽然要预测连续值，但是我们依然可以将变量转换成分类型，将连续变量用cut函数进行分组切割。然后变成分类型。
#然后用支持向量机进行分类，总体上分类正确了，比如再混淆矩阵上成主对角线排列，那么结果就很好，但是查看了下面结果
#并不优秀，样本分布不均匀

copy_data <- original_data2[]

for(i in 2:17) {
  plot(density(copy_data[[i]]))
  copy_data[[i]] <- cut(copy_data[[i]], breaks=5)
  #test[[i]] <- cut(test[[i]], breaks=5)
}

#copy_data[, c(2:17)] = scale(copy_data[, -1])
copy_data$PM <- cut(copy_data$PM, breaks = c(0, 30, 35, 40, 45, 50, 55, 60, 70, 80, 150, Inf))

head(copy_data)
train <- copy_data[select,]
test <- copy_data[-select,]
library(e1071)
SVC.Model <- svm(PM ~ .,data=train) 
summary(SVC.Model)

pre_svm <- predict(SVC.Model, train)

results <- table(Prediction=pre_svm, Actual=train$PM)
results #混淆矩阵
Correct_Rate <- sum(diag(results)) / sum(results) 
Correct_Rate


# Build CHAID Model
install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID)

CHAID.tree <- chaid(PM ~ ., data=train)
plot(CHAID.tree)

# Make Predictions
CHAID.Prediction <- predict(CHAID.tree, newdata=train, type='response')
results <- table(Prediction=CHAID.Prediction, Actual=train$PM)
results #混淆矩阵

Correct_Rate <- sum(diag(results)) / sum(results) 
Correct_Rate
