
original_data <- read.csv("~/Rcode/pm10.csv", header = TRUE, stringsAsFactors = FALSE)
#original_data <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)

original_data2 <- original_data[, -c(1, 2)]

which(!complete.cases(original_data2)) #返回空，说明没有缺失值


set.seed(2019)
select <- sample(1:nrow(original_data2), nrow(original_data2)*0.8)
train <- original_data2[select, ]
test <- original_data2[-select, ]



#model1
#lm
fit1 <- lm(PM ~ ., data = train)
summary(fit1)
kappa(fit1) 
#通过查看fit1结果（F-value很小，但是很多系数却不显著）和kappa（用来检验多重共线性的，数值很大）
#说明这个数据存在严重的多重共线性。

#model2
#逐步回归
fit2 <- step(fit1, direction = "both")
summary(fit2)#查看结果，发现这下不错，step删减掉一些变量，然后各个系数都显著。

par(mfrow = c(2, 2))
plot(fit2)
par(mfrow = c(1, 1))
#画出的左上角是残差图，应该是白噪声，在0左右波动，但是有一点的趋势。而且也不是随机的在0左右
#波动，说明，数据还有信息没有被提取出来。
#右下角的杠杆图，可以看出数据有离异点，train数据集的58， 38， 39可能是离异点。（也就是坏点，
#可以删去试一试。

#计算一些fit2的mse
(MSE=mean((test$PM-predict(fit2,newdata=test))^2))
#463.23 这个结果还是很大，感觉不太理想，再换其他模型试一试。

pre_fit2 <- predict(fit2, newdata = test)
head(data.frame("真实值" = test$PM, "fit2预测值" = pre_fit2))
#可以看出效果不好。

#model 2 用树

# Build CART-rpart Model
library(rpart)
library(rpart.plot)

CART.tree <- rpart(PM ~ ., data=train,
                   method="anova",
                   control = rpart.control(xval = 10,
                                           minsplit = 3,
                                           cp = 0.05))
rpart.plot(CART.tree)
printcp(CART.tree)
# Make Predictions
pre_cart_tree <- predict(CART.tree, newdata=test, type='vector')
(MSE=mean((test$PM-pre_cart_tree)^2))
#244. 这个比上次好一点
head(data.frame("真实值" = test$PM, "fit2预测值" = pre_cart_tree))


#支持向量机
library(e1071)
SVR.Model <- svm(PM ~ .,data=train) 
summary(SVR.Model)

# Make Predictions
pre_svm <- predict(SVR.Model, test)
(MSE=mean((test$PM-pre_svm)^2))
#165这个结果好的多， 都没有调参数，就到165了。
head(data.frame("真实值" = test$PM, "fit2预测值" = pre_svm))
#总体来说，svm更好一点。








################################################################################
#特征工程
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
