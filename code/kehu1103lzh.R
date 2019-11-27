
dataset_ica1 <- read.csv("~/Rcode/dataset_ica1.csv")

which(!complete.cases(dataset_ica1)) #没有缺失值
summary(dataset_ica1)

library(ggplot2)
ggplot(data = dataset_ica1, aes(x = main_cardio, y = vo2max, col = main_cardio)) + stat_boxplot()
pairs(dataset_ica1[, c(-2, -6)])




lm_model1 <- lm(vo2max ~ ., data = dataset_ica1)
summary(lm_model1)
#这个是显示模型的一些性质

plot(lm_model1)



table(dataset_ica1$main_cardio)

lm_best <- step(lm_model1, direction = "both") #这个是逐步回归，选择最优的变量组合。但是
#你这个数据统计性质太好了，结果和lm_model1是一样的
summary(lm_best)


dataset_ica1[c(76, 106, 85, 126), ]   #这几个是异常点 , 你多运行几次就能发现


########################################
#分隔为训练集和测试集

id<- sample(1:nrow(dataset_ica1), size  = nrow(dataset_ica1) * 0.8) #分隔比例

train_datatset <- dataset_ica1[id, ]
test_dataset <- dataset_ica1[-id, ]


#训练模型： 
my_lm <- lm(vo2max ~ ., data = train_datatset) 
summary(my_lm)
#在测试集上查看结果
pred_on_test <- predict(my_lm, newdata = test_dataset)
(MSE <- sum(pred_on_test - test_dataset$vo2max)^2) #这个是均方误差，用来判断预测的好坏。

