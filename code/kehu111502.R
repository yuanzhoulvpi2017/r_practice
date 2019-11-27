data1 <- read.csv("gasoline.csv")
library(neuralnet)

nn1 <- neuralnet(consumption ~ ., data = data1, hidden = c(2, 1))
plot(nn1)

#预测然后看MSE均方误差
(mse <- sum((data1$consumption - predict(nn1, newdata = data1))^2) / length(data1))



##########################################################
data2 <-read.csv("dividendinfo.csv")
data2$dividend <- factor(data2$dividend)

nn2 <- neuralnet(dividend ~ ., data = data2, hidden = c(2, 1), linear.output = FALSE)
plot(nn2)

#预测
prob.training <- predict(nn2, newdata = data2, type = "response")

pred <- c(0, 1)[max.col(prob.training)]
table(pred = pred, real = data2$dividend)
