install.packages("ROCR")
data <- read.table("~/Rcode/FleaBeetles.txt", header = FALSE)
names(data) <- c("Species", "widths", "corr_mean")
data$Species <- factor(data$Species)#要将Speices变成因子
head(data)

obj_data <- data.frame("widths" = 190, "corr_mean" = 125) #这个是我们目标数据


###逻辑回归
model_glm <- glm(Species ~ widths + corr_mean, data = data, 
                 family = binomial(link="logit"),control=list(maxit=1000))
summary(model_glm)

plot(model_glm)

result <- predict(model_glm, newdata = obj_data,type = "response")
(result <- ifelse(result <= 0.5, 1, 2))

#在训练集上查看效果，因为数据太少了，也没法将数据分成训练集和测试集
#plot auc
library(ROCR)

prob.traing <- predict(model_glm, type = "response")
pred <- prediction(prob.traing, data$Species)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, col = 2, lwd = 3, main = "ROC curve")
abline(0, 1)




#################################################################
library(MASS)
model_lda <- lda(Species ~ widths + corr_mean, data = data)
summary(model_lda)

predict(model_lda, newdata = obj_data)
plot(model_lda)
