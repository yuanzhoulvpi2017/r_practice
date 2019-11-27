library(readxl)
data <- read_xlsx("truck(1).xlsx")

names(data)

new_data <- data[, c(-1, -11,-18)]
new_data$is.safe <- factor(new_data$is.safe)

logit <- glm(is.safe ~ ., data = new_data,family = binomial("logit"))

summary(logit)

prob.training <- predict(logit, type = "response")
round(prob.training, 2)

table(pred = ifelse(prob.training < 0.5, 0, 1), real = new_data$is.safe)



library(ROCR)
pred <- prediction(prob.training, new_data$is.safe)
plot(performance(pred, measure = "tpr", x.measure = "fpr"), col = 2, lwd = 3, main = "roc curve")
abline(0, 1)


library(fmsb)

plot_radar_data <- rbind(apply(new_data[, -15], 2, max),
                         apply(new_data[, -15], 2, min),
                         new_data[c(1, 2, 3),-15])
radarchart(plot_radar_data)







