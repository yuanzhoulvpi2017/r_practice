dat <- read.table("D:/data/1027/LIHC_OS.txt", sep = "\t", header = TRUE)
dat <- read.table("~/Rcode/dsy01.txt", sep = "\t", header = TRUE)
dat <- read.table("D:/data/lassoSigExp.txt", header = TRUE)
library(nnet)
library(neuralnet)
names(dat)

# Generate Training & Test Datasets
set.seed(2019)
select <- sample(1:nrow(dat),nrow(dat)*0.8)
train <- dat[select,]
test <- dat[-select,]
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
train[, names(train)[3:length(names(train))]]<-lapply(train[, names(train)[3:length(names(train))]], normalize)

summary(train[, names(train)[3:length(names(train))]])
pairs(train[, c(-1, -3)])
library(corrplot)
corrplot(cor(dat[, c(-1, -3)]))
library("PerformanceAnalytics")
chart.Correlation(train[, c(-1, -3)])

################################################################################
nn_data <- dat[, c(-1, -2)]
nn1 <- neuralnet(futime ~ .,
                 data = nn_data,hidden = 10, stepmax=1e7)
plot(nn1)

#########################################################################3
nn_data <- dat[, c(-1, -2)]
nn_data$fustat <- factor(nn_data$fustat)
nn2 <- neuralnet(fustat ~ .,
                 data = nn_data,hidden = 10, stepmax=1e7, linear.output = FALSE)
plot(nn2)
pred <- ifelse(predict(nn2, newdata = nn_data, type = "response")[, 1] <= 0.5, 1, 0)
table(pred = pred, real = nn_data$fustat)

##############################################
sum((predict(nn1, newdata = nn_data) - nn_data$futime) ^ 2) / 423





train$fustat <- factor(train$fustat)
nn1 <- neuralnet(futime ~ B + NK + CD4T + CD8T + Mono + Neutro + Eosino,
                 data = train,algorithm = "rprop+",
                 hidden=c(5),
                 threshold=0.5,
                 stepmax = 1e+06)
nn1 <- neuralnet(futime ~.,
                 data = nn_data,algorithm = "rprop+", stepmax = 1e+7,
                 hidden=c(5))
plot(nn1)

pr.nn <- compute(nn1,train)
pr.nn_ <- pr.nn$net.result*(max(train)-min(exp(train$futime)))+min(exp(train$futime))
test.r <- (test_$medv)*(max(exp(train$futime))-min(exp(train$futime)))+min(exp(train$futime))
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
nn1_predict <- predict(nn1, train)
sum((exp(nn1_predict) - train$futime)^2) / nrow(train)
#gwplot(nn1)

plot(density(log(dat$futime)))

result <- unlist(lapply(seq(1, nrow(train)), FUN = function(x) {which.max(nn1_predict[x, ])}))
pred_nn1 <- ifelse(result == 1, 0, 1)
pred_nn1 <- ifelse(nn1_predict > 0.5, 1, 0)





library(boot)
set.seed(200)
lm.fit <- lm(futime ~ .,data=nn_data)

lm_pred <- predict(lm.fit, newdata = train)
summary(lm.fit)


library(factoextra)
library(FactoMineR)
res.pca <- PCA(dat[, c(-1, -2, -3)], graph = FALSE)

fviz_eig(res.pca, addlabels = TRUE)
var <- get_pca_var(res.pca)
fviz_pca_var(res.pca, col.var = "black")
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

str(res.pca$ind$cos2)
head(res.pca$ind$cos2)



res.pca <- PCA(dat[, c(-1, -2, -3)], graph = FALSE, ncp = 10)
new_nn_data <- as.data.frame(res.pca$ind$cos2)
new_nn_data$futime <- dat$futime
head(new_nn_data)
nn1 <- neuralnet(futime ~ .,
                 data = new_nn_data,hidden = 3, )
plot(nn1)

lm_1 <- lm(futime ~ ., data = new_nn_data)
summary(lm_1)

library(corrplot)
corrplot(cor(new_nn_data))


library(e1071)
svr <- svm(futime ~ ., data = new_nn_data)
summary(svr)

cor(dat$futime, dat$ZNF750)
cor_all <- function(x) {
  if (length(dat$futime) != length(as.numeric(unlist(dat[, colnames(dat)[x], drop = TRUE])))) {
    return(0)
  } else {
    return(cor(dat$futime, as.numeric(unlist(dat[, colnames(dat)[x], drop = TRUE]))))
  }
  
}
cor_data <- vapply((1:ncol(dat))[c(-1, -2, -3)], FUN = cor_all, FUN.VALUE = x)
index_sort <- sort(abs(cor_data), decreasing = TRUE, index.return = TRUE)

new_cor_data <- dat[, index_sort$ix[1:20]]
head(new_cor_data)
new_cor_data$futime <- dat$futime
lm_2 <- lm(futime ~ ., data = new_cor_data)
summary(lm_2)


nn2 <- neuralnet(futime ~ ., data = new_cor_data, hidden = c(10, 5, 2))
plot(nn2)

pred_new_data <- predict(nn2, newdata = new_cor_data)
