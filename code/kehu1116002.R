#本篇参考了两个kernel。都是支持率非常高的。
#https://www.kaggle.com/esmaeil391/ibm-hr-analysis-with-90-3-acc-and-89-auc#Feature-Engineering
#https://www.kaggle.com/arthurtok/employee-attrition-via-ensemble-tree-based-methods#2.-Feature-Engineering-&-Categorical-Encoding

#没有做描述性统计分析。直接是特征工程
#相对数据中被错误定性的改过来（factor，numerical）
#然后区分开来分别分析
#code line 17
#code line 52
#code line 90






original_data <- read.csv("IBM.csv")
names(original_data)[1] = 'Age'
summary(original_data)

which(!complete.cases(original_data)) #没有缺失值

##################################################################################
#事先处理一下。调整一下因子和数值型
#观察在调整
original_data$Education <- factor(original_data$Education)
summary(original_data$EmployeeCount) #可以看出这个变量没有用，要踢掉
original_data$EnvironmentSatisfaction <- as.numeric(original_data$EnvironmentSatisfaction)
original_data$JobInvolvement <- factor(original_data$JobInvolvement)
original_data$JobLevel <- factor(original_data$JobLevel)
original_data$JobSatisfaction <- as.numeric(original_data$JobSatisfaction)
original_data$StockOptionLevel <- as.factor(original_data$StockOptionLevel)


#先将数据中为因子列提出来
factor_names <- names(original_data)[unlist(lapply(names(original_data), FUN = function(x){is.factor(original_data[, x])}))]
#这里面有个over18应该是分类变量，但是全都是Y。所以把这列给踢出去
#而且Arrrition是目标变量，也踢出去
factor_names <- factor_names[-c(1, 11)]
factor_data <- original_data[, factor_names]
str(factor_data)
factor_data 

#做dummay 处理（哑变量）
library(caret)
dummy_model <- dummyVars(~., factor_data)
dummy_data <- data.frame(predict(dummy_model, newdata = factor_data))
head(dummy_data)

#对于两分类的因子变量，我们在进行虚拟变量处理后可
#能不需要出现代表相同意思的两列（例如：OverTime.No和OverTime.Yes)。
#这时候我们可以利用dummyVars函数中的fullRank参数，将此参数设置为TRUE
dummy_model <- dummyVars(~., factor_data, fullRank=TRUE)
dummy_data <- data.frame(predict(dummy_model, newdata = factor_data))
head(dummy_data)

###################################################################################
#将data.frame里面的数值型提取出来
numerical_names <- names(original_data)[!unlist(lapply(names(original_data), 
                                                       FUN = function(x){is.factor(original_data[, x])}))]
numerical_names <- numerical_names[c(-4)]
numerical_data <- original_data[, numerical_names]


numerical_data$aveaSatisfaction <- mean(
  as.numeric(numerical_data$EnvironmentSatisfaction)+
  as.numeric(numerical_data$JobSatisfaction)+
  as.numeric(numerical_data$RelationshipSatisfaction)+
  as.numeric(numerical_data$WorkLifeBalance))
#然后将上面合并后的列剔除掉
numerical_data <- numerical_data[, !names(numerical_data) %in% 
                                   c("EnvironmentSatisfaction",
                                       "JobSatisfaction",
                                       "RelationshipSatisfaction",
                                       "WorkLifeBalance")]

#将每个月的工资做一下处理。
#大于均值为1， 小于均值为0，也就不标记为high,low省的还要dummay一下
mean_monthly_income <- mean(numerical_data$MonthlyIncome)
numerical_data$MonthlyIncome <- as.factor(ifelse(
  numerical_data$MonthlyIncome, 1, 0 
))

#对年龄分类一下
numerical_data$Age <- as.factor(ifelse(numerical_data$Age < 29, "Young",
                                       ifelse(numerical_data$Age <= 50, "Mid", "Adult")))
dummy_model_age <- dummyVars(~Age, numerical_data, fullRank=TRUE)
dummy_data_age <- data.frame(predict(dummy_model_age, newdata = numerical_data))
#将dummy_data_age合并到numerical_data, 并剔除Age
numerical_data <- cbind(numerical_data, dummy_data_age)
numerical_data <- numerical_data[, !names(numerical_data) %in% c("Age")]


########################################################################################
#最后将factor的data.frame和numerical的data.frame合并
clearn_data <- cbind(dummy_data, numerical_data)
clearn_data$Attrition <- original_data$Attrition
names(clearn_data)
clearn_data <- clearn_data[which(complete.cases(clearn_data)), ]


clearn_data <- subset(clearn_data, select = -MonthlyIncome)

str(clearn_data)





#随机森林
library(randomForest)
library(mlbench)
library(caret)
library(e1071)


#
set.seed(2019)
sample_id <- sample(x = c(1:1470), size = 1470 * 0.8)

train_data <- clearn_data[sample_id, ]
test_data <- clearn_data[-sample_id, ]

#10 folds repeat 3 times
#10折交叉验证
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(2019)
#Number randomely variable selected is mtry
mtry <- round(sqrt(ncol(train_data)))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Attrition ~ ., 
                    data=train_data, 
                    method='rf', 
                    metric='Accuracy', 
                    tuneGrid=tunegrid, 
                    trControl=control)

rf_default$results #这个是准确率


importance_ <- rf_default$finalModel$importance
dim(importance_) <- 53

import_df <- data.frame(var_name = names(clearn_data)[-54][order(importance_, decreasing = TRUE)],
                        import_value = importance_[order(importance_, decreasing = TRUE)])
import_df #各个变量的重要性和对应的重要性的数值都给你搞出来了

#特征排序
library(ggplot2)
import_df$var_name <- factor(import_df$var_name, levels = names(clearn_data)[-54][order(importance_, decreasing = TRUE)])
ggplot(data = import_df, aes(x = var_name, y = import_value)) + geom_bar(stat = 'identity') +
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(angle = -45))
