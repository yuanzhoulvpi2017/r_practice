
dataset_ica1 <- read.csv("dataset_ica1.csv", as.is=TRUE)
which(!complete.cases(dataset_ica1)) #没有缺失值
summary(dataset_ica1)



####################################################################################################################################################################################################
#### 这个是不是应该放在一开始，因为我所有的模型都是一直用70个做的（打个比方70个哈），然后我全都完事了以后最后用那30去检测，是不是应该这样，这该咋改改，我不敢动了
#分隔为训练集和测试集

#answer 我是为你提供方法，并不是完全按照顺序，每个模块都是独立的，这个方法一般在机器学习上做，
#我也不知道适不适合这个统计学习，我感觉统计学习一般都是学习这个数据的统计信息，也并不是为了验证准确率，
#这个你自己决定。
id<- sample(1:nrow(dataset_ica1), size  = nrow(dataset_ica1) * 0.8) #分隔比例


train_datatset <- dataset_ica1[id, ]
test_dataset <- dataset_ica1[-id, ]


#训练模型： 
my_lm <- lm(vo2max ~ ., data = train_datatset) 
summary(my_lm)
#在测试集上查看结果
pred_on_test <- predict(my_lm, newdata = test_dataset)
(MSE <- sum(pred_on_test - test_dataset$vo2max)^2) / length(pred_on_test) #这个是均方误差，用来判断预测的好坏。
####################################################################################################################################################################################################







cor(dataset_ica1)
library(car)
scatterplotMatrix(dataset_ica1, spread = FALSE, smoother.args = list (lty=2), main = "Scatter Plot Matrix", col = "black")

# 想用 corplot看相关性，下面这个咋改编改编
# library(corrplot)
# x.temp <- x[,-c(4,7,10)]
# dim(x.temp)
# temp <- cor(as.matrix(na.omit(x.temp)),method="s")
#answer 因为你这个数据里面有分类变量，就是main_cardio这列，你去掉就可以画图了
#如下：
library(car)
scatterplotMatrix(dataset_ica1[, c(-6)], spread = FALSE, smoother.args = list (lty=2), main = "Scatter Plot Matrix", col = "black")







################################################################################################################################################################
#### 学长这个图咋没有main_cardio？有三幅图咋这么诡异是平行的而且还有等距的空隙？？？我没太懂这个图咋回事
library(ggplot2)
ggplot(data = dataset_ica1, aes(x = main_cardio, y = vo2max, col = main_cardio)) + stat_boxplot()
pairs(dataset_ica1[, c(-2, -6)])

#answer 第一个ggplot画的这个图是有main_cardio的，你仔细看看横坐标，main_cardio的各个类分别提出来，画箱型图.
#你可以查查boxplot这个东西。
#第二个pairs不能包含main_cardio，pairs要求数据框都为连续性,不能为character或者factor
################################################################################################################################################################




lm_model1 <- lm(vo2max ~ ., data = dataset_ica1)
summary(lm_model1)
# 除此之外Intercept的Std. Error和其他的相比贼大，11.68986，说明存在共线性了, 是不是该用VIF删了某个变量
#answer 这个统计上意义也说不准，你自己看这个办，主要看是否符合你要求，



lm_model1 <- lm(vo2max ~ ., data = dataset_ica1)
summary(lm_model1)
vif(lm_model1)
# sex和weight好像挺大，但也没有很大，刚到5，网上说如果这两个t检验都显著，基本可以不处理，可以吗？
# 然后又用了逐步回归，发现sex和weight也没啥事。那就留着还是咋整

#answer 你自己看着办，统计上意义说不准哇，哎，太假了


library(MASS)
# ？？？？？？ 向前, 向后,向前向后 里的main_cardio能不能拆出来啊，不拆的话不好和全子集回归的比较诶，全子集回归的全拆开了
########################################################################################
#answer：这个时候，你就可以使用dummy——variable了
#具体怎么做，我给演示一遍，我也不晓得对不对，我都凭感觉
#code:

#重新编码：
new_dummy_data <- dataset_ica1[]
levels(factor(dataset_ica1$main_cardio))

new_dummy_data$main_cardio_cycle <- ifelse(new_dummy_data$main_cardio=="cycle", 1, 0)
new_dummy_data$main_cardio_none <- ifelse(new_dummy_data$main_cardio=="none", 1, 0)
new_dummy_data$main_cardio_run <- ifelse(new_dummy_data$main_cardio=="run", 1, 0)
new_dummy_data$main_cardio_swim <- ifelse(new_dummy_data$main_cardio=="swim", 1, 0)
head(new_dummy_data)

new_dummy_data <- subset(new_dummy_data, select = -main_cardio)

#现在再做回归看一看
lm_dum_model_1 <- lm(vo2max ~ ., data = new_dummy_data)
summary(lm_dum_model_1)
#这个summary里面出现NA，不用紧张，好像是多重共线性原因。

lm_forward <- step(lm_dum_model_1, direction = "forward")
summary(lm_forward)
lm_both <- step(lm_dum_model_1, direction = "both")
lm_backward <- step(lm_dum_model_1, direction = "backward")

anova(lm_forward, lm_backward, lm_both)
#这结果就出来了，你自己度，你可以的


############################################################################

# 向前回归
lm_best <- lm(vo2max ~ ., data = dataset_ica1)
stepAIC(lm_best, direction = "backward")
# 向后回归
lm_best <- lm(vo2max ~ ., data = dataset_ica1)
stepAIC(lm_best, direction = "forward")
# 向前向后回归
lm_best <- lm(vo2max ~ ., data = dataset_ica1)
stepAIC(lm_best, direction = "both")
# 全子集回归
library(leaps)
leaps <- regsubsets(vo2max ~ ., data = dataset_ica1, nbest = 2)
plot(leaps, scale = "adjr2")

library(car)
subsets(leaps, statistic ="cp", main = "Cp Plot for all Subsets Regression")
abline(1, 1, lty = 2, col = "red")
summary(lm_best)
# 全子集回归，我们发现最上面那个main_cardiorun是白的，可以考虑删了它？同时在做summary(lm_model1)时，main_cardiorun和main_cardioswim没*， 是不是说明可以考虑用anova把他俩都删了或者删一个？
# ？？？？？ 好烦，我想拆开main_cardio，运行不出来，不过我估计结果是anova(lm_model1, lm_model2)不显著，这样就删main_cardiorun，正好和全子集方法对上了
lm_model1 <- lm(vo2max ~ ., data = dataset_ica1)
lm_model2 <- lm(vo2max ~ sex + age + height + weight + main_cardionone + main_cardiorun, data = dataset_ica1)
anova(lm_model1, lm_model2)

lm_model1 <- lm(vo2max ~ ., data = dataset_ica1)
lm_model3 <- lm(vo2max ~ sex + age + height + weight + main_cardionone + main_cardioswim, data = dataset_ica1)
anova(lm_model1, lm_model3)

# 对于summary(lm_model1)，R^2挺低的，模型拟合的不咋好，全子集建议删了main_cardiorun，得到summary(lm_best)，删了感觉能提高点？但是感觉还是不高，R^2一直挺低的

# 哦对，你昨天咋说体重和身高有共线性？我看是sex和weight有共线性啊

# 我想怎么找出交互项，就是斜率和截距都变了的那个. 感觉用个啥图能看出来？因为我不能硬说A和B感觉是交互的，这该咋整呢
# 我还想用展示出来交互项就是斜率和截距都变了的那个图



################################################################################################################################################################################################################################
# 回归诊断 (做这个的时候main_cardiorun已经删了，但我现在只能还用lm_model1做因为我之前不会弄233333)

lm_model1 <- lm(vo2max ~ ., data = dataset_ica1)
confint(lm_model1)  # main_cardiorun  main_cardioswim他俩的C.I.又包括0了，说明了啥？


plot(lm_model1)

dataset_ica1[c(76, 106, 85, 126), ]   #这几个是异常点 , 你多运行几次就能发现
##########  ??????  这几个异常点可咋整呢，不能直接删吧应该？那可咋整呢？

#answer:删不删你自己决定，都发现它了，还怕他跑？？

### 上面👆直接用plot()太粗糙了了，再深度检验一下
# 检验正态性 (俩图一起放) #
library(car)
lm_model1 <- lm(vo2max ~ ., data = dataset_ica1)
qqPlot(lm_model1, labels = row.names(lm_model1), id.method = "identify", simulate = TRUE, main = "Q-Q Plot", col = "black")


residplot <- function(lm_model1, nbreaks=10){
  z <- rstudent(lm_model1)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors",
       xlim = c(-6, 8))             ################看这里
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend=c("Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue", "red"), cex=.7)
}
residplot(lm_model1)
### 感觉我这个横轴设置的不咋好，有点往左，能不能往右挪挪，红线冒了，能不能填上把它
### 结论：还挺正态的吧

#answer这个是你设置的范围问题，可以改的，看182行

# 检验误差独立性
durbinWatsonTest(lm_model1)  ### 结论：误差还挺独立的

### 检验Linearity ###
library(car)
crPlots(lm_model1)           ### 结论：还挺线性的，不过可以看出有两个正比有两个反比，Interesting

### ？？？！！！ 检验constant error variance ###
library(car)
ncvTest(lm_model1)
spreadLevelPlot(lm_model1)   ### 结论：Suggested power transformation:  0.5364968 ，说明不满足constant error variance, 解决方法：
lm_model2 <- lm((vo2max)^0.5 ~ ., data = dataset_ica1)
ncvTest(lm_model2)
spreadLevelPlot(lm_model2)   ### 结论：满足constant error variance了









vif(lm_model1) #  ???我太难受了这个问题我在上面就该提的 忍不了了，这个vif咋长这样？是因为main_cardio包括三个？还是因为main_cardio包括三个非数值变量？能不能把main_cardio拆开？



################################################################################################################################################################################################################################
## 异常值观测（观测是能观测出来，咋弄他呢）

## 检查离群点 ##
library(car)
outlierTest(lm_model1) #？？？？？？  就一个76，我还想继续删126，106，85，都试试

## 检查高杠杆值点 ##
hat.plot <- function(lm_model1){
  p <- length(coefficients(lm_model1))
  n <- length(fitted(lm_model1))
  plot(hatvalues(lm_model1), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(lm_model1), names(hatvalues(lm_model1)))
}
hat.plot(lm_model1)

## 检查强影响点 ##

library(car)
avPlots(lm_model1, ask = FALSE, id.method = "identify")


## 综合检测：检查离群点, 杠杆值点,影响点 合并在一张图上 ##
library(car)
influencePlot(lm_model1, id.method = "identify", main = "Influence Plot", sub = "Circle size is proportional to Cook's Distance")
##？？？？？？？  我这水平轴有问题，咋没有0.2 0.3呢？左面那些都聚在一起了，想把它抻开
















table(dataset_ica1$main_cardio)


















