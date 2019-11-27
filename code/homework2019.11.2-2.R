##Q1
#install.packages("readstata13")
library(readstata13)
DT <- read.dta13("brexit.dta")
DT <- na.omit(DT)
#1
plot(DT$b_migr11,DT$pct_leave)
lm1 <- lm(pct_leave~b_migr11,data = DT)
summary(lm1)
##从图及线性拟合结果来看，国外出生居民比例越高，支持休假选民的百分比越低

#2
DT$change_B <- DT$b_migr11-DT$b_migr##移民差异变量（新构造）
lm2 <- lm(pct_leave~b_migr11+change_B,data = DT)
summary(lm2)
##从拟合结果来看，移民比例差异与2011年国外出生居民比例两个变量对支持休假选民百分比均有极显著影响

#3
#从b拟合的系数来看，如果移民比例不变，国外出生居民比例

#pct_leave=58.0118-1.1623*b_migr11+1.0624*change_B

#ifchange_B==0
DT$pct_leave2 <- 58.0118-1.1623*DT$b_migr11
DT$pct_leave

ss <- which(DT$pct_leave>50 & DT$pct_leave2<50)

length(ss)/nrow(DT)
##支持比例有变化占比34.41%

#4
#有很多原因，比如两个变量的交互作用，还有什么别的变量的影响，这都很正常

##5

formula_lm <- as.formula(paste0("pct_leave~", paste(unlist(names(DT)[5:119]), collapse = "+")))
lm5 <- lm(formula = formula_lm,data = DT)
summary(lm5)



#Q2
DT2 <- read.dta13("prod.dta")
#a
lms <- lm(log(DT2$va)~log(k)+log(l),data = DT2)
summary(lms)

##log(k)和log(l)系数之和大于1，所以规模报酬递增

DT2$sic3dig <- as.factor(DT2$sic3dig)
table(DT2$sic3dig)

#b

#b加入sic3dig分类更有利于生产函数的拟合，因为不同类型的工厂生产要素不同，va l k等变量在不同sic3dig存在明显区别。



#c
v1 <- which(DT2$sic3dig=="322")
DT2s <- DT2[v1,]
lms2 <- lm(log(va)~log(k)+log(l),data = DT2s)
summary(lms2)

#(Intercept)  6.30035    0.45990  13.699  < 2e-16 ***
#log(k)      -0.29339    0.06756  -4.343 3.87e-05 ***
#log(l)       1.51519    0.10551  14.361  < 2e-16 ***

v2<- which(DT2$sic3dig=="321")
DT3s <- DT2[v2,]
lms3 <- lm(log(va)~log(k)+log(l),data = DT3s)
summary(lms3)

#(Intercept)  3.66391    0.38370   9.549 4.18e-15 ***
#log(k)       0.23066    0.07682   3.002  0.00352 ** 
#log(l)       0.99249    0.13412   7.400 8.98e-11 ***

#c


##从log(k)和log(l)系数和估计来看，381工厂和323工厂存在非常显著的不同

#d

DT4S <- subset(DT2, sic3dig %in% c("321","322"))
DT4S$sic3dig <- factor(DT4S$sic3dig)

lms4 <- lm(log(va)~log(k)+log(l) + sic3dig, data = DT4S)
summary(lms4)


#e

#固定效应
library(plm)
lms5 <- plm(log(va)~log(k)+log(l) , data = DT4S, index = c("sic3dig"))
summary(lms5)


#Q3

DT3 <- read.dta13("dataset4.dta")

lms <- lm(log(quantity)~log(price)+ice+seas1+seas2+seas3+seas4+seas5+seas6+seas7+seas8+seas9+seas10+seas11+seas12,data = DT3)
summary(lms)

##a
#从回归系数检验来看，quantity变量主要由price和ice变量影响，与月份变量无显著关系
DT3$cartel <- factor(DT3$cartel)
#b
lms_b <- lm(log(quantity)~log(price)+ice+cartel,data = DT3)
summary(lms_b)

library(car)
lms_b_2 <- lm(log(quantity)~log(price)+ice,data = DT3)

Anova(lms_b, lms_b_2) #这个是比较加入cartel和不加入cartel的方差检验。
#
confint(lms_b) #这个是返回各个系数对应的置信区间


#我也不知道如何表达，给你们画个图。
library(ggplot2)
ggplot(data = DT3, mapping = aes(x = log(price), y = log(quantity), colour = cartel)) + 
  geom_point() + geom_smooth(se = FALSE)
#有警告，不用管
