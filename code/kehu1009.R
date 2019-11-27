#part 0 
install.packages(c('lme4', 'ggplot2','lattice','sjstats', 'mosaic'))
library(lme4)

politeness= read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")


favstats(~ frequency, data = politeness)
which(!complete.cases(politeness)) 
#------1------
#输出是39行，实际上，在该数据的第39行确实有个缺失值。

#####################################################################################
#这些都是part 2 
#
#------2------
boxplot(frequency ~ attitude * gender, col = c('white', 'lightgray'), data = politeness)

#------3------这里需要介绍
library(ggplot2)
ggplot(data = politeness, aes(x = subject, y = frequency, fill = attitude)) +
  geom_col() #从这个图上可以看出，F3对应的frequency最高 M7对应的frequency最小

#------4------把下面结果写上去
library(lattice)
new_data1 <- politeness[]
new_data1$attitude <- factor(new_data1$attitude)
colors <- c('red', 'blue')
lines <- c(1, 2)
points <- c(16, 17)

key_trans <- list(title = "layout",
                  space = "bottom", columns=2,
                  text = list(levels(new_data1$attitude)),
                  points = list(pch = points, col = colors),
                  lines = list(col = colors, lty = lines),
                  cex.title=1, cex=0.9)
densityplot(~frequency, data = new_data1,
            groups = attitude, 
            main = "inf dis by att",
            xlab = "attitude",
            pch = points, lty = lines, col = colors, lwd = 2, jitter = 0.005, 
            key = key_trans)

#这个图不解释了吧，因为attitude有两种，将各自对应的frequency的核密度图画出来。放到一个图里面
#
##############################################################################################
#part 3  Model Building
politeness.model <- lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness)
summary(politeness.model)
politeness.model
#------5------把下面结果写上去
#问题B 为什么态度是固定效应：
#答：因为我们这个问题是研究态度对声音频率的影响或者叫关系
#是我们主要考虑的变量，所以态度是固定效应变量

#问题C：为什么主题和建模为随机效果（而不是固定的）
#答：因为我们主要考虑的是态度对频率的影响，而主题和场景建模不是我们想要考虑的变量
#但是这个两个变量又和声音频率有较强的相关性，所以我们将他设置为随机效应变量

#问题D 计算类内相关系数
#这个我找了好久才找到
#https://stackoverflow.com/questions/29269434/how-to-get-a-prediction-interval-for-a-lmer-object
#这个网站上有解决方案，根据他的解释：
library(sjstats)
performance::icc(politeness.model)
#通过这个结果看出来距离0非常远，说明模型不错。


#问题E：将性别添加到固定效应
politeness.e <- lmer(frequency ~ gender + (1|subject) + (1|scenario), data=politeness)
summary(politeness.e)
performance::icc(politeness.e)
#这个组内相关系数就0.180。距离0非常近。说明这个变量不行，应该移除它。
####

##############################################################################################
#part  4
#问题F
politeness.null = lmer(frequency ~ gender +(1|subject) + (1|scenario), data=politeness,REML=FALSE)
politeness.model = lmer(frequency ~ attitude +gender + (1|subject) + (1|scenario),data=politeness, REML=FALSE)
anova(politeness.null,politeness.model)
#通过题目给出的提示在word最后面有个链接的16面（word上多打个0）。写出来就p值就显著了。

#这里的chisq是6.7082，p-value为0.009597说明。。。。。。态度和频率干嘛干嘛，，，，
#性别和频率无关之类的话



#问题G 是通过上面的分析，写出一个小总结，
#你照着summary(politeness.model)写出估计值，，，
#你照着anova(politeness.null,politeness.model)写出p-value,,,,还有态度影响频率。性别不影响频率之类的
#得出结论


########################################################################################
#part 5
#它这个意思就是说，不能将scenario 和subject分开考虑，还要考虑他们的一起作用的情况下的模型。
#然后就将代码更新如下
politeness.model = lmer(frequency ~ attitude + gender + (1+attitude|subject) + (1+attitude|scenario), data=politeness, REML=FALSE)
coef(politeness.model)








