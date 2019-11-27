###########################################################################################
#part 0
#总结一下：这个分析不难
#主要难点是在如何将ask02.csv数据转换成Recommender包可以读懂的数据
#第二个就是不同的method和对应的参数如何调整，使结果优秀






###########################################################################################
#part 1
#读取数据
#dat <- read.csv(file = file.choose(), header = TRUE, stringsAsFactors = TRUE)
dat <- read.csv(file = "~/Rcode/ask02.csv", header = TRUE, stringsAsFactors = TRUE)

############################################################################################
#part2
#对数据进行清洗
#先将原始数据的realip进行分组，每个ip为一组。（因为原始数据里面有一个相同的ip出现多次）
#然后将每个ip的关键词进行归类，统计计算他们出现的次数
#最后得到就是一个目标数据框（也就是表单）obj_data
#而new、term、term2等都是我在处理数据中造成的中间数据

library(tidyverse)
new <- dat %>% group_by(realIP) %>%
  summarise(text = paste(key01, collapse = ","))

term <- paste(unlist(new$text), collapse = ",")
term2 <- unique(unlist(str_split(term, ",")))

obj_data <- as.data.frame(matrix(data = NA, nrow = nrow(new), ncol = length(term2)+1))
colnames(obj_data) <- c("realIP", term2)
obj_data$realIP <- new$realIP

for (i in seq(1, nrow(new))) {
  temp_ <- table(unlist(str_split(new$text[i], ",")))
  obj_data[i, names(temp_)][is.na(obj_data[i, names(temp_)])] <- 0
  obj_data[i, names(temp_)] = temp_ + obj_data[i, names(temp_)]
} 
view(obj_data) #查看obj_data
library(recommenderlab)
#经过上面得到obj_data，再将obj_data转换成Recommender包可以读懂的数据格式
obj_matrix <- data.matrix(obj_data)
m_real <- as(obj_matrix, "realRatingMatrix")
view(m_real) #这个m_real就是Recommender可以读的懂
n_real <- normalize(m_real) #这个是对m_real进行标准化处理
image(n_real)#可视化
image(m_real)#可视化


#########################################################################################3
#part 3
#建立模型
#在下面这个函数里面的method参数，有：
#对于realRatingMatrix有六种方法：IBCF(基于物品的推荐)、
#UBCF（基于用户的推荐）、SVD（矩阵因子化）、PCA（主成分分析）、 
#RANDOM（随机推荐）、POPULAR（基于流行度的推荐）
m.recomm <- Recommender(m_real, method = "IBCF") #这个就是将m_real 数据放入方式。
m.recomm

#########################################################################################
#part 4
#预测
(ml.predict <- predict(m.recomm, m_real[3028:3031], n = 3)) #这个就是预测最后三个用户的关键词，
#在这里n可以更改，更改推荐数量
#[3028:3031]指的是对第3028到3031这4个用户进行预测，实际上你可以自己更改
obj_data$realIP[3028:3031] #这个就是这几个人的readlip

as(ml.predict, "list") #打印出预测结果



#######################################################################################3
#part 5
#评估检验

#recommenderlab 包提供了函数 evaluationScheme()建立评价方案，
#能够使用简单划分、k折交叉验证、自助法进行模型的评价。
#下面采用简单划分的方法（split），即将数据集简单分为训练集和测试集，
#在训练集训练模型，然后在测试集上评价。 evaluationScheme（）的主要参数：
#method，评估方法；train，划分为训练集的数据比例；k运行评估的折数或倍数（split的默认值为1）；
#given表示用来进行模型评价的items的数量。
scheme <- evaluationScheme(m_real, method = "split", train = 0.9,
                           given = 2, goodRating = 4)
algorithms <- list(popular = list(name = "POPULAR", 
                                  param = list(normalize = "Z-score")),
                   ubcf = list(name = "UBCF", 
                               param = list(normalize = "Z-score", 
                                            method = "Cosine", 
                                            nn = 25)),
                   ibcf = list(name = "IBCF",
                               param = list(normalize = "Z-score")))
results <- evaluate(scheme, algorithms, n = c(1, 3, 5, 10, 15, 20))
results

#如何看懂ROC和，这百度上都有，你们书本上也有
plot(results, annotate = 1:3, legend = "topleft") #ROC
plot(results, "prec/rec", annotate = 3)#precision-recall

# 按照评价方案建立推荐模型
model.popular <- Recommender(getData(scheme, "train"), method = "POPULAR")
model.ibcf <- Recommender(getData(scheme, "train"), method = "IBCF")
model.ubcf <- Recommender(getData(scheme, "train"), method = "UBCF")
# 对推荐模型进行预测
predict.popular <- predict(model.popular, getData(scheme, "known"), type = "ratings")
predict.ibcf <- predict(model.ibcf, getData(scheme, "known"), type = "ratings")
predict.ubcf <- predict(model.ubcf, getData(scheme, "known"), type = "ratings")
# 做误差的计算
predict.err <- rbind(calcPredictionAccuracy(predict.popular, getData(scheme, "unknown")), 
                     calcPredictionAccuracy(predict.ubcf, getData(scheme, "unknown")), 
                     calcPredictionAccuracy(predict.ibcf, getData(scheme, "unknown")))

rownames(predict.err) <- c("POPULAR", "UBCF", "IBCF")
predict.err

#########################################################################################
#part 6
#通过上面的predict.err的结果，比较哪个更好，
#比如说我看predict.err的结果反映，POPULAR结果更好也就是model.popular
#那么我重新预测一下：
(ml2.predict <- predict(model.popular, m_real[3028:3031], n = 3))
as(ml.predict, "list") #part 3的结果
as(ml2.predict, "list") #这个新的结果 #结果并不好，还需要继续改进，h哈哈哈
