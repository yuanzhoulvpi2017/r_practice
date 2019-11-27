library(readxl)
data <- read_xlsx(file.choose())

yangd1 <- rep(c('A', 'B', 'C', 'D', 'E'), each = 12*3) #样地
zhushu <- rep(c(names(data)[2:11], "00", '02'), time = 15)#株数

y <- numeric()
for (i in c(1:15)) {
  y <- c(y, as.numeric(unlist(data[i, -1])))
}

kanfa <- rep('no_k', time = 180)

red <- c(
  (5-1)*12 + 10,
  (6-1)*12 + 10,
  (7-1)*12 + 10,
  (7-1)*12 + 8,
  (8-1)*12 + 7,
  (8-1)*12 + 9,
  (9-1)*12 + 7,
  (9-1)*12 + 9,
  (10-1)*12 +10,
  (11-1)*12 +5,
  (11-1)*12 +7,
  (11-1)*12 +9,
  (12-1)*12 +5,
  (12-1)*12 +7,
  (12-1)*12 +9,
  (13-1)*12 +4,
  (13-1)*12 +7,
  (13-1)*12 +9,
  (14-1)*12 +4,
  (14-1)*12 +6,
  (14-1)*12 +7,
  (14-1)*12 +10,
  (15-1)*12 +4,
  (15-1)*12 +6,
  (15-1)*12 +7,
  (15-1)*12 +9
  
)
kanfa[red] = "k" #代表红色的部分

my_data <- data.frame(yangd1 = yangd1,
                      zhushu = zhushu,
                      kanfa = kanfa,
                      y = y)
View(my_data)

###############
#开始分析
#方差齐性检验
bartlett.test(y ~ yangd1)
bartlett.test(y ~ zhushu)
bartlett.test(y ~ kanfa)
#上面三个各个结果中的p-value如果大于0.05，说明就是方差齐

#主效应方差分析
install.packages("multcomp")
library(multcomp)
fit <- aov(y ~ yangd1 + zhushu + kanfa)
summary(fit)

#主效应方差分析模型的参数估计
fit$coefficients


#有交互效应的方差分析表
fit2 <- aov(y ~ yangd1 + zhushu + kanfa + yangd1:zhushu + yangd1:kanfa + kanfa:zhushu)
summary(fit2)
