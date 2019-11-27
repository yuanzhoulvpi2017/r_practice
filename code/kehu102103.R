library(readxl)
#data <- read_xlsx(file.choose())
data <- read_xlsx("~/Rcode/kh1013.xlsx")

#下面这个my_abcde就是你自己设置，你如果希望A2, A3是个类：你要这样设置:A1, A23, A23, bcde这样依次类推。
my_abcde <- c('A', 'A', "A", 'B', "B", "B", 'C', "C", "C", 'D', "D", "D", 'E', "E", "E")
my_abcde2 <- c('A1', 'A23', "A23", 'B12', "B12", "B3", 'C1', "C23", "C23", 'D1', "D23", "D23", 'E12', "E12", "E3")

location_xy <- c(
  # x, y #第一列是x，第二列是y。一定要注意，x对应excel的行，y对应excel的列
  #下面这个x,y ni你可以自己改。每个表都不一样。
  5,10,
  6,10,
  7,10,
  7,8,
  8,7,
  8,9,
  9,7,
  9,9,
  10,10,
  11,5,
  11,7,
  11,9,
  12,5,
  12,7,
  12,9,
  13,4,
  13,7,
  13,9,
  14,4,
  14,6,
  14,7,
  14,10,
  15,4,
  15,6,
  15,7,
  15,9
  
)

################################################################################################
#这个是函数
trans_fun <- function(data, name, location_xy) {
  
  yangd1 <- rep(name, each = (ncol(data) - 1)) #样地
  yaer <- rep(c(names(data)[2:11], "00", '02'), time = nrow(data))#年份
  
  y <- numeric()
  for (i in c(1:nrow(data))) {
    y <- c(y, as.numeric(unlist(data[i, -1])))
  }
  
  kanfa <- rep('no_k', time = nrow(data) * (ncol(data) - 1))
  
  
  (location_x <- location_xy[seq(from = 1, by = 2, length.out = length(location_xy)/2)])
  (location_y <- location_xy[seq(from = 2, by = 2, length.out = length(location_xy)/2)])
  
  red <- (location_x - 1) * (ncol(data) - 1) + location_y
  kanfa[red] = "k" #代表红色的部分
  
  my_data <- data.frame(yangd1 = yangd1,
                        yaer = yaer,
                        kanfa = kanfa,
                        y = y)
  return(my_data)
}
#########################################################################################

#测试代码。
(test_function_data <- trans_fun(data = data, name = my_abcde, location_xy = location_xy))
(test_function_data2 <- trans_fun(data = data, name = my_abcde2, location_xy = location_xy))


















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
