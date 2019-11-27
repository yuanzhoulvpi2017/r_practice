data <- read.csv("wtp_factorial.csv")
library(plyr)

(tg_1 <- ddply(data, 
               c("population", "rank", "type"), #依次选择变量，
               summarise, mean_test = mean(wtp))) 
#这里的mean是求均值，你可以改为别的统计量，sum、sd、median

(tg_2 <- ddply(data, c("type", "rank"), 
              summarise, 
              test_result = sd(wtp), 
              test2 = mean(wtp),
              test3 = median(wtp)))

my_test_f <- function(x) {
  length(x)
}



