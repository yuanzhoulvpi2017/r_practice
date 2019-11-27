library(readxl)

original_data <- read_xlsx("共现矩阵1.xlsx")

which(!complete.cases(original_data)) #没有缺失值

data <- na.omit(original_data)
data <- data[, -1]

library(factoextra)
library(cluster)

gap_stat <- clusGap(data, FUN = kmeans, 
                    nstart = 25, K.max = 10,
                    B = 200)
fviz_gap_stat(gap_stat)

#进行聚类分析
set.seed(2019)
km.res <- kmeans(data, 2, nstart = 25)

fviz_cluster(km.res,data)

km_ecluster <- eclust(data, "kmeans", K.max = 15)




library(tidyverse)
library(readtext)
data <- readtext("~/Rcode/yssj.xlsx")

library(stringi)
library(stringr)
