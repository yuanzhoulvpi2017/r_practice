#在R里面，你将光标移到某一行，然后按住ctrl 在按R键就可以运行
#在Rstudio里面，你按住ctrl 再按enter键就可以运行命令
install.packages(c("readxl", "pheatmap")) 
#readxl包是读取excel表格的。pheatmap包是画热力图的，热力图的函数有很多，但是这个包更加强大一些
#这些包如果之前都没有安装，你要安装，如果安装过了，就不要运行
library(readxl)

data1 <- read_xlsx(file.choose()) #会打开窗口让你选择excel文件，
#注意一定要是xlsx文件，别的文件我没打开过，这个我不清楚，但是读取xlsx文件都用这个函数



mat_data <- matrix(unlist(data1[, c(2:ncol(data1))]))
dim(mat_data) <- c(nrow(data1), ncol(data1)-1)
#因为读取的数据第一列都是名称，数据都是在第2列到最后一列
#读取数值列，然后因为data.frame都是列表，所以使用unlist（）函数将列表转换成数组
#转换的结果得到一维数组，然后将一维数组放到matrix（）函数里面生成矩阵
#将这个矩阵的形状改为和excel的性质一样



colnames(mat_data) <- names(data1)[2:ncol(data1)]
rownames(mat_data) <- c(unlist(data1[, 1, drop = TRUE]))
#将矩阵的行名称和列名称从excel表格里自动复制过来


library(pheatmap)
pheatmap(mat_data,display_numbers = TRUE)
pheatmap(mat_data,cluster_row = FALSE, cluster_cols = FALSE, display_numbers = TRUE)

#https://blog.csdn.net/sinat_38163598/article/details/72770404
#上面的链接有详细说明的这个热力图的参数如何调整以及相应的意义。
#其实，画热力图一点也不难，就是当你不知道如何读取数据如何转换数据
#还要难点就是要你调整细节的时候，就非常麻烦。
#你在百度上搜索    R语言pheatmap      
#然后你就就看到很多文章，你只要该后面参数，数据都不用改


