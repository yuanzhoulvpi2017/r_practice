`dat` <- read.delim(file.choose(), header=FALSE)#导入数据
#`dat` <- read.delim("~/Rcode/.high_freq.codon(1).pair", header=FALSE)#导入数据
test <- dat[] #复制数据，防止数据修改

######################################################################
#更改颜色，四个颜色依次循环
#发现规律，就是横坐标和纵坐标相加除以4的得到的余数，然后对不同的余数指定相应的颜色
#你仔细观察一下就知道了
#as.numeric（）函数将因子转换为数值，这个数值不是随便的数值而是他的因子对应的序号。
#正常情况下这样做对别的模型或者函数是错误的（不对因子做这样的操作），
#这里用到这个方法有点巧妙
col_my <- as.numeric(test$V1)+as.numeric(test$V2) 
col_my <- col_my %% 4
#下面两个test2，第二个好看一点，
#test2 <- list("0" = "red", "1" = "orange", "2" = "blue", "3" = "black")
test2 <- list("0" = "#E69F00", "1" = "#D55E00","2" =  "#009E73", "3" = "#0072B2")
col_result <- c()
for (i in col_my) {
  col_result <- c(col_result, test2[[as.character(i)]])
}

###########################################################
plot(as.numeric(test$V1), as.numeric(test$V2), 
     type = "n",#这句话是不让散点出现，不然图上会有点，很丑
     xaxt = "n", yaxt = "n",#这个是为了让坐标轴上的数值刻度不要出现，为我们下面添加文本刻度
     xlab = "x", ylab = "y") #这个是设置x的坐标轴的label为x，y坐标轴为y，你可以自己改
axis(1, at = seq(1, 20), labels = levels(test$V1)) #这句话意思是在横坐标上设置文本刻度
axis(2, at = seq(1, 20), labels = levels(test$V2), las = 2) #这句话是在纵坐标上设置刻度 
#las参数是为了让文本旋转一下
text(as.numeric(test$V1), as.numeric(test$V2), test$V3, 
     col = col_result,#这个是添加颜色
     cex = 0.5, srt = -45) #这个设置散点上的标签字体大小，和旋转一下
#cex是设置字体大小，默认为1，srt参数是旋转一下，-45代表顺时针45度。

