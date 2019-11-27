p <- read.table("~/Rcode/biostatitics/perished.txt",stringsAsFactors = FALSE, header = TRUE)
s <- read.table("~/Rcode/biostatitics/survived.txt",stringsAsFactors = FALSE, header = TRUE)

#第二题
#检验方差是否相等
#1
var_r <- var(p$perished) == var(s$survived)
t.test(p$perished, s$survived, var.equal = var_r)

#2
boxplot(p$perished, s$survived)


#########################################################################

#第三题

#1
iris_my <- read.csv("~/Rcode/biostatitics/iris_train.csv", header = TRUE)
table(iris_my$Species)
plot_data <- data.frame("setosa" = subset(iris_my, Species == "setosa")$Sepal.Length,
                        "versicolor" = subset(iris_my, Species == "versicolor")$Sepal.Length,
                        "virginica" = subset(iris_my, Species == "virginica")$Sepal.Length)
boxplot(plot_data)

#t.test： 原始假设h0 :mu0 - mu1 = 0    h1: m0 - mu1 != 0
t.test(plot_data$setosa, plot_data$versicolor, 
       var.equal = var(plot_data$setosa) == var(plot_data$versicolor))


#2
shapiro.test(plot_data$setosa) #p-value > 0.05说明是正态的
shapiro.test(plot_data$versicolor)
shapiro.test(plot_data$virginica)

#上面三个都是正态的
t.test(plot_data$setosa, plot_data$versicolor, paired = TRUE)
#paired = TRUE表明是配对检验 p-value<0.05,拒绝原假设，说明有差异


