#数据输入
load("mi.Rda")
write.csv(BRCA_AS_b,"te.csv")

#调入函数包
library(lattice)
library(MASS)
library(nnet)
library(mice) 

class(BRCA_AS_b)

colnames(BRCA_AS_b)

#md.pattern(BRCA_AS_b)
imp<-mice(BRCA_AS_b,m=2,seed = 123) #m重插补
result<-complete(imp,action=1)#选择插补数据集

#结果
write.csv(result,"result.csv")