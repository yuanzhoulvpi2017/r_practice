curve(dnorm(x, mean = 1.4, sd = 0.1), from = 1, to = 2)
#1.1
pnorm(1.25, mean = 1.4, sd = 0.1)

#1.2和1.1是一样的，因为连续分布函数在一点值的概率为0

#1.3
pnorm(1.45, mean = 1.4, sd = 0.1) - pnorm(1.25, mean = 1.4, sd = 0.1)

#1.4
1- pnorm(1.57, mean = 1.4, sd = 0.1) + pnorm(1.21, mean = 1.4, sd = 0.1)

#1.5
sum(rnorm(100000, mean = 1.4, sd = 0.1) < 1.25) / 100000

#1.6
sum(rnorm(100000, mean = 1.4, sd = 0.1) <= 1.25) / 100000

#1.7
data<- rnorm(100000, mean = 1.4, sd = 0.1)
sum(data>1.25 & data < 1.45) / 100000

#1.8
data<- rnorm(100000, mean = 1.4, sd = 0.1)
(sum(data>1.57) + sum(data < 1.21)) / 100000

#1.9
qnorm(0.78, mean = 1.4, sd = 0.1)


library(Sleuth3)
data(case0202)

diffvec <- case0202$Unaffected - case0202$Affected

#2.1 
sum(diffvec > 0) / 15 #正值差值的概率14 / 15

#2.2 
sum(diffvec > 0)
