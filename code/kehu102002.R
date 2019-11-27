#一
#1

(v <- seq(from = 10, by = 2, length.out = 9))
(A <- matrix(v, ncol = 3))
(B <- matrix(rbinom(16, 10, 0.8), ncol=4))
solve(B)

#2
1 - pbinom(100, 1000, 0.1)
pexp(5, 0.25) - pexp(2, 0.25)
pnorm(12, mean = 10, sd = 5) - pnorm(-12, mean = 10, sd = 5)

#3
sample(rnorm(100, mean = 0, sd = 1), 10, replace = FALSE)
prop.table(table(sample(c("男","女"), size = 200, prob = c(0.5, 0.5), replace = TRUE)))
table(sample(c("优质品", "合格品", "次品"), prob = c(0.25, 0.7, 0.05), size = 100, replace = TRUE))[["优质品"]]


#二
#2
#1)
library(readxl)
library(dplyr)
data2 <- read_xlsx(file.choose())

result1 <- data2 %>% group_by(Treat) %>%
  summarise("mean" = mean(Survival), 
            "sd" = sd(Survival), 
            "median" = median(Survival), 
            "max_min" = max(Survival) - min(Survival))
par(mfrow = c(1, 4))
pie(result1$mean, main = "mean")
pie(result1$sd, main = "sd")
pie(result1$median, main = "median")
pie(result1$max_min, main = "max_min")

barplot(result1$mean, main = "mean")
barplot(result1$sd, main = "sd")
barplot(result1$median, main = "median")
barplot(result1$max_min, main = "max_min")

par(mfrow = c(1, 1))

#2)
boxplot(data2$Survival ~ data2$Treat)

#3)
#4)

#2
#1)

data2 <- read_xlsx("~/Rcode/runer_data.xlsx")
plot(density(subset(data2, gender == 0)[, "5000m", drop = TRUE]))#男
plot(density(subset(data2, gender == 1)[, "5000m", drop = TRUE]))#女

#2)
data2$bmi <- data2$weight/(data2$height ^ 2)
n_bmi <- subset(data2, gender == 0)[, "bmi", drop = TRUE]
hist(n_bmi, breaks = seq(from = min(n_bmi), to = max(n_bmi), length.out = 8), probability = TRUE)
lines(density(n_bmi))

nv_bmi <- subset(data2, gender == 1)[, "bmi", drop = TRUE]
hist(nv_bmi, breaks = seq(from = min(nv_bmi), to = max(nv_bmi), length.out = 8), probability = TRUE)
lines(density(nv_bmi))

#3)

(nan_mean <- mean(subset(data2, gender == 0)[, "5000m", drop = TRUE]))
(nv_mean <- mean(subset(data2, gender == 1)[, "5000m", drop = TRUE]))
(nan_sd <- sd(subset(data2, gender == 0)[, "5000m", drop = TRUE]))
(nv_sd <- sd(subset(data2, gender == 1)[, "5000m", drop = TRUE]))
(prob_nan <- pnorm(15*60, mean = nan_mean, sd = nan_sd) - 
    pnorm(12*60, mean = nan_mean, sd = nan_sd)) #这个是男的概率 
(prob_nv <- pnorm(15*60, mean = nv_mean, sd = nv_sd) - 
    pnorm(12*60, mean = nv_mean, sd = nv_sd))#这个是女的概率

#4)
data2$new_5000 <- ifelse(data2$`5000m` > 15*60, "fast", ifelse(data2$`5000m` > 14*60, "normal", "slow"))


#三
#1
q1 <- function(x) {
  if (x >= 0 & x< 1) {
    result <- exp(abs(x - 0.5))
  } else {
    if (x>= 1 & x< 3) {
      result <- abs(x-2)
    } else {
      result <- sin(pi * x)
    }
  }
  return(result)
}

q1(1)
q1(0.5)
q1(0)
#3
mult_norm <- function(...) {
  this <- c(...)
  p <- 1
  for (i in seq(1, length(this)/3)) {
    p <- p * pnorm(this[(i-1)*3+1], mean = this[(i-1)*3+2], sd = this[(i-1)*3+3])
  }
  return(p)
}
mult_norm(1, 2, 1, #第一列是数，第二列是均值，第三列是标准差
          3, 4, 5,
          5, 6, 7)


#4
x<- runif(1000, min = 0.0001, max = 1)
f <- function(x) {log(x) / (x^2 - x - 1)}
sum(f(x), na.rm = TRUE) / 1000

#5
x<- runif(1000, min = -1, max = 1)
y <- runif(1000, min = 0, max = 2)
f <- function(x, y) {sqrt(x^2+2*y^2)}
sum(f(x, y)) / 1000000

#6
fac<- function(n){
  if (n <= 0) warning("必须为正整数")
  if (!is.integer(n)) warning("必须为正整数")
  if (n == 1) return(1)
  if (n >0& is.integer(n)) {
    fac(n-1)*n
  }
}

fac(1.4)
fac(1)
fac(4)
fac(3)
fac(-1)
