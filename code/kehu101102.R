(x2 <- runif(30, min = 10, 20))
(x1 <- x2 * 0.5 + rnorm(30, mean = 0, sd = 15))
(x3 <- x2 ** 2 * 0.05 + rnorm(30, mean = 3, sd = 10))
(y <- x1 + x2 + x3 + rnorm(30, mean = 0, sd = 5))
data1 <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

library(psych)
corr.test(data1)

lm_1 <- lm(y ~ ., data = data1)
summary(lm_1)
anova(lm_1)
library(car)
vif(lm_1, digits=3)
kappa(data1)

#--------------------------------------
library(psych)

x2 <- runif(30, min = 11, max = 15)
x1 <- x2 ** (-2) + rnorm(30, mean = 20, sd = 1)
x3 <- x2 ** 2 + rnorm(30, mean = 20, sd = 5)
y <- x1 + x2 + x3 + rnorm(30, mean = 0, sd = 5)
data1 <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

corr.test(data1)
lm_1 <- lm(y ~ ., data = data1)
summary(lm_1)
vif(lm_1)



library(gapminder)
data("gapminder")
library(dplyr)


for (coun in unique(gapminder$country)) {
  test <- filter(gapminder, country == coun)
  
}

pop <- test$pop
pop2 <- which.max(pop[-1] - pop[-length(pop)])
