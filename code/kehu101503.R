library(mvtnorm)

x <- rmvnorm(n=500, mean=rep(0, 3), sigma=diag(c(3, 4, 5)), method="chol")
x1 <- x[, 1]
x2 <- x[, 2]
y <- x[, 3]

lm_1 <- lm(y ~ x1 + x2)
a <- summary(lm_1)
