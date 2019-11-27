my_summary <- function(x) {
  c <- c(mean, sd, median)
  lapply(c, function(f) f(x, na.rm = TRUE))
}
my_summary((2:5))

my_dist <- function(x) {
  s <- apply(x, 1, crossprod)
  s2 <- matrix(s, nrow = nrow(x), ncol = nrow(x))
  s3 <- tcrossprod(s2)
  s4 <- s2 + t(s2) - 2 * s3
  diag(s4) <- 0
  sqrt(s4)
}

my_dist(matrix(1:16, nrow = 4))
x <- matrix(1:16, nrow = 4)
diag(x) <- 0
x
my_dist(x)
dist(x)

data <- read.csv('bbb.csv', header = TRUE, row.names = 1)
data
ds <- dist(data)

op1 <- matrix(NA, nrow = 13, ncol = 13)
for (i in 1:13) {
  op1[1:13, i] <- c(rep(0, i-1), c(matrix(ds)[(-0.5 * i ^2 + 14.5 * i - 13):(-0.5 * i ^ 2 + 13.5 * i), 1]))
}
row.names(op1) <- row.names(data)[1:13]
colnames(op1) <- row.names(data)[1:13]
write.csv(op1,file = "output.csv")
