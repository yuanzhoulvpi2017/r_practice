#第一题1
(A <- matrix(c(1,2,1/5,1/3,1/4,1/6,1/5,
              1/2,1,1/3,1/2,1/5,1/7,1/6,
              5,3,1,3,1/3,1/5,1/4,
              3,2,1/3,1,1/4,1/6,1/5,
              4,5,3,4,1,1/3,1/2,
              6,7,5,6,3,1,1/3,
              5,6,4,5,2,3,1),byrow = TRUE, nrow = 7))
(A <- 1 / A)

#(A <- matrix(c(1, 3, 5, 1/3, 1, 2, 1/5, 1/2, 1), byrow = TRUE, nrow = 3))

(A_dot <- t(apply(A, 1,  function(x) {x / colSums(A)})))
(M <- rowSums(A_dot))
(w <- M / sum(M)) #所求特征向量

(AW <- A %*% w)
(lambda_max <- sum(AW / (nrow(A) * w))) #最大特征根

#一致性检验
(CI <- (lambda_max - nrow(A)) / (nrow(A) - 1))
RI <- 1.32
(CR <- CI / RI)
#如果CR小于0.1，故矩阵具有满意性

