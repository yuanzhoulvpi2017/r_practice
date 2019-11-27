


###################################################################
#计算关联度
#定义成函数-------------------------------------------
hsyc <- function(y, ro) {
  #这个函数是计算关联度和关联度系数
  #初始化
  (x <- y / y[ ,1])
  #求绝对差序列
  x0 <- x[1, ]
  theta <- t(abs(apply(as.matrix(x[-1, ]), 1, function(t) {t - x0})))
  #相关系数
  nieta <- (apply(theta, 1, min) + ro * rep(max(theta), each = nrow(y)-1)) / 
    (theta + ro * rep(max(theta), each = nrow(y)-1))
  #关联度
  xgd <- apply(nieta, 1, mean)
  
  #传到list
  result <- list("xgxs" = nieta, 'xgd' = xgd)
  print(result)
  
}

###############################################################################
#灰色预测模型
gmm11 <- function(x) {
  x0 <- x
  x1 <- cumsum(x)
  
  (b <- matrix(1, ncol = 2, nrow = length(x1)-1))
  
  for (i in seq_along(x1)-1) {
    b[i, 1] <- -(x1[i] + x1[i+1])/2
  }
  b
  (y <- x0[-1])
  
  (b_t_b <- t(b) %*% b)
  (b_t_b_1 <- solve(b_t_b))
  (b_t_y <- t(b) %*% matrix(y))
  (alpha_j <- b_t_b_1 %*% b_t_y)
  #得出预测模型
  (a <- alpha_j[1])
  (nu <- alpha_j[2])
  
  #第五步 残差检验
  #1 计算
  (x_j_1 <- (x0[1] - nu / a) * exp(- a * c(0:(length(x0)-1))) + nu / a)
  #---
  #打印公式
  cat("公式为:\n", "x(k+1) =", x0[1] - alpha_j[2] / alpha_j[1], "* exp(", alpha_j[1] , "* k)", alpha_j[2]/alpha_j[1], "\n")
  #---
  #2 累减
  lj <- function(x) {
    out <- array(dim = length(x))
    x_temp <- c(0, x)
    for(i in seq_along(x)) {
      out[i] <- x_temp[i+1] - x_temp[i]
    }
    as.numeric(out)
  }
  (x_j_0 <- lj(x_j_1))
  
  #3 计算绝对误差序列和相对误差序列
  (theta <- round(abs(x_j_0 - x0), 6))#保留小数点后6位
  (big_theta <- round(theta / x_j_0, 8))
  
  #第六步 进行关联度检验
  (nitheta <- (min(theta) + 0.5 * max(theta)) / (theta + 0.5 *max(theta)))
  # 2 关联度
  (r <- mean(nitheta))
  
  # 第七步 后验差检验
  # 1原始序列标准差
  (s1 <- sd(x0))
  # 2残差标准差
  (s2 <- sd(theta))
  # 3 计算C
  (c <- s2 / s1)
  # 4 计算小误差概率
  #s0没计算出来
  (ei <- abs(theta - mean(theta)))
  #第八步 预测值
  
  x_next <- (x0[1] - nu / a) * (exp(- a * (length(x0)+1)) - exp(- a * length(x0)))
  list(a=a, 
       mu=nu, 
       jdwc=theta,# 绝对误差
       glxs = nitheta, #关联系数
       r=r, #关联度
       c = round(c, 6), #
       ei = ei, #小误差概率
       x_next = x_next #预测值
  )
}


#-------------------------------------------------------
#测试函数
x1 <- c(26.7, 31.5, 32.8, 34.1, 35.8, 37.5)
(a <- gmm11(x1)$x_next)
(b <- gmm11(c(x1, a))$x_next)



gmm11_n <- function(x, n) {
  #多期的
  x_n <- c()
  l_t <- 1
  x_inthis <- x
  while (l_t <= n) {
    xt_ <- gmm11(x_inthis)$x_next
    x_n <- c(x_n, xt_)
    x_inthis <- c(x_inthis, xt_)
    l_t <- l_t + 1
  }
  return(x_n)
}


x1 <- c(26.7, 31.5, 32.8, 34.1, 35.8, 37.5)
(a <- gmm11(x1)$x_next)
(b <- gmm11(c(x1, a))$x_next)
gmm11_n(x1, 5)


#读入数据

