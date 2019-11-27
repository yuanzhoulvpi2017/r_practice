k0 = -5 #单位时间等待成本
k1 = -10 #单位时间长途成本
k2 = -13#单位时间短途成本
k3 = -15 #空车返回成本
R1 = 60 #单位时间长途收益
R2 = 50 #单位时间短途收益
p1 = 0.6 #载长途概率
#p12 = 1 - p1 #短途概率
t_bar = 0.2 #在蓄车池等待时间
q2 = 0.5 #载客回概率
#q2 = 1 - q1 #空车回概率
M <- 1.5
#t1到t2时间段 第二次

t_all <- round(abs(rnorm(100, mean = 1.5, sd = 0.5)), 3)

all_f2 <- function(t) {
  result <- 0
  expi <- c()
  cat("#-------start-----------------------#\n")
  lun <- 1
  deriver_time <- 0 #司机完整一个时间
  while(TRUE) {
    t <- sample(t_all, size = 1, replace = TRUE)
    if (t >= M) {
      
      expi <- c(expi, "长途")
      res_long <- round(t * (R1 + k1), 4)
      result <- res_long + result
      deriver_time <- deriver_time + t
      cat(paste("长途: ", res_long,"       时间：", 2*t, "\n"))
      break
    } else {
      if (sample(
        c(1, 2),
        size = 1,
        replace = TRUE,
        prob = c(q2, 1 - q2)
      ) == 1) {
        #空车返回
        expi_temp <- "短没带人"
        res_1 <- round(t * (R2 + k1 + k3) + k0 * t_bar, 4)
        t_ <- t * 2 + t_bar
        cat(paste("短途回来没带人:", res_1,"时间：", t_ - t_bar, "\n"))
      } else {
        #不空车返回
        expi_temp <- "短带人"
        res_1 <- round(t * (R2 + k1) * 2 + k0 * t_bar, 4)
        t_ <- t * 2 + t_bar
        cat("短途回来带人了:", res_1,"时间：", t_-t_bar, "\n")
      }
    }
    
    result <- result + res_1
    deriver_time <- deriver_time + t_
    expi <- c(expi, expi_temp)
    lun <- lun + 1
  }
  if (lun != 1) {
    result <- result - res_long
  } 
  
  cat(paste("总收入: ", result, "    总时间：", deriver_time, "\n"))
  cat("#-------end-------------------------#\n\n")
  list("过程" = expi, "收入" = result)
}




all_f2()
replicate(10, all_f2())
#(y_test <- replicate(10, all_f2()))
#all_f2()

#y_test[seq(from = 1, by = 2, length.out = 10)]
#unlist(lapply(seq(from = 1, by = 2, length.out = 10), function(i) {length(y_test[[i]])}))

df_result <- function(np) {
  result_1 <- replicate(np, all_f2())
  #长途为1， 短途为2
  class_1 <- unlist(lapply(seq(from = 1, by = 2, 
                               length.out = np), 
                           function(i) {length(result_1[[i]])}))
  data.frame(long_short = ifelse(class_1 == 1, 1, 2),
             huoli = unlist(lapply(seq(from = 2, by = 2, 
                                       length.out = np), 
                                   function(i) {result_1[[i]]})))
}



#模拟n次，长短比较 # 画出density图
denplot <- function(np) {
  test_reslut <- df_result(100)
  long_result <- subset(test_reslut, long_short == 1)$huoli
  short_result <- subset(test_reslut, long_short == 2)$huoli
  dl <- density(long_result)
  ds <- density(short_result)
  y_max <- max(c(dl$y, ds$y))
  x_min <- min(c(dl$x, ds$x))
  x_max <- max(c(dl$x, ds$x))
  
  plot(dl, xlim = c(x_min, x_max), 
       y_lim = c(0, y_max), lty = 1, col = "red",
       xlab = "profit",
       main = "density long vs density short")
  lines(ds, lty = 3,col = "blue")
  legend("topright", inset=.05,
         c("long", "short"), 
         lty = c(1, 3), col = c("red", "blue"))
  
}
denplot(1000)

#------------------------------
#画出判断长短的阀值与长短收益比的图#模拟1000人
summary(t_all)

M1 <- seq(from = 1.539 - 0.55, to = 1.539 + 0.5, length.out = 20)

ratio_long_short <- lapply(M1, function(M) {
  ratio_result <- df_result(1000)
  long_result <- subset(ratio_result, long_short == 1)$huoli
  short_result <- subset(ratio_result, long_short == 2)$huoli
  mean(long_result) / mean(short_result)
})

plot(M1, ratio_long_short, lty = 1)


replicate(100, {ratio_long_short <- lapply(M1, function(M) {
  ratio_result <- df_result(1000)
  long_result <- subset(ratio_result, long_short == 1)$huoli
  short_result <- subset(ratio_result, long_short == 2)$huoli
  mean(long_result) / mean(short_result)
})
points(M1, ratio_long_short)})



#-------------------
#改变均值
#改变阀值

mean1 <- seq(from = 1.5, to = 2.0, length.out = 20)
M1 <- seq(from = 1.539 - 0.55, to = 1.539 + 0.5, length.out = 20)

result_mm <- outer(X = mean1, Y = M1, FUN = Vectorize(function(X, M) {
  t_all <- round(abs(rnorm(100, mean = X, sd = 0.5)), 3)
  ratio_result <- df_result(1000)
  long_result <- subset(ratio_result, long_short == 1)$huoli
  short_result <- subset(ratio_result, long_short == 2)$huoli
  return(unlist(mean(long_result) / mean(short_result)))
}))


library(plot3D)
persp3D(mean1, M1, result_mm, inttype = 2, 
        shade = 0.2, xlab = "mean", ylab = "M", zlab = "ratio_long_short",
        phi = 20, theta = 40, contour = TRUE)

        