k0 = -5 #单位时间等待成本
k1 = -10 #单位时间长途成本
k2 = -13 #单位时间短途成本
k3 = -15 #空车返回成本
R1 = 60 #单位时间长途收益
R2 = 50 #单位时间短途收益
p1 = 0.6 #载长途概率
p12 = 1 - p1 #短途概率
t_bar = 0.2 #在蓄车池等待时间
q1 = 0.5 #载客回概率
q2 = 1 - q1 #空车回概率
M <- 1.5
#t1到t2时间段 第二次

t_all <- rnorm(100, mean = 1.5, sd = 0.5)

all_f2 <- function(t) {
  result <- 0
  expi <- c()
  cat("--start-----------------\n")
  
  while(TRUE) {
    t <- sample(t_all, size = 1, replace = TRUE)
    if (t >= M) {
      cat(paste("长途: ", t * (R1 + k1), "\n"))
      expi <- c(expi, "长途")
      result <- t * (R1 + k1) + result
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
        res_1 <- (t * (R2 + k1 + k3) + k0 * t_bar)
        cat(paste("短途回来没带人:", res_1, "\n"))
      } else {
        #不空车返回
        expi_temp <- "短带人"
        res_1 <- (t * (R2 + k1) * 2 + k0 * t_bar)
        cat("短途回来带人了", res_1, "\n")
      }
    }
    
    result <- result + res_1
    expi <- c(expi, expi_temp)
  }
  cat(paste("总: ", result, "\n"))
  cat("---end------------------\n")
  list("过程" = expi, "收入" = result)
}

all_f2()
y_test <- replicate(10, all_f2())
all_f2()
