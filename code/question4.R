k0 = -10 #单位时间等待成本
k1 = -20 #单位时间长途成本
k2 = -20 #单位时间短途成本
k3 = -20 #空车返回成本
R1 = 30 #单位时间长途收益
R2 = 30 #单位时间短途收益
p1 = 0.5 #载长途概率
p12 = 1 - p1 #短途概率
t_bar = 0.2 #在蓄车池等待时间
q1 = 0.5 #载客回概率
q2 = 1 - q1 #空车回概率
#0-t1时刻 第一次
result_long1 <- function(t) {
  t * (R1 + k1)
}
result_short1 <- function(t) {
  t * (R2 + k2) + k0 * t_bar
}
#t1到t2时间段 第二次
all_f <- function(t) {
  if (sample(c(1, 2),
             size = 1,
             replace = TRUE,
             prob = c(p1, 1 - p1)) == 1) {
    #等于1为跑长途-----------------------
    cat("长途\n")
    return(t * (R1 + k1))
    #------------------------------------
  } else {
    #跑短途------------------------------
    if (sample(
      c(1, 2),
      size = 1,
      replace = TRUE,
      prob = c(q2, 1 - q2)
    ) == 1) {
      #1为空车返回
      cat("短途回来没带人\n")
      return(t * (R2 + k1 + k3) + k0 * t_bar + all_f(2.5))
    } else {
      #不空车返回
      cat("短途回来带人了\n")
      return(t * (R2 + k1) * 2 + k0 * t_bar + all_f(2.6))
    }
    #------------------------------------
  }
}

replicate(10, all_f(10))
