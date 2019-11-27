#第二问
#改写matlab code
#失败

n = 50 #仿真总人数
lambda = 9 #单位时间顾客到达数
mu = 2 #单个服务台单位时间接待顾客数
s = 4 #服务台数
max_num = 100 #队伍的容量注意不包括正在服务的，
#只包括正在等待的

rem_list <- matrix(0, nrow = 7, ncol = n) 
rem_list[1, ] <- rexp(n = n, rate = lambda) #产生指数分布的顾客到达时间间隔
rem_list[1, ] <- cumsum(rem_list[1, ])#第一行顾客到达时间
rem_list[2, ] <- rexp(n = n, mu) #第二行对顾客的服务时间
#第三行用来存放等待时间
#第四行用来存放离开时间
#第五行用来存放到达时整系统的人数（包括正在被服务的）
#第六行存放正在等待中的人数
#第七行用来存放该顾客是否直接离开，1离开，0排队等待

peo <- c() #储存每个顾客到达时在系统中的总人数




#------------------------
#仿真阶段
for (i in 1:n) {
  now <- which(rem_list[4, peo] > rem_list[1, i])
  peo <- peo[now]
  num <- length(now)
  if (num >= max_num +s){
    rem_list[2:7, i] <- c(0, 0, rem_list[1, i], num, num-s, 1)
    next
  } else {
    if (num < s) {
      rem_list[3:6, i] <- c(0, sum(rem_list[1:3, i]), num, 0)
      peo <- c(peo, i)
    } else {
      time <- sort(rem_list[4, peo])
      
      rem_list[3, i] <- time[length(time)-s+1] - rem_list[1, i]
      rem_list[4:6, i] <- c(sum(rem_list[1:3, i]), num, num-s)
      peo <- c(peo, i)
    }
  }
}

#计算在时间范围内系统队长的变化
timechange <- matrix(0, nrow = 4, ncol = 2 *n)
temp1 <- sort(c(rem_list[1, ], rem_list[4, ]), index.return = TRUE)
timechange[1, ] <- temp1$x
timechange[2, 1] <- 1
for (i in 2:(2*n)) {
  if (temp1$ix[i] <= n) {
    timechange[2, i] <- timechange[2, i-1] + 1
  } else {
    timechange[2, i] <- timechange[2, i-1] - 1
  }
  if (timechange[2, i] >= s) {
    timechange[3:4, i] <- c(timechange(2, i)-s, 1)
  }
}

all_peo = 0
all_wait = 0
busy_time = 0

for (i in 1:(2*n-1)){
  block <- timechange[1, i+1] - timechange[1, i]
  all_peo <- all_peo + block*timechange[2, i]
  all_wait = all_wait + block*timechange[3, i]
  busy_time = busy_time + block*timechange[4, i]
}

pbusy <- busy_time / timechange[1, ncol(timechange)]
mean_peo <- all_peo / timechange[1, ncol(timechange)]
mean_wait_time <- sum(rem_list[7, ])
num <- which(rem_list[7, ] == 1)

mean_wait_time
mean_wait_peo