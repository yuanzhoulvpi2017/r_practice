original <- read.delim("~/Rcode/beijing.txt")  #含有很多信息
object <- read.table("~/Rcode/北京.txt", header = TRUE) #目标日期

original[1, 1]
mode(original[1, 1])

library(lubridate)
library(tidyverse)

data1 <- original %>% select(Station, Year, Month, Date, Rain) %>% 
  mutate(daydate = make_datetime(Year, Month, Date)) %>%
  select(Station, daydate, Rain)


big_continue <- function(data_array) {
  #recode
  changdu_re <- numeric()
  total_re <- numeric()
  mask_array <- as.numeric(data_array > 0)
  for (i in seq(1, length(data_array))) {
    for (bg_ct in seq(3, length(data_array))) {
      
      cat("this is 0 ", i,"*", bg_ct, "\n")
      
      if (sum(mask_array[i: (i+bg_ct-1)], na.rm = TRUE) == bg_ct) {
        
        sum_data <- sum(data_array[i: (i+bg_ct-1)])
        re_bg_ct <- bg_ct
        
        
        cat("this is 1 ",i, "\t", bg_ct, "\t", data_array[i: (i+bg_ct-1)],'\t', sum_data, "\n")
        
        changdu_re <- c(changdu_re, re_bg_ct)
        total_re <- c(total_re, sum_data)
      } else {
        sum_data <- 0
        re_bg_ct <- 0
        changdu_re <- c(changdu_re, re_bg_ct)
        total_re <- c(total_re, sum_data)
      }
      
      
    }
  }
  return(list(changdu_ = changdu_re, total_ = total_re))
}

test2 <- big_continue(c(1, 1, 1, 2, 4, 0, 0, 0, 0, 0))
big_max <- which.max(test2$changdu_)
test2$changdu_[big_max]
test2$total_[big_max]


day_long_re <- numeric()
total_rain <- numeric()

for (row_object in seq(1, nrow(object))) {
  #对数据进行筛选
  data_temp <- filter(data1, Station == object[row_object, 1])
  
  time1 <- make_date(object[row_object, 2], object[row_object, 3] %/% 100, object[row_object, 3] %% 100)
  time2 <- make_date(object[row_object, 2], object[row_object, 4] %/% 100, object[row_object, 4] %% 100)
  
  teamp1 <- filter(data_temp, daydate >= time1 & daydate <= time2) 
  teamp1 <- teamp1
  
  #某年
  test1 <- teamp1$Rain
  test2 <- big_continue(test1)
  
  cat('this is 2 ********* ', object[row_object, 1], '********** ',object[row_object, 2], ' ***********\n')
  
  
  big_max <- which.max(test2$changdu_)
  
  big_day_long <- test2$changdu_[big_max]
  total_ <- test2$total_[big_max]
  
  day_long_re <- c(day_long_re, big_day_long)
  total_rain <- c(total_rain, total_)
}



result <- data.frame("Satation" = object$Station, 
           "Year" = object$Year,
           "num_day" = day_long_re,
           "total_rain" = total_rain)

write.csv(result, file = "my_result.csv", row.names = FALSE)

