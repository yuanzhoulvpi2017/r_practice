original <- read.delim("~/Rcode/xinjiang_original.txt")  #含有很多信息
object <- read.table("~/Rcode/xinjiang_growtime.txt", header = TRUE,stringsAsFactors = FALSE) #目标日期

object$Station <- as.numeric(object$Station)
object$Year <- as.numeric(object$Year)
object$X1 <- as.numeric(object$X1)
object$X2 <- as.numeric(object$X2)

head(object)

object$Station <- as.numeric(object$Station)
sum(is.na(object$Station))

mode(object[1, 1])
mode(object[1, 2])
object[1, 3] %% 100
object[1, 4] %% 100
as.numeric(object[1, 1])
head(object)

which(is.na(object$Station) == 0)
table(is.na(object$Station))
table(is.na(original$Station))
object[1, ]
object$Station[which(is.na(object$Station))]




library(lubridate)
library(tidyverse)

data1 <- original %>% select(Station, Year, Month, Date, Rain) %>% 
  mutate(daydate = make_datetime(Year, Month, Date)) %>%
  select(Station, daydate, Rain)

head(data1)

big_continue <- function(data_array) {
  #recode
  changdu_re <- numeric()
  total_re <- numeric()
  mask_array <- as.numeric(data_array > 0)
  for (i in seq(1, length(data_array))) {
    for (bg_ct in seq(3, length(data_array))) {
      
      cat("this is 0 ", i,"*", bg_ct, "\n")
      
      if (sum(mask_array[i: (i+bg_ct-1)], na.rm = TRUE) == bg_ct) {
        
        sum_data <- sum(data_array[i: (i+bg_ct-1)], na.rm = TRUE)
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

write.csv(result, file = "my_result2.csv", row.names = FALSE)

