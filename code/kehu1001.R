original <- read.delim("~/Rcode/beijing.txt")  #含有很多信息
object <- read.table("~/Rcode/北京.txt", header = TRUE) #目标日期


library(lubridate)
library(tidyverse)

data1 <- original %>% select(Year, Month, Date, Rain) %>% 
  mutate(daydate = make_datetime(Year, Month, Date)) %>%
  select(daydate, Rain)

#尝试第一年的
time1 <- make_date(1981, object[1, 3] %/% 100, object[1, 3] %% 100)
time2 <- make_date(1981, object[1, 4] %/% 100, object[1, 4] %% 100)

teamp1 <- filter(data1, daydate >= time1 & daydate <= time2) 
teamp1 <- teamp1[!duplicated(teamp1[1]),]
teamp1

test1 <- teamp1$Rain
test1


big_continue <- function(data_array) {
  #recode
  changdu_re <- numeric()
  total_re <- numeric()
  mask_array <- as.numeric(data_array > 0)
  for (i in seq_along(data_array)) {
    for (bg_ct in seq(3, length(data_array))) {
      if (sum(mask_array[i: (i+bg_ct-1)], na.rm = TRUE) == bg_ct) {
        
        sum_data <- sum(data_array[i: (i+bg_ct-1)])
        re_bg_ct <- bg_ct
        re_i <- i
        
        cat(i, "\t", bg_ct, "\t", data_array[i: (i+bg_ct-1)],'\t', sum_data, "\n")
        changdu_re <- c(changdu_re, re_bg_ct)
        total_re <- c(total_re, sum_data)
      } else 0
      
      
    }
  }
  return(list(changdu_ = changdu_re, total_ = total_re))
}

test3 <- c(1.2, 1.3, 1.4, 1.5, 1.6, 0, 0, 0, 0, 1.3, 1.4, 1.6, 1.7, 0, 0, 0)
test4 <- big_continue(test3)
test4

