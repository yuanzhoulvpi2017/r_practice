install.packages(c("ggplot2", "readxl"))
library(ggplot2)
library(readxl)

my_data <- read_xlsx("D:/mydata/数据.xlsx")

my_data <- 


ggplot(data = my_data, aes(PM10)) + stat_ecdf() +
  labs(y = "PM10", x = "land area")


test <- installed.packages()
test[, 1]
paste0(test[, 1], collapse = '","')
install.packages()


