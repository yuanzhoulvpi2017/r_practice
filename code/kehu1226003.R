library(readxl)
data <- read_xlsx("docum.xlsx")

library(stringr)




for (i in colnames(data)[3:10]) {
  data[str_detect(data[, i, drop = TRUE], 
                  "万"), i] <- as.numeric(as.character(str_replace(
                    data[str_detect(data[, i, drop = TRUE], "万"), 
                         i, drop = TRUE], "万", ""))) * 10000
} #清洗完毕
#接下来的data就是干净的了

write.csv(data, "clearn_docum.csv")
