library(readxl)
b1 <- read_xlsx("D:/temp1012/成分及靶点.xlsx", col_names = FALSE)
colnames(b1) <- c("x1", "x2", "x3", "x4") #第三列是靶点，第二列是成分

b2 <- read.csv("D:/temp1012/疾病靶点.csv") #第二列是靶点

banana <- intersect(b1$x3, b2$Description) #求交集


result <- b1[which(b1$x2 %in% banana), ][, c("x2", "x3")]

write.csv(result,"D:/result.csv", col.names = FALSE)



data <- read.table("D:/temp1012/batman-I2019-10-12-96410-1570866892-cluster1-ScoreGT2targets.txt", sep = "\t")
View(data)

x <- as.numeric(names(yc))
y <- yc[1, ]
plot(x, y, type = "b")

y2 <- yc[nrow(yc), ]
plot(x, y2, type = "b")
