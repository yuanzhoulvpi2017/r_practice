library(ggplot2)

exam <- data.frame(
  "id" = c(1:6),
  "class" = c(2, 1, 2, 1, 1, 2),
  "english" = c(98, 97, 86, 98, 80, 89),
  "science" = c(50, 60, 78, 58, 65, 98)
)

#第一问
#提取出class= 1
(data1 <- subset(exam, class == 1))
summary(data1)
cor(data1$english, data1$science)
hist(data1$science)
qplot(english, science, data = data1)

#第二问
#提取出class= 2
(data2 <- subset(exam, class == 2))
summary(data2)
cor(data2$english, data2$science)
hist(data2$science)
qplot(english, science, data = data2)

#第三问
#提取出english > 80
(data3 <- subset(exam, english > 80))
summary(data3)
cor(data3$english, data3$science)
hist(data3$science)
qplot(english, science, data = data3)
#第四问
#提取出class= 1
(data4 <- subset(exam, class == 1 & english > 80))
   
summary(data4)
cor(data4$english, data4$science)
hist(data4$science)
qplot(english, science, data = data4)

#第五问
library(dplyr)
data5 <- exam %>% group_by(class) %>%
  summarise(mean(english), mean(science))

names(data5) <- c("class", "english_mean", "science_mean")
data5


install.packages(c("ggplot2", "dplyr"))