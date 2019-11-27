library(readxl)
data <- read_xlsx(file.choose())

#双对数模型
lm1 <- lm(log(data$GNP) ~ log(data$M2))
summary(lm1)

#增长模型（对数-线性模型）
lm2 <- lm(log(data$GNP) ~ data$M2)
summary(lm2)

#线性对数模型
lm3 <- lm(data$GNP ~ log(data$M2))
summary(lm3)

#线性模型
lm4 <- lm(data$GNP ~ data$M2)
summary(lm4)

