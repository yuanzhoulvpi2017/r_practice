data <- read.csv(file.choose(), header = TRUE)
library(ggplot2)


names(data) <- c("old", 'edu', 'work_old', 'add', 
                 'income', 'rate', 'card', 'other', 'weiyue')

data$weiyue <- factor(data$weiyue)
levels(data$weiyue) <- c("违约", "不违约")
head(data$weiyue)
#加水平线
m_w_0 <- mean(subset(data, weiyue=="违约")$income, na.rm = TRUE)
m_w_1 <- mean(subset(data, weiyue=="不违约")$income, na.rm = TRUE)
mean_weiyue <- data.frame(weiyue = factor(c("违约", "不违约")), mean_w_0 = c(m_w_0, m_w_1))


#加垂直线
m_w_0_0 <- mean(subset(data, weiyue=="违约")$old, na.rm = TRUE)
m_w_1_1 <- mean(subset(data, weiyue=="不违约")$old, na.rm = TRUE)
mean_weiyue_old <- data.frame(weiyue = factor(c("违约", "不违约")), mean_w_1 = c(m_w_0_0, m_w_1_1))



ggplot(data = data, aes(x = old, y = income, col = weiyue, shape=weiyue)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  labs(title = "年龄和收入", x = "年龄", y = "收入")+
  geom_hline(data = mean_weiyue, aes(yintercept = mean_w_0)) + 
  geom_vline(data = mean_weiyue_old, aes(xintercept = mean_w_1))+
  facet_grid(.~ weiyue)

