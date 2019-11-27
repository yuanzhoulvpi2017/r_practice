library(readxl)
jnxs <- read_xlsx("jnxs.xlsx")
t_jnxs <- as.matrix(jnxs[, -1])
dim(t_jnxs) <- c(100, 1)
year <- rep(c(1995:2014), each = 5, time = 20)
prov <- rep(c("Beijing", "Shanghai","Guangdong", "Hubei","Yunnan"), time = 20)

jnxs_df <- data.frame(jnxs = t_jnxs, year = year, prov = factor(prov))
library(ggplot2)
ggplot(data = jnxs_df, aes(x = year, y = jnxs, colour = prov)) + geom_line() +geom_point() +
  labs(x = NULL, y = NULL) + 
  scale_x_continuous(breaks = c(1995:2014), labels = c(1995:2014)) +
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.ticks = element_line(size = 0.7)) +
  ggtitle("Gini Coefficient in 5 Provinces in China") +
  theme(plot.title = element_text(hjust = 0.5))



####################################################
gdp <- read.csv("gdp2.csv", header = FALSE)
location <- rep("prov", time = 32)
location[21] <- as.character(gdp$V1[21])
gdp$location <- factor(location)
l_name <- as.character(gdp$V1[order(gdp$V2)])
gdp$V1 <- ordered(gdp$V1, levels = l_name)
ggplot(data=gdp, aes(x=V1, y=V2)) + 
  geom_bar(aes(fill = location),stat="identity") + scale_fill_manual(values=c("red", "gray")) +
  theme(plot.subtitle = element_text(vjust = 1), 
          plot.caption = element_text(vjust = 1), 
          axis.text.x = element_text(angle = -45))+
  ggtitle("Fixed Assets Investment 2014 Provincial Ranking and Average") +
  theme(plot.title = element_text(hjust = 0.5))



#####################################################################
#fa
fa <- read_xlsx("fa.xlsx")
fa_v <- as.matrix(fa[, -1])
dim(fa_v) <- c(21*5)
prov <- rep(unlist(names(fa)[-1]), each = 21)
year2 <- rep(fa[,1, drop = TRUE], time = 5)
fa_df <- data.frame(fa_v = fa_v, year = year2, prov = factor(prov))


ggplot(data = fa_df, aes(x = year, y = fa_v, colour = prov)) + geom_point() + geom_line() +
  labs(x = NULL, y = NULL) + 
  scale_x_continuous(breaks = c(1994:2014), labels = c(1994:2014)) + 
  ggtitle("Diverge: 1994-2014 Fixed Asset Investment in 5 Provinces") +
  theme(plot.title = element_text(hjust = 0.5))


############################################################
#qgtj

qgtj <- read_xlsx("qgfx.xlsx")
ggplot(data = qgtj, aes(x = year, y = nature)) + geom_line()+ geom_point() +
  labs(x = NULL, y = NULL) + 
  scale_x_continuous(breaks = c(1994:2014), labels = c(1994:2014)) +
  scale_y_continuous(breaks = seq(from = min(qgtj$nature), to = max(qgtj$nature), length.out = 20),
                     labels = round(seq(from = min(qgtj$nature), to = max(qgtj$nature), length.out = 20), 2)) +
  ggtitle("Nationwide Cumulative Amount of Fixed Assets Investment（1994-2014）") +
  theme(plot.title = element_text(hjust = 0.5))


