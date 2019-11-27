library(ggplot2)
library(readxl)
library(tidyverse)
#plot1
aa_data <- read_xlsx("aa_new.xlsx", col_names = FALSE)
colnames(aa_data) <- c("country", "physician_density")

aa_data$country <- factor(aa_data$country, 
                          levels = aa_data$country[order(aa_data$physician_density,
                                                         decreasing = TRUE)])

ggplot(data = aa_data, aes(x = country, y = physician_density)) +
  geom_bar(stat = "identity")  +
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(angle = -45))+
  ggtitle(" variation in physician density across OECD countries") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("physician density")
  
#ggsave("var_pyhsician_density.png", width = 10, height = 5)

#plot2

time_trend_data <- read_xlsx("time_trend2.xlsx", )
row.names(time_trend_data) <- time_trend_data[, 1, drop = TRUE]
time_trend_data <- select(time_trend_data, -c("...1"))
value <- as.matrix(time_trend_data)
dim(value) <- ncol(time_trend_data) * nrow(time_trend_data)
country <- rep(rownames(time_trend_data), rep = 34)
year <- rep(colnames(time_trend_data), each = 5)

time_trend_data_new <- data.frame(value = value,
                                  country = factor(country),
                                  year = year)

ggplot(data = time_trend_data_new, aes(x = year, y = value, color = country)) +
  geom_point() +geom_line(aes(group = country))+
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(angle = -45))+
  ggtitle("time trend of physician density in 5 countries") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("physician density")
  scale_y_continuous(breaks = seq(from = 0, to = 5, length.out = 20), 
                     labels = round(seq(from = 0, to = 5, length.out = 20), 2))

ggsave("value_year_country.png", width = 10, height = 5)




#plot
abchina <- read_xlsx("abchina.xlsx")
abchina$location <- factor(abchina$location,
                           levels = abchina$location[order(abchina$physiciandensity, decreasing = TRUE)])

ggplot(data = abchina, aes(x = location, y = physiciandensity)) +
  geom_bar(stat = "identity") + xlab("province") + ylab("physician density") +
  ggtitle("Variation in Physician Density across Provinces in China") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text.x = element_text(angle = -45))

ggsave("variation_in_Pyhsician_density.png", width = 10, height = 5)
