dat <- read.csv(file = "~/Rcode/ask02.csv", header = TRUE, stringsAsFactors = TRUE)
a <- table(dat$realIP)
length(a) / nrow(dat)
length(duplicated(dat$FULLURL))

names(dat)

library(tidyverse)
new <- dat %>% group_by(realIP) %>%
  summarise(text = paste(unique(key01), collapse = ","))

term <- paste(unlist(new$text), collapse = ",")
term2 <- unique(unlist(str_split(term, ",")))

obj_data <- as.data.frame(matrix(data = NA, nrow = nrow(new), ncol = length(term2)+1))
colnames(obj_data) <- c("realIP", term2)
obj_data$realIP <- new$realIP

for (i in seq(1, nrow(new))) {
  obj_data[i, unique(unlist(str_split(new$text[i], ",")))] = 1
} 

library(recommenderlab)
Recommender(data = obj_data)
