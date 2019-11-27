data <- load("test.Rda")


library(tidyverse)

ndata <- as_tibble(test)
small_data <- ndata



for (i in 1:ncol(small_data)) {
  ave <- mean(small_data[, i, drop = TRUE], na.rm = TRUE) 
  small_data[is.na(small_data[, i]), i] <- ave
}


#parallel
my_f <- function(i) {
  ave <- mean(small_data[, i, drop = TRUE], na.rm = TRUE) 
  small_data[is.na(small_data[, i]), i] <<- ave
}
my_f(5)

lapply(X = 1:ncol(small_data), FUN = my_f)

