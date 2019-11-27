#rolling regression 

data <- read.csv("~/Rcode/8100.csv")

name_data <- names(data)



library(roll)


x <- as.matrix(data[2:333, c("XVW", "PREM", "delta.slope", "UI", "CGNON", "REALTB")])
result_total = list()

for (i in name_data[3:27]) { #27
  y <- data[2:333, c(i)]
   result_temp <- roll_lm(x, y, 60)
   
   #result_total[i] = list("coefficients" = list(), "r.squared" = list(), "std.error" = list())
   result_total[[i]]$"coefficients" <- result_temp$coefficients
   result_total[[i]]$"r.squared" <- result_temp$r.squared
   result_total[[i]]$"std.error" <- result_temp$std.error
  cat(i, "\n")
}

result_total$Decile.1.Portfolio.Excess.Return$coefficients
result_total$X6month.excess.return$coefficients
