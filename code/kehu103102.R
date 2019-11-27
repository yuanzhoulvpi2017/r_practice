library(readxl)
data <- read_xlsx("~/Rcode/data.xlsx")

library(survival)
library(plyr)
names(data)[37:45]
median_data <- apply(data[, 37:45], 2, median)
names(median_data)
is.list(median_data)

for (i in names(data)[37:45]) {
  data[which(data[, i] < median_data[i]), i] = 1
  data[which(data[, i] >= median_data[i]), i] = 2
}

Uniox_f <- function(x) {
  fml <- as.formula(paste0('Surv(futime, fustat) ~ `', x, '`'))
  afcox <- coxph(fml, data = data)
  afsum <- summary(afcox)
  HR <- round(afsum$coefficients[, 2], 2)
  Pvalue <- round(afsum$coefficients[, 5], 3)
  CI <- paste0(round(afsum$conf.int[, 3:4], 2), collapse = "-")
  Uniox <- data.frame("characteristics" = x,
                      "Hazard Ratio" = HR,
                      "CI95" = CI,
                      "P value" = Pvalue)
  return(Uniox)
}
Uniox_f(names(data)[40])

var_names <- colnames(data)[21:45]
univar <- lapply(var_names, Uniox_f)
univar <- ldply(univar, data.frame)

x_temp <- paste0('`', as.character(univar$characteristics[univar$P.value < 0.05][1:3]), '`', collapse = "+")
fml <- as.formula(paste0('Surv(futime, fustat) ~ ', x_temp))
Multicox <- coxph(fml, data = data)
summary(Multicox)
