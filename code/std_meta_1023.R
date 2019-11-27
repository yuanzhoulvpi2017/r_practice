install.packages(c("meta", "metafor", "rmeta"))

library(metafor)
library(meta)
library(rmeta)

data("dat.bcg")
str(dat.bcg)

data("amlodipine")
str(amlodipine)

#calculating :log odds ratio
Y <- with(dat.bcg, log(tpos * cneg/(tneg*cpos)))
V <- with(dat.bcg, 1/tpos + 1/cneg + 1/tneg + 1/cpos)
cbind(Y, V)

#using metafor FOR ES calculation
ES <- escalc(ai = tpos, bi = tneg, ci = cpos, di = cneg,
             data = dat.bcg, measure = "OR")
summary(ES)
cbind(ES$yi, ES$vi)
