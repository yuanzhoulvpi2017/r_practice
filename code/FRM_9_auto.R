# Importing a csv file ----
setwd("C:/Users/Shubin Yu/Desktop/RFM")
dd=read.csv("Auto.csv", sep=",", dec=".", stringsAsFactors = F,na.strings=c("NA",""," ","  ","?") )

# Take a look of the data----
sapply(dd, class)
colSums(is.na(dd))
windows()
pairs(mpg~cylinders+displacement+weight+year+acceleration+horsepower,data=dd)
library(psych)
dd$horsepower=replace(dd$horsepower,is.na(dd$horsepower),mean(dd$horsepower,na.rm = TRUE))
windows()
cor.plot(cor(dd[,-8:-9]),numbers=T,las=2)

# Feature engineering----
# build a new variable
dd$brand<-NA
dd$brand <- sapply(dd$brand,as.character)

#Split the brand name
brand=strsplit(dd$name," ")

#use sapply function
#dd$brand <- sapply(brand,"[",1)

#use loop
for (i in 1:397){
  dd$brand[i]=brand[[i]][1]
}

#Sort brands in the ascendant order
library(dplyr)
dd=dd %>%
  arrange(brand)

#Find unique and frequency
unique(dd$brand)
factor(dd$brand)
table(dd$brand)

#correct for the typos
dd$brand = sapply(dd$brand,as.character)
dd$brand = ifelse(dd$brand =="chevroelt"|dd$brand=="chevy","chevrolet", dd$brand)
dd$brand = ifelse(dd$brand =="maxda","mazda", dd$brand)
dd$brand = ifelse(dd$brand =="hi",NA, dd$brand)
dd$brand = ifelse(dd$brand == "mercedes-benz","mercedes", dd$brand)
dd$brand = ifelse(dd$brand == "toyouta","toyota", dd$brand)
dd$brand = ifelse(dd$brand == "vokswagen"|dd$brand == "vw","volkswagen", dd$brand)

# store frequency table in a dataframe
table(dd$brand)
aux <- as.data.frame(table(dd$brand))
#arrange frequencies in desc order
aux=aux %>%
  arrange(desc(Freq))

#Get top 9 brands
aux[1:9,]
names(aux)
topbrand <- as.character(aux$Var1[1:9])
dd$brand_f <- ifelse(dd$brand %in% topbrand, dd$brand, "other")
class(dd$brand_f)
dd$brand_f = as.factor(dd$brand_f)


# Splitting the dataset into the Training set and Test set----
# # install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dd$horsepower, SplitRatio = 2/3)
training_set = subset(dd, split == TRUE)
test_set = subset(dd, split == FALSE)

#Fit the linear model----
#Model 1 linear model
names(dd)
modellinear = lm(mpg~cylinders+displacement+horsepower+weight+brand_f, data = training_set)
summary(modellinear)
mean(dd$mpg[dd$brand_f=="amc"],na.rm = TRUE)
#How to interpret: how other cars compare to AMC. Datsun is the best in terms of MPG#
#Model 2 Linear model with less predictors
modellinear2 = lm(mpg~horsepower+weight+brand_f, data = training_set)
summary(modellinear2)

# Validate results ----
MSE1=mean(((test_set$mpg)-predict(modellinear,newdata=test_set))^2)
MSE2=mean(((test_set$mpg)-predict(modellinear2,newdata=test_set))^2)

#Model 3 Regression tree
library(tree)
modeltree = tree(mpg~horsepower+weight, data = training_set)
summary(modeltree)

# Visualize the tree
windows()
plot(modeltree,col="brown")
text(modeltree,col="dark green")

# Prune the tree
aux=cv.tree(modeltree)
modeltree2=prune.tree(modeltree, best=4)
windows()
plot(modeltree2,col="brown")
text(modeltree2,col="dark green")

# Validate the model
MSE3=mean(((test_set$mpg)-predict(modeltree,newdata=test_set))^2)
MSE4=mean(((test_set$mpg)-predict(modeltree2,newdata=test_set))^2)
