# import the data ----
rm(list=ls())
setwd("C:/Users/Shubin Yu/Desktop/RFM")
dd=read.csv("pm10.csv")
sapply(dd,class)
sum(is.na(dd))


# transform the PM to a binary variable
dd$PMD = ifelse(dd$PM<50,"No","Yes")
dd$PMD=as.factor(dd$PMD)
library("dplyr")
dd$PM_lag1=lag(dd$PM,n=1)

# take a look at the data----
windows()
# for mac 
# dev.new()
layout(matrix(c(1:3),nrow=1,ncol=3))
plot(dd$RHMAX[dd$PMD=="No"],dd$TASMAX[dd$PMD=="No"],pch=19,col="blue",xlab="RHMAX",ylab="TASMAX")
points(dd$RHMAX[dd$PMD=="Yes"],dd$TASMAX[dd$PMD=="Yes"],pch=19,col="red")
boxplot(RHMAX~PMD, data=dd, col=c("blue","red"))
boxplot(TASMAX~PMD, data=dd, col=c("blue","red"))

windows()
layout(matrix(c(1:4),nrow=1,ncol=4))
plot(dd$PRCPAVG[dd$PMD=="No"],dd$TASMIN[dd$PMD=="No"],pch=19,col="blue",xlab="PRCPAVG",ylab="TASMIN")
points(dd$PRCPAVG[dd$PMD=="Yes"],dd$TASMIN[dd$PMD=="Yes"],pch=19,col="red")
boxplot(PSLAVG~PMD, data=dd, col=c("blue","red"))
boxplot(RHAVG~PMD, data=dd, col=c("blue","red"))
boxplot(PRCPAVG~PMD, data=dd, col=c("blue","red"))

# feature engineering ----
ddn=dd[,names(dd) %in% c("PM","PMD","PSLAVG","TASMAX","TASMIN","RHAVG","PRCPAVG")]
ddn$CP=dd$sfcWindAVG*lag(dd$sfcWindAVG,n=1)
ddn$PM_lag1=lag(dd$PM,n=1)
ddn$R=ddn$PM_lag1/ddn$CP
ddn$D1=ifelse(dd$RHMAX==100,1,0)
ddn$D2=ifelse(dd$PRCPAVG==0,1,0)
ddn$D3=ifelse(dd$sfcWindMIN==0,1,0)
ddn$D=ddn$D1*ddn$D2*ddn$D3

# remove the first observation because of the laggd variable
ddn=na.omit(ddn)

# estimate one or several competing regression equations
# split data into training and validation set
set.seed(123)
train=sample(nrow(ddn),round(nrow(ddn)*3/4))

# estimate parameters of logstic regression----
eq1=glm(PMD~PSLAVG+TASMAX+TASMIN+RHAVG+D+R,data=ddn,subset=train,family = binomial)
summary(eq1)
#validate results
nn=data.frame(PMD=ddn$PMD[-train],Estimate_Probability=predict(eq1,type="response",newdata=ddn[-train,]))
#visualize estimated probabilities
windows()
hist(nn$Estimate_Probability,breaks = seq(from=0,to=1,by=0.05),col="light blue",main="Frequency distribution of estimated probabilities")
# Make classification
contrasts(ddn$PMD)
nn$Predicted_PMD = ifelse(nn$Estimate_Probability>0.5,"Yes","No")
# Look at the confusion Matrix and the TER
table(nn$PMD,nn$Predicted_PMD)
(6)/nrow(nn) 
(10)/nrow(nn)

# estimate a classification tree----
library(tree)
eq2=tree(PMD~PSLAVG+TASMAX+TASMIN+RHAVG+D+R,data=ddn,subset=train)
summary(eq2)
windows()
plot(eq2,col="brown")
text(eq2,col="dark green")
#validate results
aux=predict(eq2,newdata=ddn[-train,],type="class")
# calculate a confusion matrix
table(ddn$PMD[-train],aux)
# TER
(6)/nrow(ddn[-train,])

# estimate a ramdon forest----
library(randomForest)
eq3=randomForest(PMD~PSLAVG+TASMAX+TASMIN+RHAVG+D+R,data=ddn,subset=train,importance=T)
windows()
varImpPlot(eq3)
# Calculate a confusion matrix
aux = predict(eq3,newdata = ddn[-train,])
table(ddn$PMD[-train],aux)
# TER
(6)/nrow(ddn[-train,])



