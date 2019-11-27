#Goal: (1) Illustrate how to create a data frame in three different ways
#      (2) Show how to use a data.frame
 
##############################################
# Method 1: Convert a Matrix to a Data Frame #
##############################################

#Step 1: Populate a matrix with values
temps = matrix(0,nrow=5,ncol=5)
temps[,1] = c(102,99,94,91,91)
temps[,2] = c(71,85,67,68,68)
temps[,3] = c(82,83,79,79,79)
temps[,4] = c(112,110,108,104,104)
temps[,5] = c(82,78,71,84,82)

#Step 2: Create a data.frame with named columns
#        Rows: Dates (Aug1, Aug2, Aug3, Aug4, Aug5)
#        Columns: City Name
tdat = data.frame(temps)
rownames(tdat) = c("8.1","8.2","8.3","8.4","8.5")
colnames(tdat) =c("merced","sf","la","vegas","seattle")

print(tdat)

############################################
# Method 2: Create a Data Frame from Rows  #
############################################

merced = c(102,99,94,91,91)
sf = c(71,85,67,68,68)
la = c(82,83,79,79,79)
vegas = c(112,110,108,104,104)
seattle = c(82,78,71,84,82)
tdat = data.frame(merced,sf,la,vegas,seattle)
rownames(tdat) =  c("8.1","8.2","8.3","8.4","8.5") 

print(tdat)

#############################################
# Method 3: Read Data Frame in from a File  #
#############################################

tdat = read.csv(file="temps.csv", header=TRUE, sep=",")

print(tdat)

#######################
# Uses of Data Frames #
#######################

#Access a row and calculate a quantity:
meanMerced = mean(tdat$merced);

cat("Mean Temperature in Merced ", " = ",meanMerced,"\n")

#######################################
# Create a BoxPlot (Paired Histogram) #
#######################################

boxplot(tdat$merced, tdat$la, main = "Temperature in Different Cities", names=c("Merced","LA"), col = c("orange","red"))

#############################
# Create a Time Series Plot #
#############################

plot(tdat$X,tdat$merced,  type='b',main = "Temperature in Different Cities",ylim=c(60,102),col="red")
lines(tdat$X,tdat$la, col="blue", type="b")
legend(8.1,70, legend=c("Merced", "Los Angeles"), col=c("red", "blue"), lty=1:1)

###################################
#my code 
###################################
college <- read.csv("~/Rcode/College(2).csv")
advertising <- read.csv("~/Rcode/Advertising.csv")


#plot tv and sales
#图1
plot(advertising$TV, advertising$Sales)
#图2
plot(advertising$Newspaper, advertising$Sales)

#这个趋势很明显，你自己描述


#第2 问这题是自由发挥的
#图1
plot(college$PhD, college$Personal)

#图2
college$Private <- factor(college$Private)
boxplot(college$PhD ~ college$Private)

#图3
plot(college$Top10perc ~ college$PhD) #这个图很漂亮，我给你用ggplot2画出来
library(ggplot2)
ggplot(data = college, aes(x = PhD, y = Top10perc, col = Private)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Private)
