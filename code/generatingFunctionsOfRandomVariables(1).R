#好像是有错误
#Generate Functions of Random Variables 

#Goal: Sample random variables, take functions of them and show
#      that the results match our calculations.

#########################################
# Part 1: Square a Uniform Distribution #
#########################################

#Determine the Number of Samples we will take.
NumSamples  = 200

#What is the minimum and maximum range of our Uniform Distribution
MinValue = 0
MaxValue = 1

###################################################
# Sample a Uniformly Distributed Random Variables #
###################################################

sample1=runif(NumSamples,MinValue,MaxValue)
sample1Squared = sample1^3;

########################################
# Creating X and Y Ranges for Plotting #
########################################

numXCoords  = 20;
xRangeOrig  = seq(MinValue,MaxValue,length.out=numXCoords);
pdfOriginal = rep(1/10,numXCoords);

xRangeSquared = seq(MinValue^2,MaxValue^2,length.out=numXCoords);
pdfSquared    = 1/3 * seq(from = 0.001, to = 1, length = numXCoords)^(-2/3);


####################################################
# Generate Histograms of our Sample and its Square #
####################################################

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array

#Histogram of the Original Sample
hist(sample1,breaks=xRangeOrig,probability= TRUE)
lines(xRangeOrig, pdfOriginal,col='red')

#Generate a histogram the squares
hist(sample1Squared,breaks=xRangeSquared,probability=TRUE)
lines(xRangeSquared, pdfSquared,col='red')
lines(density(xRangeSquared), col = "blue")

##########################################################
# Part 2: Maximum of Two Different Uniform Distributions #
##########################################################

#Determine the Number of Samples we will take.
NumSamples  = 200

#What is the minimum and maximum range of our Uniform Distribution
MinValue = 0
MaxValue = 1

###################################################
# Sample a Uniformly Distributed Random Variables #
###################################################

sample1=runif(NumSamples,MinValue,MaxValue)
sample2=runif(NumSamples,MinValue,MaxValue)
sample3=runif(NumSamples,MinValue,MaxValue)

#Parallel Minimum (there is also a pmax to take the max)
maxSample = pmax(sample1,sample2, sample3)

########################################
# Creating X and Y Ranges for Plotting #
########################################

numXCoords  = 10;
xRange      = seq(MinValue,MaxValue,length.out=numXCoords);
pdfOriginal = rep(1/10,numXCoords);

pdfMin    = 2*(1-xRange)

####################################################
# Generate Histograms of our Sample and its Square #
####################################################

par(mfrow=c(1,4))    # set the plotting area into a 1*2 array

#Histogram of the Original Sample
hist(sample1,probability= TRUE)
lines(xRange,pdfOriginal,col='red')

hist(sample2,probability= TRUE)
lines(density(sample2), col = "red")
plot(density(sample2), col = "red")
lines(xRange,pdfOriginal,col='red')

hist(sample3,probability= TRUE)
lines(xRange,pdfOriginal,col='red')

hist(maxSample, breaks = 10, probability= TRUE)
lines(density(maxSample), col = "red")

