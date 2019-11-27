#Monte Carlo Covariance estimation

#Random variables under consideration:
#X and Y, uniformly distributed under the constraints 
#0< X <7 and 0<Y<X^2+1.

capfunction <- function(x) {x^2+1} #<-- Set this to your boundary curve

#######################################################
# Set the following parameters to describe the region #
#######################################################

minX = 0;
maxX = 7;

minY = 0; 
maxY = 50;

#################################################
# Set the following to customize the simulation #
#################################################


maxSamples = 500; #<-- This determines how many points are generated


###########################
# This plots the boundary #
###########################

x <- seq(minX,maxX,0.1)
plot(x, capfunction(x),type = 'l',
     col='black',main="0 < Y < X^2+1, 0 < X < 7",ylab="Y",xlab="X")



##############################
#  Point generator #
##############################

xVals=rep(0,maxSamples);
yVals=rep(0,maxSamples);


pointsmade=0
while (pointsmade < maxSamples)
{
  Xsample=runif(1,minX,maxX)
  Ysample=runif(1,minY,maxY)
  
  if(capfunction(Xsample)>Ysample){
    pointsmade = pointsmade +1;
    xVals[pointsmade]=Xsample;
    yVals[pointsmade]=Ysample;
    points(xVals[pointsmade],yVals[pointsmade],col='red')
    }
  
}

#############################################################################
# This next line computes and displays the covariance between the generated #
# set of X and Y coordinates                                                #
#############################################################################

cat("Observed covariance of X and Y in a simulation with ",maxSamples,  "data points = ",cov(xVals,yVals),"\n")
