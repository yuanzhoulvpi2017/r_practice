#Monte Carlo Integration 2D Calculate PI

#Goal: Write a Monte Carlo integrator for a 1D Function
#      (1) Calculus (By Hand)
#      (2) Numerical Integration (R Tools)
#      (3) Monte Carlo Integration

#Function Under Consideration:
fVal <- function(x,y) {x^2+y^2} #<-- Will change based on function

minX =-1;
maxX = 1;

minY = -1;
maxY = 1;

#####################
# Plot the Function #
#####################

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array

x <- seq(-1,1,length=100)
plot(x,sqrt(1 - x^2),type='l',col='black',
     xlim=c(-1,1),ylim=c(-1,1),xlab="x",ylab="y",
     main="Circle of Radius 1")
lines(x,-sqrt(1 - x^2),type='l',col='black')

#######################
# Monte Carlo Sampler #
#######################

maxSamples = 500;

xVals=runif(maxSamples,minX,maxX);
yVals=runif(maxSamples,minY,maxY);

runningIntegral = rep(0,maxSamples);

boxArea = (maxX - minX)*(maxY - minY);

numPointsUnderCurve = 0;
for (j in seq(from=1,to=maxSamples,by=1))
{
  if(fVal(xVals[j],yVals[j])<1 ){
    numPointsUnderCurve = numPointsUnderCurve +1;
    points(xVals[j],yVals[j],col='red')
  }
  else{
    points(xVals[j],yVals[j],col='blue')
  }
  runningIntegral[j] =  (numPointsUnderCurve/j)*boxArea
}

plot(seq(1,maxSamples,1),abs(runningIntegral-pi)/pi,
     type = 'l',col='black',
     main="Relative Error in Pi",ylab="Error",
     xlab="Num Samples")

monteCarloIntegral = (numPointsUnderCurve/maxSamples)*boxArea;

cat("Monte Carlo Integration", "\t= ",monteCarloIntegral,"\n")

