#question1 ################################################################
#Monte Carlo Integration 1-Dimension

#Goal: Write a Monte Carlo integrator for a 1D Function
#      (1) Calculus (By Hand)
#      (2) Numerical Integration (R Tools)
#      (3) Monte Carlo Integration

#Function Under Consideration:
#f(x) = sqrt(1/(2*x) + 1)
#Interval: 1 to 3

integrand <- function(x) {sqrt(1/(2*x) + 1)} #<-- Will change based on function

minX = 1;
maxX = 3;


minY = 0;
maxY = 2;

# Set minY and maxY to the min max values of the integrand
#samples = integrand(seq(minX,maxX,0.0001));
#minY = min(samples);
#maxY = max(samples);
#areaUnderMinY = max(0, minY) * (maxX - minX);

#####################
# Plot the Function #
#####################

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
x <- seq(minX,maxX,0.1)
plot(x, integrand(x),type = 'l',
     col='black',main="sqrt(1/2x + 1)",ylab="f(x)")


#############################
# (2) Numerical Integration #
#############################

soln = integrate(integrand, lower = minX, upper = maxX)
numericalIntegral = soln$value;
cat("Numerical Integration", "\t\t= ",numericalIntegral,"\n")

##############################
# (3) Monte Carlo Integrator #
##############################

maxSamples = 700;

xVals=runif(maxSamples,minX,maxX);
yVals=runif(maxSamples,minY,maxY);

runningIntegral = rep(0,maxSamples);

boxArea = (maxX - minX)*(maxY - minY);

numPointsUnderCurve = 0;
for (j in seq(from=1,to=maxSamples,by=1))
{
  if(yVals[j] < integrand(xVals[j])){
    numPointsUnderCurve = numPointsUnderCurve +1;
    points(xVals[j],yVals[j],col='red')
  }
  else{ 
    points(xVals[j],yVals[j],col='blue')
  }
  runningIntegral[j] =  (numPointsUnderCurve/j)*boxArea #+ areaUnderMinY;
}

plot(seq(1,maxSamples,1),abs(runningIntegral-numericalIntegral)/numericalIntegral,
     type = 'l',col='black',
     main="Relative Error in Integral",ylab="Error",
     xlab="Num Samples")

monteCarloIntegral = (numPointsUnderCurve/maxSamples)*boxArea #+ areaUnderMinY;

cat("Monte Carlo Integration", "\t= ",monteCarloIntegral,"\n")


#question2 ################################################################
#Monte Carlo Integration 1-Dimension

#Goal: Write a Monte Carlo integrator for a 1D Function
#      (1) Calculus (By Hand)
#      (2) Numerical Integration (R Tools)
#      (3) Monte Carlo Integration

#Function Under Consideration:
#f(x) = 1 / (x^4 + 25 * x^2)
#Interval: 1 to 5

integrand <- function(x) {1 / (x^4 + 25 * x^2)} #<-- Will change based on function

minX = 1;
maxX = 5;

minY = 0;
maxY = 0.5;

# Set minY and maxY to the min max values of the integrand
#samples = integrand(seq(minX,maxX,0.0001));
#minY = min(samples);
#maxY = max(samples);
#areaUnderMinY = max(0, minY) * (maxX - minX);

#####################
# Plot the Function #
#####################

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
x <- seq(minX,maxX,0.1)
plot(x, integrand(x),type = 'l',
     col='black',main="1/(x^4+25*x^2)",ylab="f(x)")


#############################
# (2) Numerical Integration #
#############################

soln = integrate(integrand, lower = minX, upper = maxX)
numericalIntegral = soln$value;
cat("Numerical Integration", "\t\t= ",numericalIntegral,"\n")

##############################
# (3) Monte Carlo Integrator #
##############################

maxSamples = 700;

xVals=runif(maxSamples,minX,maxX);
yVals=runif(maxSamples,minY,maxY);

runningIntegral = rep(0,maxSamples);

boxArea = (maxX - minX)*(maxY - minY);

numPointsUnderCurve = 0;
for (j in seq(from=1,to=maxSamples,by=1))
{
  if(yVals[j] < integrand(xVals[j])){
    numPointsUnderCurve = numPointsUnderCurve +1;
    points(xVals[j],yVals[j],col='red')
  }
  else{ 
    points(xVals[j],yVals[j],col='blue')
  }
  runningIntegral[j] =  (numPointsUnderCurve/j)*boxArea #+ areaUnderMinY;
}

plot(seq(1,maxSamples,1),abs(runningIntegral-numericalIntegral)/numericalIntegral,
     type = 'l',col='black',
     main="Relative Error in Integral",ylab="Error",
     xlab="Num Samples")

monteCarloIntegral = (numPointsUnderCurve/maxSamples)*boxArea #+ areaUnderMinY;

cat("Monte Carlo Integration", "\t= ",monteCarloIntegral,"\n")





#question3 ################################################################
#Monte Carlo Integration 1-Dimension

#Goal: Write a Monte Carlo integrator for a 1D Function
#      (1) Calculus (By Hand)
#      (2) Numerical Integration (R Tools)
#      (3) Monte Carlo Integration

#Function Under Consideration:
#f(x) = 1 / (pi * (1 + x^2))
#Interval: 1 to 5

integrand <- function(x) {1 / (pi * (1 + x^2))} #<-- Will change based on function

minX = 1;
maxX = 5;

minY = 0;
maxY = 1;

# Set minY and maxY to the min max values of the integrand
#samples = integrand(seq(minX,maxX,0.0001));
#minY = min(samples);
#maxY = max(samples);
#areaUnderMinY = max(0, minY) * (maxX - minX);

#####################
# Plot the Function #
#####################

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
x <- seq(minX,maxX,0.1)
plot(x, integrand(x),type = 'l',
     col='black',main="1/(pi*(1+x^2))",ylab="f(x)")


#############################
# (2) Numerical Integration #
#############################

soln = integrate(integrand, lower = minX, upper = maxX)
numericalIntegral = soln$value;
cat("Numerical Integration", "\t\t= ",numericalIntegral,"\n")

##############################
# (3) Monte Carlo Integrator #
##############################

maxSamples = 700;

xVals=runif(maxSamples,minX,maxX);
yVals=runif(maxSamples,minY,maxY);

runningIntegral = rep(0,maxSamples);

boxArea = (maxX - minX)*(maxY - minY);

numPointsUnderCurve = 0;
for (j in seq(from=1,to=maxSamples,by=1))
{
  if(yVals[j] < integrand(xVals[j])){
    numPointsUnderCurve = numPointsUnderCurve +1;
    points(xVals[j],yVals[j],col='red')
  }
  else{ 
    points(xVals[j],yVals[j],col='blue')
  }
  runningIntegral[j] =  (numPointsUnderCurve/j)*boxArea #+ areaUnderMinY;
}

plot(seq(1,maxSamples,1),abs(runningIntegral-numericalIntegral)/numericalIntegral,
     type = 'l',col='black',
     main="Relative Error in Integral",ylab="Error",
     xlab="Num Samples")

monteCarloIntegral = (numPointsUnderCurve/maxSamples)*boxArea #+ areaUnderMinY;

cat("Monte Carlo Integration", "\t= ",monteCarloIntegral,"\n")
