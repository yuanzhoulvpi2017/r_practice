
##这个是第一个1小时检查得到的。
#H-distribution 

poissonmean <- 120 #<-- Will change based on the distribution
maxSamples = 500#<-- Will change based on the simulation choices
threshold = 1 #<-- The process will stop generating arrivals after this value

samplestogenerate=floor(10*threshold*max(poissonmean,1))
first_arrive <- rexp(samplestogenerate,poissonmean)


#############################################
# Simulate the arrival intervals, then count #
# number required to pass the threshhold     #
##############################################
outcomes=rep(0,maxSamples)
averages=rep(0,maxSamples)

for (j in seq(from=1,to=maxSamples,by=1)) {
  
  arrivalintervals = rexp(samplestogenerate,poissonmean)
  
  poissontotal=arrivalintervals[1]
  
  if(poissontotal > threshold) 
  {
    outcomes[j]=0
  } else
  {
    while(poissontotal<threshold)
    {
      outcomes[j]=outcomes[j]+1
      poissontotal=poissontotal+arrivalintervals[outcomes[j]+1]
      
    }
  }  
  
  #Compute an average of all outcomes of the experiment at each stage  
  averages[j]=mean(outcomes[1:j])
}


#Display the mean value of the Poisson random variable over all simulations
cat("Observed mean value of the Poisson R.V. in a simulation with",maxSamples,  "data points = ",averages[maxSamples],"\n")
cat("Proportion of the observed values of the Poisson R.V. below the mean in a simulation with",maxSamples,  "data points = ",length(outcomes[outcomes<=poissonmean])/maxSamples,"\n")



########################################################
#这个第一分钟检查得到的
#M-distribution
######################################################################################################################################################################################################

poissonmean <- 120 #<-- Will change based on the distribution
maxSamples = 500#<-- Will change based on the simulation choices
threshold = 1/60 #<-- The process will stop generating arrivals after this value

samplestogenerate=floor(10*threshold*max(poissonmean,1))
first_arrive <- rexp(samplestogenerate,poissonmean)


#############################################
# Simulate the arrival intervals, then count #
# number required to pass the threshhold     #
##############################################
outcomes=rep(0,maxSamples)
averages=rep(0,maxSamples)

for (j in seq(from=1,to=maxSamples,by=1)) {
  
  arrivalintervals = rexp(samplestogenerate,poissonmean)
  
  poissontotal=arrivalintervals[1]
  
  if(poissontotal > threshold) 
  {
    outcomes[j]=0
  } else
  {
    while(poissontotal<threshold)
    {
      outcomes[j]=outcomes[j]+1
      poissontotal=poissontotal+arrivalintervals[outcomes[j]+1]
      
    }
  }  
  
  #Compute an average of all outcomes of the experiment at each stage  
  averages[j]=mean(outcomes[1:j])
}


#Display the mean value of the Poisson random variable over all simulations
cat("Observed mean value of the Poisson R.V. in a simulation with",maxSamples,  "data points = ",averages[maxSamples],"\n")
cat("Proportion of the observed values of the Poisson R.V. below the mean in a simulation with",maxSamples,  "data points = ",length(outcomes[outcomes<=(poissonmean/60)])/maxSamples,"\n")


##############################################3
#U-distrubution
# U < E(U)
##############################################
arrivalintervals = rexp(samplestogenerate,poissonmean)
sum(arrivalintervals < mean(arrivalintervals)) / length(arrivalintervals)
