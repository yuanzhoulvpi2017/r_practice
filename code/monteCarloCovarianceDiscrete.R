#####################################
# Monte Carlo Covariance estimation #
#####################################

#########################################################
# Random variables under consideration: A pair of       #
# discrete random variables, whose joint mass function  #
# is encoded in a data frame called 'possibilities'     #
# Rows in the data frame are of form (X,Y,p), where     #
# (X,Y) is a possible outcome of the variables with     #
# probability 'p' of occuring.                          #
#########################################################



#Construct an empty data frame for the values.
#Provide the number of outcomes of the joint distribution

outcomes=8 #<---- Adjust this for the number of outcomes in your mass function
X=rep(0,outcomes)
Y=rep(0,outcomes)
p=rep(0,outcomes)
possibilities=data.frame(X,Y,p)


#The next instructions provide the parameters of the joint distribution
#Change these for your application:

# Add or delete rows as needed.

# Each instruction describes a specific outcome of (X,Y).
# For example the first line says P(X=0,Y=-3)=1/20.
possibilities[1,]=c(-1,-1,3/20)
possibilities[2,]=c(-1,3,2/20)
possibilities[3,]=c(-1,7,1/20)
possibilities[4,]=c(-1,10,1/20)

possibilities[5,]=c(3,-1,1/20)
possibilities[6,]=c(3,3,2/20)
possibilities[7,]=c(3,7,4/20)
possibilities[8,]=c(3,10,6/20)


#Select the sample size
samplesize=1000

##############################
#  Sample Point generator    #
##############################
sampledata=data.frame(xval=rep(0,samplesize),yval=rep(0,samplesize))
for (j in seq(from=1,to=samplesize,by=1))
{
  samplepoint = possibilities[sample(c(1:9),1,T,as.numeric(possibilities[,3])),1:2]
sampledata[j,]=samplepoint
}

#Computes and displays the covariance observed between the values of X and Y
cat("Observed covariance of X and Y in a simulation with ",samplesize,  "data points = ",cov(sampledata$xval,sampledata$yval),"\n")
