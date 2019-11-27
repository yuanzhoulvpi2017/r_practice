##############################################################################
####################  Statistical programming with R   #######################
####################            Math 167R              #######################
####################            Midterm 2 D             #######################
##############################################################################

########## DR. CRISTINA TORTORA
########## DURATION OF EXAMINATION: 1 HOUR 10 MINUTES
########## SAN JOSE STATE UNIVERSITY TEST 2 NOVEMBER 7, 2019

##########LAST NAME
##########FIRST NAME
##########STUDENT NUMBER 


############################   INSTRUCTIONS   ################################
## ANSWER THE QUESTIONS AND SOLVE THE EXERCISES USING R
## THE CODE NEEDS TO RUN WITHOUT ERRORS
## SAVE THIS FILE AS LASTNAME_NAME.R

#1. (10 points) Which of the following is true:
#a) A Q-Q plot can be used to see if a dataset is normally distributed
#b) A Q-Q plot cannot be used to compare two distributions
#c) A Q-Q plot compares the CDF of two distributions
#d) A Q-Q plot is typically used to assess whether the underlying 
#distribution of a sample is well approximated by an assumed distribution.


#TRUE :  d



#2. (10 points) What is the difference between a for and a while loop

#############
#answer:
#any for loop can be tranform to while loop,
#but while loop  can not be transform to for loop

#For loop processes each condition until the last one is processed. 
#While loop is not ending until the condition is met
####################3


#3.(20 points) Write the following function
#input:  a vector x
#output: the vector x without duplicates
# you can use  %%, all, nrow, ncol, and length

my_fun3 <- function(vec_x) {
  return(as.numeric(names(table(vec_x))))
}
#test my code
my_fun3(vec_x = c(2,3,5,8,1,25,5,5,52,5, 5, 5, 2,8))




#4.(25 points) Write the following function
#input: a numerical vector x
#output:a vector y = (x-min(x))/(max(x)-min(x)), you canNOT use the functions min and max
# you can use  %%, nrow, ncol, and length



my_f <- function(v_data) {
  test_x = v_data
  #find max and min
  find_max <- function(x, vector_x) {return(all(vector_x <= x))}
  find_min <- function(x, vector_x) {return(all(vector_x >= x))}
  
  #calculate
  vector_x_max <- test_x[unlist(lapply(test_x, find_max,vector_x = test_x))]
  vector_x_min <- test_x[unlist(lapply(test_x, find_min,vector_x = test_x))]
  return((test_x - vector_x_min) / (vector_x_max - vector_x_min))
}

#test my code
my_f(v_data = c(2,3,5,8,1,25,5,5,52,5,8))





#5.(20 points) Use the qq plot to test if the variable mpg from the dataset 
#mtcars follow a normal distribution or a t distribution with 5 df. 
#Make one plot with the two graphs, make the lines red.
#Use appropriate axe labels and titles 
##Comment your result
data("mtcars")
qqnorm(mtcars$mpg)
qqline(mtcars$mpg, col = "red")

#6. (15 points) Use the package ggplot2 and the dataset mtcars
#to make the following scatter plot:
# x axis qsec, y axis mpg, shaped according to am.  


library(ggplot2)
ggplot(data = mtcars, aes(x = qsec, y = mpg, shape = factor(am))) + geom_point()
