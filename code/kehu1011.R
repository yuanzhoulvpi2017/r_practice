#不能用状态函数,完成带$$$的部分，要是能按那个行数（就是$的行数）解决最好
# Part 4: Dataframe & Strings
# Crate a dataframe named "dfA" with the following elements
# First column (name = Name): ("Kang", "Kim", "Cho", "Ng")
# Second column (name = Age): (30, 40, 25, 50)
# Third column (name = Address): 
# ("An, Guangzhou", "Mn, Beijing", "Qn, Shanghai", "Bn, Shenzhen")
Name <- c("Kang", "Kim", "Cho", "Ng")
Age <- c(30, 40, 25, 50)
Address <- c("An, Guangzhou", "Mn, Beijing", "Qn, Shanghai", "Bn, Shenzhen")
dfA <- data.frame(Name,Age,Address)
dfA
# Answer the following questions based on the created dataframe
# Q11-1: Count the number of characters of each name
nchar(Name)

# Q11-2: Returen the name of the youngest person
Name[c(which.min(Age))]

# Q11-3:反馈城市名称，就是地址里逗号后面的部分，比如“Guangzhou""Beijing"等，然后得出的结果要是没有空格的 
#Return the city names (address after the comma) only
# Hint: search an example of returning the second element in each list item
# Causion!: spaces must be removed 

unlist(lapply(Address, function(x) {unlist(strsplit(x, " "))[2]} ))
#这题就是对之腹产分割，所以就用strsplit()函数，但是为了向量运算，加个lapply就可以将函数function()
#对向量里的每一个字符串进行分隔，因为lapply的结果是列表，所以使用unlist将列表转换成向量
# Answer the following questions
# Q12-1: Read the provided file "2018_2_Scores_Students.csv" uisng the full path
scores <- read.csv("./2018_2_Scores_Students.csv", header = TRUE)

# The score of unsubmitted assignment is recorded as NA
# Q12-2: How many students did not submit their results for each assignment?


which(!complete.cases(scores))
#这里的complete.cases（）我也不知道是什么意思，我看别人就是用来检验某一行是否有缺失值。
#我自己积累的，然后which函数返回有缺失值的行号
#这里返回了没有完成作业对应的行号
# Q12-3: Compute the mean of each assignmen

apply(scores[2:6], 2, mean, na.rm = TRUE)
# Q12-4: Count the number of students who received the total score greater than 45
#这个也一样apply对scores的第2到第6列进行求均值，因为有缺失值，所以要对mean函数里面的na.rm=TRUE
#
which(apply(scores[2:6], 1, sum, na.rm = TRUE) > 45)
#这个和上面一样，apply对scores的第2到第6列进行行求和，因为有缺失值，所以要对sum函数里面的na.rm=TRUE
#然后返回行号

