#������״̬����,��ɴ�$$$�Ĳ��֣�Ҫ���ܰ��Ǹ�����������$��������������
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

# Q11-3:�����������ƣ����ǵ�ַ�ﶺ�ź���Ĳ��֣����硰Guangzhou""Beijing"�ȣ�Ȼ��ó��Ľ��Ҫ��û�пո�� 
#Return the city names (address after the comma) only
# Hint: search an example of returning the second element in each list item
# Causion!: spaces must be removed 

unlist(lapply(Address, function(x) {unlist(strsplit(x, " "))[2]} ))
#������Ƕ�֮�����ָ���Ծ���strsplit()����������Ϊ���������㣬�Ӹ�lapply�Ϳ��Խ�����function()
#���������ÿһ���ַ������зָ�����Ϊlapply�Ľ�����б�������ʹ��unlist���б�ת��������
# Answer the following questions
# Q12-1: Read the provided file "2018_2_Scores_Students.csv" uisng the full path
scores <- read.csv("./2018_2_Scores_Students.csv", header = TRUE)

# The score of unsubmitted assignment is recorded as NA
# Q12-2: How many students did not submit their results for each assignment?


which(!complete.cases(scores))
#�����complete.cases������Ҳ��֪����ʲô��˼���ҿ����˾�����������ĳһ���Ƿ���ȱʧֵ��
#���Լ����۵ģ�Ȼ��which����������ȱʧֵ���к�
#���ﷵ����û�������ҵ��Ӧ���к�
# Q12-3: Compute the mean of each assignmen

apply(scores[2:6], 2, mean, na.rm = TRUE)
# Q12-4: Count the number of students who received the total score greater than 45
#���Ҳһ��apply��scores�ĵ�2����6�н������ֵ����Ϊ��ȱʧֵ������Ҫ��mean���������na.rm=TRUE
#
which(apply(scores[2:6], 1, sum, na.rm = TRUE) > 45)
#���������һ����apply��scores�ĵ�2����6�н�������ͣ���Ϊ��ȱʧֵ������Ҫ��sum���������na.rm=TRUE
#Ȼ�󷵻��к�
