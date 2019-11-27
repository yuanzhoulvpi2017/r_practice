
#data = read.csv("~/Rcode/water_heater.csv", header=TRUE)
data = read.csv(file.choose(), header=TRUE)
data$发生时间=strptime(data$发生时间,"%Y%m%d%H%M%S")
data$eventnum=as.numeric(row.names(data))

whdata=data[data$水流量 !=0,]
t1=whdata$发生时间
m=length(t1)                   
Tm=240
t2=c(t1[1],t1[1:(m-1)])
t3=c(t1[2:m],t1[m])
td1=difftime(t1,t2,units = "secs")      
td2=difftime(t1,t3,units = "secs")       
  
headornot=rep(0,m)
endornot=rep(0,m)
if (whdata$水流量[1]!=0 )headornot[1]=1
if (whdata$水流量[m]!=0 )endornot[m]=1
for ( i in 2:length(headornot)){       
if (abs(td1[i])>=Tm){
	headornot[i]=1
	}else{
	headornot[i]=0
}
}
for ( i in 1:(length(endornot)-1)){   
if (abs(td2[i])>=Tm){
	endornot[i]=1
	}else{
	endornot[i]=0
}
}
dividsequence=data.frame(matrix(NA,sum(headornot==1),3))
colnames(dividsequence)=c("事件序号","事件起始编号","事件终止编号")
dividsequence[,1]=c(1:sum(headornot==1))
dividsequence[,2]=whdata$eventnum[which(headornot==1)]
dividsequence[,3]=whdata$eventnum[which(endornot==1)]

table(cut(dividsequence$事件终止编号 - dividsequence$事件起始编号, breaks = 100))

write.csv(file="D:/dividsequence.csv",dividsequence,row.names=F)

as.Date(data$发生时间[10], "%Y%m%d%H%M%S")
diff_time <- difftime(data$发生时间[dividsequence$事件终止编号], data$发生时间[dividsequence$事件起始编号], units = "mins")
hist(as.numeric(diff_time))
table(cut(as.numeric(diff_time), breaks = 24))
plot(table(cut(as.numeric(diff_time), breaks = 24)))


