#install.packages("vioplot")

library(vioplot)                                                   
#setwd("C:\\Users\\lexb4\\Desktop\\tcgaTMB\\23.vioplot")         
lv1 = 37
lv2 = 41
lv3 = 39
lv4 = 38

rt=read.table("CIBERSORT.filter.txt",sep="\t",header=T,row.names=1,check.names=F)   
width = 120

p_col <- 1:ncol(rt)
#pdf("vioplot.pdf",height=8,width=15)             
par(las=1,mar=c(10,6,3,3))
x=c(p_col)
y=c(p_col)
plot(x,y,
     xlim=c(0,width),ylim=c(min(rt)-0.01,max(rt)+0.05),
     main="",xlab="", ylab="Fraction",
     pch=21,
     col="white",
     xaxt="n")



for(i in p_col){
  if(sd(rt[1:lv1,i])==0){
    rt[1,i]=0.001
  }
  if(sd(rt[(lv1+1):(lv1+lv2),i])==0){
    rt[(lv1+1),i]=0.001
  }
  if(sd(rt[(lv1+lv2+1):(lv1+lv2 + lv3),i])==0){
    rt[(lv1+lv2+1),i]=0.001
  }
  if(sd(rt[(lv1+lv2+lv3+1):(lv1+lv2 +lv3 + lv4),i])==0){
    rt[(lv1+lv2+1),i]=0.001
  }
  lv1Data=rt[1:lv1,i]
  lv2Data=rt[(lv1+1):(lv1+lv2),i]
  lv3Data = rt[(lv1+lv2+1):(lv1+lv2 +lv3),i]
  lv4Data = rt[(lv1+lv2+lv3+1):(lv1+lv2 +lv3 +lv4),i]
  
  vioplot(lv1Data,at=5*(i-1),lty=1,add = T,col = 'green')
  vioplot(lv2Data,at=5*(i-1)+1,lty=1,add = T,col = 'red')
  vioplot(lv3Data,at=5*(i-1)+2,lty=1,add = T,col = 'yellow')
  vioplot(lv4Data,at=5*(i-1)+3,lty=1,add = T,col = 'blue')
  #wilcoxTest=wilcox.test(lv1Data,lv2, lv3)
  aov_data <- data.frame(leve = c(rep("lv1", length(lv1Data)),
                                  rep("lv2", length(lv2Data)),
                                  rep("lv3", length(lv3Data)),
                                  rep("lv4", length(lv4Data))),
                         value = c(lv1Data, lv2Data, lv3Data, lv4Data))
  
  res <- aov(value ~ leve, data = aov_data)
  res2 <- summary(res)
  p <- round(res2[[1]]$`Pr(>F)`[1], 3)
  #p=round(wilcoxTest$p.value,3)
  mx=max(c(lv1Data,lv2Data, lv3Data, lv4Data))
  lines(c(x=5*(i-1)+0.2,x=5*(i-1)+0.8),c(mx,mx))
  text(x=5*(i-1)+0.5, y=mx+0.02, labels=ifelse(p<0.001, paste0("p<0.001"), paste0("p=",p)), cex = 0.8)
  text(c(1 + seq(1,width,5))[1:22],-0.05,xpd = NA,labels=colnames(rt),cex = 1,srt = 45,pos=2)
}

#dev.off()

